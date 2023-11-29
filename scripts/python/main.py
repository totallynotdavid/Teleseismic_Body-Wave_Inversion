import os
import subprocess
import requests
import re
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed

# Configuración del registro de eventos (logging)
# Esto es para que los mensajes de registro se muestren en la consola y se guarden en un archivo de registro. Especiamente útil para depurar procesos largos.
log_file = os.path.join(os.getcwd(), "logs/fetch_and_process.log")
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s %(levelname)s:%(message)s',
    handlers=[
        logging.FileHandler(log_file),
        logging.StreamHandler()
    ]
)

# Definición de directorios y endpoints
BASE_DIR = "data/preprocesamiento"
STATIONXML_PATH = os.path.join(os.getcwd(), "dependencies/stationxml-seed-converter-2.1.3.jar")
STATION_URL = "http://service.iris.edu/fdsnws/station/1/query"
DATASELECT_URL = "http://service.iris.edu/fdsnws/dataselect/1/query"

# Información sobre el evento sísmico definida por el usuario
EVENT_LATITUDE = -9.68
EVENT_LONGITUDE = -80.08
EVENT_START_TIME = "1996-02-21T12:46:01"
EVENT_END_TIME = "1996-02-21T13:51:01"
MIN_RADIUS = 30  # en grados
MAX_RADIUS = 90  # en grados
NETWORK_PREFERENCE = "II,IU"

def obtener_estaciones_dentro_del_rango(latitude, longitude, start_time, end_time, min_radius, max_radius, networks):
    """
    Obtener una lista de estaciones dentro de un rango epicentral especificado y redes.
    """
    params = {
        "latitude": latitude,
        "longitude": longitude,
        "starttime": start_time,
        "endtime": end_time,
        "minradius": min_radius,
        "maxradius": max_radius,
        "network": networks,
        "format": "text",
        "level": "station"
    }
    response = requests.get(STATION_URL, params=params)
    response.raise_for_status()
    return response.text

def obtener_estaciones_del_response(response_text):
    """
    Parsear la lista de estaciones del texto de respuesta, comenzando desde la segunda línea para saltar el encabezado.
    """
    lines = response_text.strip().split('\n')
    stations = []
    for line in lines[1:]:
        parts = line.split('|')
        if len(parts) > 3:
            network, station = parts[0], parts[1]
            stations.append((network, station))
    
    logging.info(f"Estaciones encontradas: {len(stations)}. Estaciones: {stations}")
    return stations

def verificar_directorio():
    """
    Crear el directorio de salida si no existe.
    """
    os.makedirs(BASE_DIR, exist_ok=True)

def sanitizar_nombre(nombre):
    """
    Sanitizar el nombre del archivo reemplazando los caracteres no alfanuméricos, excepto 'T', con guiones bajos. Hacemos esto porque esos caracteres no son válidos en los nombres de archivo.
    """
    nombre_saneado = re.sub(r'[^a-zA-Z0-9_T.]', '_', nombre)  # Preservar la 'T' entre fecha y hora
    nombre_saneado = nombre_saneado.replace('T', '_')  # Reemplazar la 'T' con un guión bajo
    return nombre_saneado

def descargar_archivo(url, parametros, ruta_archivo):
    logging.debug(f"Intentando descargar archivo desde {url} con parámetros {parametros}")
    if os.path.exists(ruta_archivo):
        logging.info(f"Archivo ya existe: {ruta_archivo}. Saltando descarga.")
        return True

    try:
        with requests.Session() as sesion:
            respuesta = sesion.get(url, params=parametros)
            respuesta.raise_for_status()
            with open(ruta_archivo, 'wb') as archivo:
                archivo.write(respuesta.content)
        logging.info(f"Descargado: {ruta_archivo}")
        return True
    except requests.exceptions.HTTPError as err_http:
        logging.error(f"Error HTTP: {err_http} - {respuesta.status_code}")
        logging.error(f"Respuesta del servidor: {respuesta.text}")
    except Exception as err:
        logging.error(f"Ocurrió un error: {err}")
    return False

def convertir_formato(comando):
    logging.debug(f"Ejecutando comando: {comando}")
    try:
        resultado = subprocess.run(comando, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        return resultado.returncode == 0
    except subprocess.CalledProcessError as e:
        logging.error(f"Error al ejecutar: {comando}")
        logging.error(e.output.decode())
        return False

def procesar_estacion(network, station, inicio, fin):
    # Descargar metadata en formato .xml
    nombre_metadata = sanitizar_nombre(f"{network}_{station}_{inicio.replace(':', '_').replace('-', '_')}.xml")
    ruta_metadata = os.path.join(BASE_DIR, nombre_metadata)
    ruta_dataless = ruta_metadata.replace('.xml', '.dataless')

    # Descargar datos en formato miniSEED
    archivo_miniseed = f"{network}_{station}.mseed"
    ruta_miniseed = os.path.join(BASE_DIR, archivo_miniseed)

    metadata_downloaded = False
    miniseed_downloaded = False

    # Descargar metadata de la estación
    if descargar_archivo(STATION_URL, {
            "network": network,
            "station": station,
            "starttime": inicio,
            "endtime": fin,
            "level": "response",
            "format": "xml"
        }, ruta_metadata):
        metadata_downloaded = True
        # Convertir metadata a formato dataless
        if not convertir_formato(f"java -jar {STATIONXML_PATH} -i {ruta_metadata} -o {ruta_dataless}"):
            metadata_downloaded = False

    # Descargar datos miniSEED
    for channel in ["BHE", "BHN", "BHZ"]:  # Canales específicos
        archivo_miniseed_canal = archivo_miniseed.replace(".mseed", f"_{channel}.mseed")
        ruta_miniseed_canal = os.path.join(BASE_DIR, archivo_miniseed_canal)

        if descargar_archivo(DATASELECT_URL, {
                "network": network,
                "station": station,
                "starttime": inicio,
                "endtime": fin,
                "format": "mseed",
                "cha": channel
            }, ruta_miniseed_canal):
            miniseed_downloaded = True

    if metadata_downloaded and miniseed_downloaded:
        for channel in ["BHE", "BHN", "BHZ"]:
            ruta_miniseed_canal = os.path.join(BASE_DIR, archivo_miniseed.replace(".mseed", f"_{channel}.mseed"))
            convertir_formato(f"rdseed -f {ruta_miniseed_canal} -R -d -o 1 -p -g {ruta_dataless} -q {BASE_DIR}")

# Programa principal
def main():
    logging.info("Inicio del proceso de descarga y conversión de datos en paralelo.")
    verificar_directorio()

    # Obtener y parsear estaciones
    stations_response = obtener_estaciones_dentro_del_rango(
        EVENT_LATITUDE, EVENT_LONGITUDE, EVENT_START_TIME, EVENT_END_TIME, MIN_RADIUS, MAX_RADIUS, NETWORK_PREFERENCE
    )
    updated_stations = obtener_estaciones_del_response(stations_response)

    with ThreadPoolExecutor(max_workers=None) as ejecutor:
        futuro_a_estacion = {ejecutor.submit(procesar_estacion, red, estacion, EVENT_START_TIME, EVENT_END_TIME): (red, estacion) for red, estacion in updated_stations}

        for futuro in as_completed(futuro_a_estacion):
            red, estacion = futuro_a_estacion[futuro]
            try:
                futuro.result()
                logging.info(f"Procesamiento completo para la estación: {estacion}, red: {red}")
            except Exception as exc:
                logging.error(f"La estación {estacion} generó una excepción: {exc}")

    logging.info("Todas las tareas han sido completadas.")

if __name__ == "__main__":
    main()
