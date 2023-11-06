import os
import subprocess
import requests
import re
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed

# Configuración del registro de eventos (logging)
logging.basicConfig(level=logging.INFO, format='%(asctime)s %(levelname)s:%(message)s')

# Definición de directorios y endpoints
BASE_DIR = "data/preprocesamiento"
STATIONXML_PATH = os.path.join(os.getcwd(), "dependencies/stationxml-seed-converter-2.1.3.jar")
STATION_URL = "http://service.iris.edu/fdsnws/station/1/query"
DATASELECT_URL = "http://service.iris.edu/fdsnws/dataselect/1/query"

# Estaciones y redes a descargar
ESTACIONES = [
    ("II", "ASCN"),  # Butt Crater, Ascension Island
    ("IU", "CCM"),  # Cathedral Cave, Missouri, USA
    ("IU", "COL"),  # College Outpost, Alaska, USA
    ("IU", "COR"),  # Corvallis, Oregon, USA
    # ("CN", "FFC"),  # Flin Flon, Canada (no hay datos)
    ("IU", "HKT"),  # Hockley, Texas
    ("IU", "HRV"),  # Adam Dziewonski Observatory, Massachusetts, USA
    ("G",  "KOG"),  # Kourou, French Guiana, France
    ("IU", "PAB"),  # San Pablo, Spain
    ("CI", "PFO"),  # Pinon Flat, California, USA
    ("II", "RPN"),  # Rapanui, Easter Island, Chile
    ("IU", "SPA"),  # South Pole, Antarctica
    ("G",  "TAM"),  # Tamanrasset, Algeria
    ("IU", "TUC"),  # Tucson, Arizona
    ("G",  "UNM")   # Unam, Mexico, Mexico
]

# Información sobre el evento sísmico
INFO_EVENTO = {
    # Latitud y longitud no son relevantes
    # "latitude": -9.6915,
    # "longitude": -79.767,
    "inicio": "1996-02-21T12:46:01",  # 5 minutos antes del evento
    "fin": "1996-02-21T13:51:01"     # 60 minutos después del evento
}

def verificar_directorio():
    """
    Crear el directorio de salida si no existe.
    """
    os.makedirs(BASE_DIR, exist_ok=True)

def sanitizar_nombre(nombre):
    """
    Sanitizar el nombre del archivo reemplazando los caracteres no alfanuméricos, excepto 'T', con guiones bajos.
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
        return resultado .returncode == 0
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

    if descargar_archivo(DATASELECT_URL, {
            "network": network,
            "station": station,
            "starttime": inicio,
            "endtime": fin,
            "format": "mseed"
        }, ruta_miniseed):
        miniseed_downloaded = True

    if metadata_downloaded and miniseed_downloaded:
        convertir_formato(f"rdseed -f {ruta_miniseed} -R -d -o 1 -p -g {ruta_dataless} -q {BASE_DIR}")

# Programa principal
def main():
    logging.info("Inicio del proceso de descarga y conversión de datos en paralelo.")
    verificar_directorio()

    inicio = INFO_EVENTO["inicio"]
    fin = INFO_EVENTO["fin"]

    with ThreadPoolExecutor(max_workers=None) as ejecutor:
        futuro_a_estacion = {ejecutor.submit(procesar_estacion, red, estacion, inicio, fin): (red, estacion) for red, estacion in ESTACIONES}

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
