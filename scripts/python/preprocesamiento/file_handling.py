import logging
import os
import re
import subprocess

import requests
from config import BASE_DIR, CHANNELS, DATASELECT_URL, STATION_URL, STATIONXML_PATH


def verificar_directorio():
    """
    Crear el directorio de salida si no existe.
    """
    os.makedirs(BASE_DIR, exist_ok=True)


def sanitizar_nombre(nombre):
    """
    Sanitizar el nombre del archivo reemplazando los caracteres no alfanuméricos, excepto 'T', con guiones bajos. Hacemos esto porque esos caracteres no son válidos en los nombres de archivo.
    """
    nombre_saneado = re.sub(
        r"[^a-zA-Z0-9_T.]", "_", nombre
    )  # Preservar la 'T' entre fecha y hora
    nombre_saneado = nombre_saneado.replace(
        "T", "_"
    )  # Reemplazar la 'T' con un guión bajo
    return nombre_saneado


def descargar_archivo(url, parametros, ruta_archivo):
    logging.debug(
        f"Intentando descargar archivo desde {url} con parámetros {parametros}"
    )
    if os.path.exists(ruta_archivo):
        logging.info(f"Archivo ya existe: {ruta_archivo}. Saltando descarga.")
        return True

    try:
        with requests.Session() as sesion:
            respuesta = sesion.get(url, params=parametros)
            respuesta.raise_for_status()
            with open(ruta_archivo, "wb") as archivo:
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
        resultado = subprocess.run(
            comando, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        return resultado.returncode == 0
    except subprocess.CalledProcessError as e:
        logging.error(f"Error al ejecutar: {comando}")
        logging.error(e.output.decode())
        return False


def procesar_estacion(network, station, inicio, fin):
    # Descargar metadata en formato .xml
    nombre_metadata = sanitizar_nombre(
        f"{network}_{station}_{inicio.replace(':', '_').replace('-', '_')}.xml"
    )
    ruta_metadata = os.path.join(BASE_DIR, nombre_metadata)
    ruta_dataless = ruta_metadata.replace(".xml", ".dataless")

    # Descargar datos en formato miniSEED
    archivo_miniseed = f"{network}_{station}.mseed"
    # ruta_miniseed = os.path.join(BASE_DIR, archivo_miniseed)

    metadata_downloaded = False
    miniseed_downloaded = False

    # Descargar metadata de la estación
    if descargar_archivo(
        STATION_URL,
        {
            "network": network,
            "station": station,
            "starttime": inicio,
            "endtime": fin,
            "level": "response",
            "format": "xml",
        },
        ruta_metadata,
    ):
        metadata_downloaded = True
        # Convertir metadata a formato dataless
        if not convertir_formato(
            f"java -jar {STATIONXML_PATH} -i {ruta_metadata} -o {ruta_dataless}"
        ):
            metadata_downloaded = False

    # Descargar datos miniSEED
    for channel in CHANNELS:
        archivo_miniseed_canal = archivo_miniseed.replace(".mseed", f"_{channel}.mseed")
        ruta_miniseed_canal = os.path.join(BASE_DIR, archivo_miniseed_canal)

        if descargar_archivo(
            DATASELECT_URL,
            {
                "network": network,
                "station": station,
                "starttime": inicio,
                "endtime": fin,
                "format": "mseed",
                "cha": channel,
            },
            ruta_miniseed_canal,
        ):
            miniseed_downloaded = True

    if metadata_downloaded and miniseed_downloaded:
        for channel in CHANNELS:
            ruta_miniseed_canal = os.path.join(
                BASE_DIR, archivo_miniseed.replace(".mseed", f"_{channel}.mseed")
            )
            convertir_formato(
                f"rdseed -f {ruta_miniseed_canal} -R -d -o 1 -p -g {ruta_dataless} -q {BASE_DIR}"
            )
