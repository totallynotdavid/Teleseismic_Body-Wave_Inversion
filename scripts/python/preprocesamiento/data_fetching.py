import requests
import logging
from config import STATION_URL


def obtener_estaciones_dentro_del_rango(
    latitude, longitude, start_time, end_time, min_radius, max_radius, networks
):
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
        "level": "station",
    }
    response = requests.get(STATION_URL, params=params)
    response.raise_for_status()
    return response.text


def obtener_estaciones_del_response(response_text):
    """
    Parsear la lista de estaciones del texto de respuesta, comenzando desde la segunda línea para saltar el encabezado.
    """
    lines = response_text.strip().split("\n")
    stations = []
    for line in lines[1:]:
        parts = line.split("|")
        if len(parts) > 3:
            network, station = parts[0], parts[1]
            stations.append((network, station))

    logging.info(f"Estaciones encontradas: {len(stations)}. Estaciones: {stations}")
    return stations
