import logging
from concurrent.futures import ThreadPoolExecutor, as_completed

from config import (
    EVENT_LATITUDE,
    EVENT_LONGITUDE,
    EVENT_TIME,
    MAX_RADIUS,
    MIN_RADIUS,
    NETWORK_PREFERENCE,
)
from data_fetching import (
    obtener_estaciones_del_response,
    obtener_estaciones_dentro_del_rango,
)
from file_handling import procesar_estacion, verificar_directorio
from utils.time import calcular_rango_tiempo


# Programa principal
def main():
    verificar_directorio()

    EVENT_START_TIME, EVENT_END_TIME = calcular_rango_tiempo(EVENT_TIME)

    # Obtener y parsear estaciones
    stations_response = obtener_estaciones_dentro_del_rango(
        EVENT_LATITUDE,
        EVENT_LONGITUDE,
        EVENT_START_TIME,
        EVENT_END_TIME,
        MIN_RADIUS,
        MAX_RADIUS,
        NETWORK_PREFERENCE,
    )
    updated_stations = obtener_estaciones_del_response(stations_response)

    with ThreadPoolExecutor(max_workers=None) as ejecutor:
        futuro_a_estacion = {
            ejecutor.submit(
                procesar_estacion, red, estacion, EVENT_START_TIME, EVENT_END_TIME
            ): (red, estacion)
            for red, estacion in updated_stations
        }

        for futuro in as_completed(futuro_a_estacion):
            red, estacion = futuro_a_estacion[futuro]
            try:
                futuro.result()
                logging.info(
                    f"Procesamiento completo para la estación: {estacion}, red: {red}"
                )
            except Exception as exc:
                logging.error(f"La estación {estacion} generó una excepción: {exc}")

    logging.info("Todas las tareas han sido completadas.")


if __name__ == "__main__":
    main()
