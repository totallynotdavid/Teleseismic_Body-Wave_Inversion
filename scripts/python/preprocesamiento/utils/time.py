from datetime import datetime, timedelta


def calcular_rango_tiempo(event_time_str, minutes_before=5, minutes_after=10):
    """
    Calcular el rango de tiempo para el evento sísmico.

    :param event_time_str: La hora del evento sísmico en formato de cadena.
    :param minutes_before: Minutos antes de la hora del evento para iniciar el rango.
    :param minutes_after: Minutos después de la hora del evento para finalizar el rango.
    :return: Una tupla que contiene las horas de inicio y fin como cadenas.
    """
    event_time = datetime.strptime(event_time_str, "%Y-%m-%dT%H:%M:%S")
    start_time = event_time - timedelta(minutes=minutes_before)
    end_time = event_time + timedelta(minutes=minutes_after)
    return start_time.strftime("%Y-%m-%dT%H:%M:%S"), end_time.strftime(
        "%Y-%m-%dT%H:%M:%S"
    )
