import os
import logging

# Configuración de logging en caso de error
# Los logs se guardan en la carpeta logs
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

# Información sobre el evento sísmico definida (pendiente: por el usuario)
EVENT_LATITUDE = -9.68
EVENT_LONGITUDE = -80.08
EVENT_START_TIME = "1996-02-21T12:46:01"
EVENT_END_TIME = "1996-02-21T13:51:01"
MIN_RADIUS = 30  # en grados
MAX_RADIUS = 90  # en grados
NETWORK_PREFERENCE = "II,IU"
CHANNELS = ["BHE", "BHN", "BHZ"]
