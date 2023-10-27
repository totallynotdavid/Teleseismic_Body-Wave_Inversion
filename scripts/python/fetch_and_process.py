import os
import subprocess
import requests
import re

# Configuración y endpoints
DATA_DIR = "Data"
OUTPUT_DIR = "dataless_files"
STATION_URL = "http://service.iris.edu/fdsnws/station/1/query"
DATASELECT_URL = "http://service.iris.edu/fdsnws/dataselect/1/query"

# Estaciones y redes a descargar
STATIONS = [
    ("II", "ASCN"),  # Butt Crater, Ascension Island
    ("IU", "CCM"),  # Cathedral Cave, Missouri, USA
    ("IU", "COL"),  # College Outpost, Alaska, USA
    ("IU", "COR"),  # Corvallis, Oregon, USA
    ("CN", "FFC"),  # Flin Flon, Canada
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

# Información sobre el evento
EVENT_INFO = {
    "latitude": -9.6915,
    "longitude": -79.767,
    "starttime": "1996-02-21T12:46:01",  # 5 minutos antes del evento
    "endtime": "1996-02-21T13:51:01"     # 60 minutos después del evento
}

# Asegurarse que los directorios existan
os.makedirs(DATA_DIR, exist_ok=True)
os.makedirs(OUTPUT_DIR, exist_ok=True)

def sanitize_filename(filename):
    """
    Sanitizar el nombre del archivo reemplazando los caracteres no alfanuméricos, excepto 'T', con guiones bajos.
    """
    sanitized_name = re.sub(r'[^a-zA-Z0-9_T.]', '_', filename)  # Preservar la 'T' entre fecha y hora
    sanitized_name = sanitized_name.replace('T', '_')  # Reemplazar la 'T' con un guión bajo
    return sanitized_name

def download_dataless(network, station, starttime, endtime):
    """
    Descargar los metadatos de dataless SEED para una red, estación y ventana de tiempo dada.
    """
    filename = sanitize_filename(f"{network}_{station}_{starttime.replace(':', '_').replace('-', '_')}.xml")
    file_path = os.path.join(OUTPUT_DIR, filename)
    
    if os.path.exists(file_path):
        print(f"Los Dataless SEED metadata para `{network} {station}` ya existe. Saltando la descarga.")
        return
    
    params = {
        "network": network,
        "station": station,
        "starttime": starttime,
        "endtime": endtime,
        "level": "response",
        "format": "xml"  # Formato de salida. StationXML.
    }
    response = requests.get(STATION_URL, params=params)
    if response.status_code == 200:
        with open(file_path, 'wb') as file:
            file.write(response.content)
        print(f"Descargado: {filename}")

        transform_xml_to_dataless(file_path)
    else:
        print(f"Hubo un error al descargar los metadatos de dataless SEED para `{network} {station}`. 
              Código de estado: {response.status_code}")

def transform_xml_to_dataless(xml_file):
    """
    Convertir XML a dataless SEED.
    """
    dataless_file = xml_file.replace('.xml', '.dataless')
    cmd = f"java -jar stationxml-seed-converter-2.1.3.jar -i {xml_file} -o {dataless_file}"
    print(f"Ejecutando: {cmd}")
    subprocess.run(cmd, shell=True)

def download_miniseed(network, station, starttime, endtime):
    """
    Descargar datos miniSEED para los parámetros definidos en la configuración
    """
    filename = f"{network}_{station}.mseed"
    file_path = os.path.join(DATA_DIR, filename)

    if os.path.exists(file_path):
        print(f"MiniSEED data para {network} {station} ya existe. Saltando la descarga.")
        return

    params = {
        "network": network,
        "station": station,
        "starttime": starttime,
        "endtime": endtime,
        "format": "mseed"
    }
    response = requests.get(DATASELECT_URL, params=params)
    if response.status_code == 200:
        with open(file_path, 'wb') as file:
            file.write(response.content)
        print(f"Descargado miniSEED: {filename}")
    else:
        print(f"Ha ocurrido un error al descargar los datos miniSEED para `{network} {station}`. 
              Código de estado: {response.status_code}")

# Programa principal
if __name__ == "__main__":
    # Descargar los datos miniSEED y los metadatos StationXML para cada estación
    for network, station in STATIONS:
        download_miniseed(network, station, EVENT_INFO["starttime"], EVENT_INFO["endtime"])
        download_dataless(network, station, EVENT_INFO["starttime"], EVENT_INFO["endtime"])

    # Mover a DATA_DIR para las operaciones de rdseed
    os.chdir(DATA_DIR)

    # Convertir miniSEED a SAC usando rdseed
    for network, station in STATIONS:
        miniseed_file = f"{network}_{station}.mseed"
        dataless_file = sanitize_filename(f"{network}_{station}_{EVENT_INFO['starttime']}.dataless")
        dataless_file_path = os.path.join(os.pardir, OUTPUT_DIR, dataless_file)

        if os.path.exists(miniseed_file) and os.path.exists(dataless_file_path):
            print(f"Convirtiendo {miniseed_file} a SAC usando el archivo dataless SEED...")
            cmd = f"rdseed -f {miniseed_file} -R -d -o 1 -p -g {dataless_file_path}"
            print(f"Ejecutando: {cmd}")
            subprocess.run(cmd, shell=True)
        else:
            print(f"Ha habido un error al convertir {miniseed_file} a SAC. Faltan los metadatos StationXML o los datos miniSEED.")
