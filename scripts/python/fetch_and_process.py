import os
import subprocess
import requests
import re

# Configuration
DATA_DIR = "Data"
STATION_URL = "http://service.iris.edu/fdsnws/station/1/query"
DATASELECT_URL = "http://service.iris.edu/fdsnws/dataselect/1/query"
OUTPUT_DIR = "dataless_files"

# Stations and their corresponding network
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

# Event Information
EVENT_INFO = {
    "latitude": -9.6915,
    "longitude": -79.767,
    "starttime": "1996-02-21T12:46:01",  # 5 minutes before the event
    "endtime": "1996-02-21T13:51:01"     # 60 minutes after the event
}

# Create output directories
os.makedirs(DATA_DIR, exist_ok=True)
os.makedirs(OUTPUT_DIR, exist_ok=True)

def sanitize_filename(filename):
    """Sanitize filename by replacing non-alphanumeric characters, except for 'T', with underscores."""
    sanitized_name = re.sub(r'[^a-zA-Z0-9_T.]', '_', filename)  # Preserve 'T'
    sanitized_name = sanitized_name.replace('T', '_')  # Replace "T" between date and time with an underscore
    return sanitized_name

def download_dataless(network, station, starttime, endtime):
    """Download the dataless SEED metadata for a given network, station, and time window."""
    filename = sanitize_filename(f"{network}_{station}_{starttime.replace(':', '_').replace('-', '_')}.xml")
    file_path = os.path.join(OUTPUT_DIR, filename)
    
    if os.path.exists(file_path):
        print(f"Dataless SEED metadata for {network} {station} already exists. Skipping download.")
        return
    
    params = {
        "network": network,
        "station": station,
        "starttime": starttime,
        "endtime": endtime,
        "level": "response",
        "format": "xml"  # StationXML format
    }
    response = requests.get(STATION_URL, params=params)
    if response.status_code == 200:
        with open(file_path, 'wb') as file:
            file.write(response.content)
        print(f"Downloaded: {filename}")

        transform_xml_to_dataless(file_path)
    else:
        print(f"Failed to download data for {network} {station}. Status code: {response.status_code}")

def transform_xml_to_dataless(xml_file):
    """Convert XML to dataless SEED."""
    dataless_file = xml_file.replace('.xml', '.dataless')
    cmd = f"java -jar stationxml-seed-converter-2.1.3.jar -i {xml_file} -o {dataless_file}"
    print(f"Running command: {cmd}")
    subprocess.run(cmd, shell=True)

def download_miniseed(network, station, starttime, endtime):
    """Download miniSEED data for the specified parameters."""
    filename = f"{network}_{station}.mseed"
    file_path = os.path.join(DATA_DIR, filename)
    
    if os.path.exists(file_path):
        print(f"MiniSEED data for {network} {station} already exists. Skipping download.")
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
        print(f"Downloaded miniSEED: {filename}")
    else:
        print(f"Failed to download miniSEED for {network} {station}. Status code: {response.status_code}")

if __name__ == "__main__":
    # Download miniSEED Data and StationXML Metadata
    for network, station in STATIONS:
        download_miniseed(network, station, EVENT_INFO["starttime"], EVENT_INFO["endtime"])
        download_dataless(network, station, EVENT_INFO["starttime"], EVENT_INFO["endtime"])

    # Move to DATA_DIR for rdseed operations
    os.chdir(DATA_DIR)

    # Convert miniSEED to SAC using rdseed
    for network, station in STATIONS:
        miniseed_file = f"{network}_{station}.mseed"
        dataless_file = sanitize_filename(f"{network}_{station}_{EVENT_INFO['starttime']}.dataless")
        dataless_file_path = os.path.join(os.pardir, OUTPUT_DIR, dataless_file)

        if os.path.exists(miniseed_file) and os.path.exists(dataless_file_path):
            print(f"Converting {miniseed_file} to SAC using dataless SEED file...")
            cmd = f"rdseed -f {miniseed_file} -R -d -o 1 -p -g {dataless_file_path}"
            print(f"Running command: {cmd}")
            subprocess.run(cmd, shell=True)
        else:
            print(f"Failed to convert {miniseed_file} to SAC. Missing StationXML metadata or miniSEED data.")
