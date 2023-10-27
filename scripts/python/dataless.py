import requests
import os

# Base URL for the FDSNWS-Station Web Service
BASE_URL = "http://service.iris.edu/fdsnws/station/1/query"

# Create a directory for downloaded metadata
output_dir = "dataless_files"
os.makedirs(output_dir, exist_ok=True)

def sanitize_filename(filename):
    """
    Replace invalid filename characters.
    """
    for char in ['?', '*', ':']:
        filename = filename.replace(char, '_')
    return filename

def download_dataless(network, station, channel, starttime, endtime):
    """
    Download the dataless SEED metadata for a given network, station, channel, and time window.
    """
    params = {
        "network": network,
        "station": station,
        "channel": channel,
        "starttime": starttime,
        "endtime": endtime,
        "format": "xml"  # StationXML format
    }

    response = requests.get(BASE_URL, params=params)

    if response.status_code == 200:
        filename = sanitize_filename(f"{network}_{station}_{channel}_{starttime}.xml")
        with open(os.path.join(output_dir, filename), 'wb') as file:
            file.write(response.content)
        print(f"Downloaded: {filename}")
    else:
        print(f"Failed to download data for {network} {station} {channel} {starttime}-{endtime}. Status code: {response.status_code}")

if __name__ == "__main__":
    # Manifest file
    with open("wilber-david-duran-1-1996-02-21-mw74-off-coast-of-northern-peru.txt", 'r') as file:
        for line in file.readlines():
            parts = line.strip().split()
            network = parts[0]
            station = parts[1]
            channel = parts[3]
            starttime = parts[4]
            endtime = parts[5]
            download_dataless(network, station, channel, starttime, endtime)