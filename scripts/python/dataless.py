import requests
import os

# Endpoint para el servicio web FDSNWS-Station
BASE_URL = "http://service.iris.edu/fdsnws/station/1/query"

# Crear un directorio para los metadatos descargados
output_dir = "dataless_files"
os.makedirs(output_dir, exist_ok=True)

def sanitizar_nombre_archivo(filename):
    """
    Reemplazar caracteres inválidos en el nombre del archivo.
    """
    for char in ['?', '*', ':']:
        filename = filename.replace(char, '_')
    return filename

def descargar_dataless(network, station, channel, starttime, endtime):
    """
    Descargar los metadatos de dataless SEED para una red, estación, canal y rango de tiempo dado.
    """
    params = {
        "network": network,
        "station": station,
        "channel": channel,
        "starttime": starttime,
        "endtime": endtime,
        "format": "xml"  # Formato StationXML
    }

    response = requests.get(BASE_URL, params=params)

    if response.status_code == 200:
        filename = sanitizar_nombre_archivo(f"{network}_{station}_{channel}_{starttime}.xml")
        with open(os.path.join(output_dir, filename), 'wb') as file:
            file.write(response.content)
        print(f"Descargado: {filename}")
    else:
        print(f"Ha ocurrido un error al descargar los metadatos de dataless SEED para 
              `{network} {station} {channel} {starttime}-{endtime}`. 
              Código de estado: {response.status_code}")

if __name__ == "__main__":
    # Archivo de texto con los parámetros de descarga
    with open("wilber-david-duran-1-1996-02-21-mw74-off-coast-of-northern-peru.txt", 'r') as file:
        for line in file.readlines():
            parts = line.strip().split()
            network = parts[0]
            station = parts[1]
            channel = parts[3]
            starttime = parts[4]
            endtime = parts[5]
            descargar_dataless(network, station, channel, starttime, endtime)