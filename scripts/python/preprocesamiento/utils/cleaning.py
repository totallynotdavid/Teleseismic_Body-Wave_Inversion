import glob
import os
import logging

def eliminar_archivos_especificos(base_dir):
    """
    Elimina archivos específicos dentro del directorio proporcionado.

    Args:
    base_dir (str): Ruta del directorio base donde se realizará la eliminación de archivos.
    """
    original_dir = os.getcwd()
    try:
        os.chdir(base_dir)

        # Patrones de archivos a eliminar
        file_patterns = [
            "RESP*.01.*", "RESP*.10.*", "RESP*.20.*", "RESP*.30.*",
            "RESP*.40.*", "RESP*.50.*", "RESP*.60.*",
            "*.mseed", "*.dataless", "*.xml"
        ]

        # Proceso de eliminación de archivos
        for pattern in file_patterns:
            for filename in glob.glob(pattern):
                try:
                    os.remove(filename)
                    logging.info(f"Archivo eliminado: {filename}")
                except OSError as e:
                    logging.error(f"Error al eliminar el archivo {filename}: {e}")

    except Exception as e:
        logging.error(f"Error durante la eliminación de archivos en {base_dir}: {e}")
    finally:
        os.chdir(original_dir)  # Restaurar el directorio original
