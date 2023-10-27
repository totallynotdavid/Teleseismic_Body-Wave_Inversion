import os
import subprocess

def obtener_ubicacion_repo():
    """
    Retorna el path absoluto al directorio raíz del repositorio.
    """
    return subprocess.check_output(["git", "rev-parse", "--show-toplevel"]).strip().decode('utf-8')
