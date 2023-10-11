# Modelo de Kikuchi y Kanamori

## Descripción
El modelo de Kikuchi y Kanamori es una herramienta valiosa en el análisis sísmico. Para obtener más detalles o acceder a la versión original del modelo, visite [el sitio web oficial](https://wwweic.eri.u-tokyo.ac.jp/ETAL/KIKUCHI/).

## Pre-requisitos
Este modelo depende de herramientas externas, específicamente:
- **Rdseed**: Un lector de datos SEED.
- **Seismic Analysis Code (SAC)**: Un conjunto de aplicaciones y bibliotecas para el análisis sísmico.

Los archivos comprimidos necesarios para estas herramientas se encuentran en la carpeta `dependencies/`.

## Instalación
Para simplificar el proceso de instalación de las dependencias, hemos incluido un script que automatiza esta tarea. Para ejecutar el script y realizar la instalación:

1. Asegúrese de tener permisos de ejecución para el script:
   ```bash
   chmod +x dependencies.sh
   ```

2. Ejecute el script:
   ```bash
   ./dependencies.sh
   ```
