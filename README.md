# Modelo de Kikuchi y Kanamori

## 📖 Descripción

El modelo de Kikuchi y Kanamori es una herramienta esencial para el análisis sísmico. Si deseas obtener una descripción detallada o acceder a la versión original del modelo, puedes hacerlo a través de [su sitio web oficial](https://wwweic.eri.u-tokyo.ac.jp/ETAL/KIKUCHI/).

## 🛠 Pre-requisitos

Para hacer uso de este modelo, necesitarás instalar ciertas herramientas externas. ¡No te preocupes! Hemos automatizado este proceso para facilitártelo:

- Fortran77
- [Rdseed](https://github.com/iris-edu-legacy/rdseed): Puedes compilarlo desde el código fuente o descargarlo ya compilado desde la [página del IRIS](http://www.iris.edu/pub/programs/rdseedv5.3.1.tar.gz).
- [Seismic Analysis Code](http://ds.iris.edu/ds/nodes/dmc/forms/sac/): Debes completar un formulario para obtenerlo. La respuesta suele ser rápida.
- Java
- Perl

📌 **Nota**: Todos los archivos comprimidos para estas herramientas están disponibles en la carpeta `dependencies/`.

## 🚀 Instalación

Sigue estos sencillos pasos para instalar las dependencias:

1. Navega a la carpeta `scripts/bash/`:
    ```bash
    cd scripts/bash/
    ```

2. Otorga permisos de ejecución al script:
    ```bash
    chmod +x dependencies.sh
    ```

3. Ejecuta el script:
   ```bash
   ./dependencies.sh
   ```

## 🔄 Preprocesamiento

Antes de emplear el modelo, necesitarás obtener datos en formato SAC. ¡También hemos simplificado este proceso!

1. Navega a `scripts/python`
2. Ejecuta:
    ```bash
    python3 fetch_and_process.py
    ```

Este script se encargará de descargar y procesar los datos necesarios. Si deseas obtener detalles técnicos sobre este proceso, consulta el apartado "Detalles técnicos" al final de este documento.

Para limpiar los datos que no son esenciales:

1. Ve a `scripts/bash`
2. Otorga permisos y ejecuta el script de limpieza:
    ```bash
    chmod +x cleaning.sh && ./cleaning.sh
    ```

Finalmente, navega a `scripts/perl` y realiza:
```bash
cp mk_conv.farm.pl ../../data/preprocess/data/ && cd ../../data/preprocess/data/ && perl mk_conv.farm.pl
```

## 🔧 Procesamiento

🚧 Esta sección está en desarrollo. ¡Vuelve pronto para más información!

---

### Detalles técnicos

El script `fetch_and_process.py` realiza la descarga de archivos `.mseed` y metadata en formato `xml` desde IRIS mediante API endpoints públicos. Luego, utiliza `stationxml` para transformar estos archivos `xml` en archivos dataless y, posteriormente, `rdseed` para convertir los archivos `.mseed` y `.xml` en sac.

El criterio de limpieza de datos se basa en la recomendación del [Dr. Sianipar](https://sianipar17.com/2017/12/14/tutorial-for-teleseismic-body-wave-inversion-program/), eliminando archivos que no sigan el patrón 00.BH*.
