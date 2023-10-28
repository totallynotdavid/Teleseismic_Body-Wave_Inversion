#!/bin/bash

# Descripción: Este script fue creado para instalar las dependencias necesarias para el funcionamiento del modelo de Kikuchi y Kanamori; y sus respectivas dependencias.
# Autor: David Duran

obtener_ubicacion_repo() {
    git rev-parse --show-toplevel
}

# Función wrapper para ejecutar comandos y mostrar un output más amigable
ejecutar_comando() {
    local step=$1
    local total_steps=$2
    local command_name=$3
    shift 3
    echo -ne "$step/$total_steps Trabajando en $command_name...\r"
    SECONDS=0
    "$@"
    echo "$step/$total_steps Trabajando en $command_name... Completado. Nos ha tomado $SECONDS segundos."
}

# Funciones para instalación y verificación

instalar_paquetes() {
    sudo apt update -y &> /dev/null
    sudo apt upgrade -y &> /dev/null
    sudo apt install fort77 default-jre perl libtirpc-dev libc6-dev libncurses5 -y &> /dev/null
}

instalar_rdseed() {
    local ARCHIVO_RDSEED="$DIRECTORIO_ZIPS/rdseedv5.3.1.tar.gz"
    if [ ! -f "$ARCHIVO_RDSEED" ]; then
        wget http://www.iris.edu/pub/programs/rdseedv5.3.1.tar.gz -O "$ARCHIVO_RDSEED"
    fi
    tar -xf "$ARCHIVO_RDSEED"
    local RDSEED_DIR=$(ls -d rdseedv*/)
    sudo mkdir -p /opt/local/bin
    sudo mv "$RDSEED_DIR/rdseed.rh6.linux_64" /opt/local/bin/rdseed
    sudo chmod +x /opt/local/bin/rdseed
    rm -r "$RDSEED_DIR"
    if ! grep -q "/opt/local/bin" ~/.bashrc; then
        echo "export PATH=\$PATH:/opt/local/bin" >> ~/.bashrc
    fi
}

instalar_sac() {
    local SAC_TAR=$(ls $DIRECTORIO_ZIPS/sac*.tar.gz)
    tar -xf "$SAC_TAR"
    local SAC_DIR=$(tar -tf "$SAC_TAR" | head -1 | cut -f1 -d"/")
    sudo mv "$SAC_DIR" /usr/local/
    if ! grep -q "SACHOME=" ~/.bashrc; then
        echo "# Configuraciones para SAC" >> ~/.bashrc
        echo "export SACHOME=/usr/local/sac" >> ~/.bashrc
        echo "export PATH=\$PATH:\$SACHOME/bin" >> ~/.bashrc
        echo "export SACAUX=\$SACHOME/aux" >> ~/.bashrc
    fi
}

# Principal

DIRECTORIO_ZIPS="$(obtener_ubicacion_repo)/dependencies/"
if [ ! -d "$DIRECTORIO_ZIPS" ]; then
    mkdir -p "$DIRECTORIO_ZIPS"
fi

ejecutar_comando 1 5 "instalar paquetes" instalar_paquetes

RDSEED_OUTPUT=$(timeout 2s rdseed 2>&1)
if ! echo "$RDSEED_OUTPUT" | grep -q "IRIS SEED Reader"; then
    ejecutar_comando 2 5 "instalar Rdseed" instalar_rdseed
else
    echo "2/5 Rdseed ya está instalado. Saltando..."
fi

if ! which sac &> /dev/null; then
    echo "3/5 Por favor descargar SAC manualmente del siguiente link y guárdalo en $DIRECTORIO_ZIPS:"
    echo "http://ds.iris.edu/ds/nodes/dmc/forms/sac/"
    read -p "Presiona enter una vez que el archivo sac*.tar.gz esté en $DIRECTORIO_ZIPS..."
    ejecutar_comando 3 5 "Instalando SAC" instalar_sac
else
    echo "3/5 SAC ya está instalado. Saltando..."
fi

ejecutar_comando 4 5 "actualizar con bashrc" source ~/.bashrc

RDSEED_OUTPUT=$(timeout 2s rdseed 2>&1)
if echo "$RDSEED_OUTPUT" | grep -q "IRIS SEED Reader"; then
    echo "4/5 rdseed está instalado correctamente."
else
    echo "4/5 Error: No se pudo ejecutar rdseed. Revisa la instalación"
fi

if which sac &> /dev/null; then
    echo "5/5 SAC está instalado correctamente."
else
    echo "5/5 Error: No se pudo ejecutar SAC. Revisa la instalación."
fi
