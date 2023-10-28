#!/bin/bash

# Descripción: Este script fue creado para instalar las dependencias necesarias para el funcionamiento del modelo de Kikuchi y Kanamori; y sus respectivas dependencias.
# Autor: David Duran

obtener_ubicacion_repo() {
    git rev-parse --show-toplevel
}

instalar_paquetes() {
    sudo apt update -y
    sudo apt upgrade -y
    sudo apt install fort77 default-jre perl libtirpc-dev libc6-dev libncurses5 -y
}

instalar_rdseed() {
    RDSEED_OUTPUT=$(timeout 2s rdseed 2>&1)
    if ! echo "$RDSEED_OUTPUT" | grep -q "IRIS SEED Reader"; then
        echo "Instalando Rdseed..."
        local ARCHIVO_RDSEED="$DIRECTORIO_ZIPS/rdseedv5.3.1.tar.gz"
        if [ ! -f "$ARCHIVO_RDSEED" ]; then
            wget http://www.iris.edu/pub/programs/rdseedv5.3.1.tar.gz -O "$ARCHIVO_RDSEED"
        fi
        tar -xf "$ARCHIVO_RDSEED"
        RDSEED_DIR=$(ls -d rdseedv*/)
        sudo mkdir -p /opt/local/bin
        sudo mv "$RDSEED_DIR/rdseed.rh6.linux_64" /opt/local/bin/rdseed
        sudo chmod +x /opt/local/bin/rdseed
        rm -r "$RDSEED_DIR"
        if ! grep -q "/opt/local/bin" ~/.bashrc; then
            echo "export PATH=\$PATH:/opt/local/bin" >> ~/.bashrc
        fi
    else
        echo "Rdseed ya está instalado."
    fi
}

instalar_sac() {
    if ! which sac &> /dev/null; then
        echo "Instalando SAC..."
        echo "Por favor, descargue SAC manualmente desde el siguiente enlace y colóquelo en $DIRECTORIO_ZIPS:"
        echo " http://ds.iris.edu/ds/nodes/dmc/forms/sac/"
        read -p "Presiona enter una vez que tenga el archivo sac*.tar.gz en $DIRECTORIO_ZIPS..."
        SAC_TAR=$(ls $DIRECTORIO_ZIPS/sac*.tar.gz)
        tar -xf "$SAC_TAR"
        SAC_DIR=$(tar -tf "$SAC_TAR" | head -1 | cut -f1 -d"/")
        sudo mv "$SAC_DIR" /usr/local/
        if ! grep -q "SACHOME=" ~/.bashrc; then
            echo "# Configuraciones para SAC" >> ~/.bashrc
            echo "export SACHOME=/usr/local/sac" >> ~/.bashrc
            echo "export PATH=\$PATH:\$SACHOME/bin" >> ~/.bashrc
            echo "export SACAUX=\$SACHOME/aux" >> ~/.bashrc
        fi
    else
        echo "SAC ya está instalado."
    fi
}

verificar_instalaciones() {
    RDSEED_OUTPUT=$(timeout 2s rdseed 2>&1)
    if echo "$RDSEED_OUTPUT" | grep -q "IRIS SEED Reader"; then
        echo "rdseed parece estar instalado correctamente."
    else
        echo "Error: No se pudo ejecutar rdseed. Revise la instalación."
    fi
    if which sac &> /dev/null; then
        echo "SAC parece estar instalado correctamente."
    else
        echo "Error: No se pudo ejecutar SAC. Revise la instalación."
    fi
}

DIRECTORIO_ZIPS="$(obtener_ubicacion_repo)/dependencies/"
if [ ! -d "$DIRECTORIO_ZIPS" ]; then
    mkdir -p "$DIRECTORIO_ZIPS"
fi

instalar_paquetes
instalar_rdseed
instalar_sac
source ~/.bashrc
verificar_instalaciones
