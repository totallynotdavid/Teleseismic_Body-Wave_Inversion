#!/bin/bash

# Instalador de Rdseed y SAC para análisis sísmico

# Directorio para guardar los archivos comprimidos
DIRECTORIO_ZIPS="dependencies/" #

# Comprobar y/o crear el directorio de dependencias
if [ ! -d "$DIRECTORIO_ZIPS" ]; then
    mkdir -p "$DIRECTORIO_ZIPS"
fi

# Instalación de paquetes necesarios
echo "Instalando paquetes necesarios..."
sudo apt update
sudo apt install fort77 libtirpc-dev libc6-dev libncurses5 -y

# Instalación de Rdseed
RDSEED_OUTPUT=$(timeout 2s rdseed 2>&1)
if ! echo "$RDSEED_OUTPUT" | grep -q "IRIS SEED Reader"; then
    echo "Instalando Rdseed..."
    
    if [ ! -f "$DIRECTORIO_ZIPS/rdseedv5.3.1.tar.gz" ]; then
        wget http://www.iris.edu/pub/programs/rdseedv5.3.1.tar.gz -O "$DIRECTORIO_ZIPS/rdseedv5.3.1.tar.gz"
    fi
    
    tar -xf "$DIRECTORIO_ZIPS/rdseedv5.3.1.tar.gz"
    RDSEED_DIR=$(ls -d rdseedv*/)
    
    sudo mkdir -p /opt/local/bin
    sudo mv $RDSEED_DIR/rdseed.rh6.linux_64 /opt/local/bin/rdseed
    sudo chmod +x /opt/local/bin/rdseed
    rm -r $RDSEED_DIR

    if ! grep -q "/opt/local/bin" ~/.bashrc; then
        echo "export PATH=\$PATH:/opt/local/bin" >> ~/.bashrc
    fi
else
    echo "Rdseed ya está instalado."
fi

# Instalación de SAC
if which sac &> /dev/null; then
    SAC_OUTPUT=$(timeout 2s sac &> /dev/null & echo $!)
    kill $SAC_OUTPUT
    echo "SAC ya está instalado."
else
    echo "Instalando SAC..."

    echo "Por favor, descargue SAC manualmente desde el siguiente enlace y colóquelo en $DIRECTORIO_ZIPS:"
    echo " http://ds.iris.edu/ds/nodes/dmc/forms/sac/"
    read -p "Presione cualquier tecla una vez que tenga el archivo sac*.tar.gz en $DIRECTORIO_ZIPS..."

    SAC_TAR=$(ls $DIRECTORIO_ZIPS/sac*.tar.gz)
    tar -xf $SAC_TAR
    SAC_DIR=$(tar -tf $SAC_TAR | head -1 | cut -f1 -d"/")
    
    sudo mv $SAC_DIR /usr/local/
    rm -r $SAC_DIR

    if ! grep -q "SACHOME=" ~/.bashrc; then
        echo "# Configuraciones para SAC" >> ~/.bashrc
        echo "export SACHOME=/usr/local/sac" >> ~/.bashrc
        echo "export PATH=\$PATH:\$SACHOME/bin" >> ~/.bashrc
        echo "export SACAUX=\$SACHOME/aux" >> ~/.bashrc
    fi
fi

source ~/.bashrc

# Verificar instalaciones
RDSEED_OUTPUT=$(timeout 2s rdseed 2>&1)
if echo "$RDSEED_OUTPUT" | grep -q "IRIS SEED Reader"; then
    echo "rdseed parece estar instalado correctamente."
    pkill -f rdseed
else
    echo "Error: No se pudo ejecutar rdseed. Revise la instalación."
fi

if which sac &> /dev/null; then
    SAC_OUTPUT=$(timeout 2s sac &> /dev/null & echo $!)
    kill $SAC_OUTPUT
    echo "SAC parece estar instalado correctamente."
else
    echo "Error: No se pudo ejecutar SAC. Revise la instalación."
fi
