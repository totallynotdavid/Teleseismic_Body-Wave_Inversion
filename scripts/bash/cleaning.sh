#!/bin/bash

# Navegar al directorio donde se encuentran los archivos
# cd /path-al-directorio

# Iterar sobre todos los archivos SAC y verificar si no coinciden con el patrón 00.BH*
for file in *.SAC; do
    if [[ ! $file =~ "00.BH" ]]; then
        rm "$file"
        echo "$file fue eliminado."
    fi
done

echo "Filtrado completado."
