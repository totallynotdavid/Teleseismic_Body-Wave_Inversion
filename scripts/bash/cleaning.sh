#!/bin/bash

# Navigate to the directory where the files are located
# cd /path/to/your/files

# Loop through all SAC files and check if they don't match the 00.BH* pattern
for file in *.SAC; do
    if [[ ! $file =~ "00.BH" ]]; then
        # Delete the file
        rm "$file"
        echo "Removed: $file"
    fi
done

echo "Filtering completed!"
