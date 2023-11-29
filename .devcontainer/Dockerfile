FROM ubuntu:latest
ARG DEBIAN_FRONTEND=noninteractive

# Instalar paquetes básicos
RUN apt-get update && apt-get install -y \
    git \
    fort77 \
    default-jre \
    perl \
    libtirpc-dev \
    libc6-dev \
    libncurses5 \
    wget \
    python3 \
    python3-pip \
    gfortran

RUN echo "La instalación de paquetes ha finalizado."

# Clonamos el repositorio ya que no está disponible durante el Docker build
RUN git clone https://github.com/totallynotdavid/Teleseismic_Body-Wave_Inversion.git /app/repo
RUN echo "Se ha clonado el repositorio."

# Movernos a la carpeta de dependencias
WORKDIR /app/repo/dependencies

RUN echo "Directorio de trabajo cambiado a /app/repo/dependencies"

# Instalación de rdseed
RUN tar -xf rdseedv5.3.1.tar.gz && \
    mv rdseedv5.3.1/rdseed.rh6.linux_64 /usr/local/bin/rdseed && \
    chmod +x /usr/local/bin/rdseed

RUN echo "Instalación de rdseed completada."

# Instalación del SAC
RUN tar -xf sac-102.0-linux_x86_64.tar.gz && \
    mv sac*/ /usr/local/sac && \
    echo "export SACHOME=/usr/local/sac" >> ~/.bashrc && \
    echo "export PATH=\$PATH:\$SACHOME/bin" >> ~/.bashrc && \
    echo "export SACAUX=\$SACHOME/aux" >> ~/.bashrc
RUN echo "Instalación de SAC completada."

# Establecer variables de entorno
ENV SACHOME=/usr/local/sac
ENV PATH="${PATH}:${SACHOME}/bin"
ENV SACAUX=${SACHOME}/aux
RUN echo "Se han establecido las variables de entorno de SAC."

# Limpieza
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN echo "Limpieza terminada"

# Instalar Poetry
RUN curl -sSL https://install.python-poetry.org | python3 -

# Set the entry point
CMD ["/bin/bash"]
