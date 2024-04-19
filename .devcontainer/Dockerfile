FROM ubuntu:latest
ARG DEBIAN_FRONTEND=noninteractive

# Instalar paquetes necesarios
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
    gfortran \
    fontconfig \
    vim \
    neovim \
    python3-pygments \
    ttf-mscorefonts-installer \
    ghostscript \
    locales \
    perl-modules \
    libencode-locale-perl && \
    rm -rf /var/lib/apt/lists/*

RUN echo "La instalación de paquetes ha finalizado."

# Ya que los binarios para procesar data sísmica está en el repositorio, clonamos el repositorio
RUN git clone https://github.com/totallynotdavid/Teleseismic_Body-Wave_Inversion.git /app/repo
RUN echo "Repositorio clonado."

# Moverse al directorio de dependencias
WORKDIR /app/repo/dependencies
RUN echo "Directorio de dependencias establecido."

# 1. Instalación de rdseed
RUN tar -xf rdseedv5.3.1.tar.gz && \
    mv rdseedv5.3.1/rdseed.rh6.linux_64 /usr/local/bin/rdseed && \
    chmod +x /usr/local/bin/rdseed
RUN echo "Instalación de rdseed finalizada."

# 2. Instalación de SAC
RUN tar -xf sac-102.0-linux_x86_64.tar.gz && \
    mv sac*/ /usr/local/sac && \
    echo "export SACHOME=/usr/local/sac" >> ~/.bashrc && \
    echo "export PATH=\$PATH:\$SACHOME/bin" >> ~/.bashrc && \
    echo "export SACAUX=\$SACHOME/aux" >> ~/.bashrc
RUN echo "Instalación de SAC finalizada."

# Establecer variables de entorno para SAC
ENV SACHOME=/usr/local/sac
ENV PATH="${PATH}:${SACHOME}/bin"
ENV SACAUX=${SACHOME}/aux
RUN echo "Variables de entorno establecidas."

# 3. Instalar TeXLive
ARG TEXLIVE_VERSION=2023
ARG TEXLIVE_MIRROR="https://mirror.ctan.org/systems/texlive/tlnet"
ENV LANG="es_ES.UTF-8" \
    LANGUAGE="es_ES.UTF-8" \
    TERM="xterm" \
    TEXDIR="/usr/local/texlive/${TEXLIVE_VERSION}" \
    TEXUSERDIR="/texlive-user" \
    PATH="${PATH}"

WORKDIR /tmp/texlive

RUN wget --no-check-certificate "${TEXLIVE_MIRROR}/install-tl-unx.tar.gz" && \
    wget --no-check-certificate "${TEXLIVE_MIRROR}/install-tl-unx.tar.gz.sha512" && \
    sha512sum -c install-tl-unx.tar.gz.sha512 && \
    tar xzf install-tl-unx.tar.gz --strip-components=1 && \
    rm install-tl-unx.tar.gz install-tl-unx.tar.gz.sha512

COPY <<EOF /tmp/texlive/profile.txt
selected_scheme scheme-basic
instopt_letter 1
instopt_adjustpath 0
tlpdbopt_autobackup 0
tlpdbopt_desktop_integration 0
tlpdbopt_file_assocs 0
tlpdbopt_install_docfiles 0
tlpdbopt_install_srcfiles 0
EOF

RUN export TEXLIVE_INSTALL_NO_CONTEXT_CACHE=1 && \
    export TEXLIVE_INSTALL_NO_WELCOME=1 && \
    ./install-tl -profile /tmp/texlive/profile.txt -no-interaction -texdir ${TEXDIR} -texuserdir ${TEXUSERDIR} && \
    rm -rf /tmp/texlive ${TEXDIR}/*.log

ENV PATH="/usr/local/texlive/${TEXLIVE_VERSION}/bin/x86_64-linux:${PATH}"

RUN tlmgr update --self --all && \
    tlmgr install latexmk latexindent booktabs xcolor tikz pgf pgfgantt textpos etoolbox babel-spanish refman fbb xstring xkeyval microtype enumitem listings menukeys adjustbox relsize framed ly1 cm-super && \
    tlmgr update --all && \
    texhash

# Limpieza
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN echo "Limpieza finalizada."

# 4. Instalar Python Poetry
RUN curl -sSL https://install.python-poetry.org | python3 -

# Establecer el directorio de trabajo
CMD ["/bin/bash"]
