# Force x86_64 architecture - torch doesn't have ARM64 Linux binaries
ARG BUILD_PLATFORM=linux/amd64
FROM --platform=${BUILD_PLATFORM} r-base:4.5.0

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
ENV CXXFLAGS="-O2 -pipe -std=gnu++17"

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates gnupg apt-transport-https \
    build-essential cmake g++ git wget make \
    automake libtool autoconf pkg-config supervisor \
    ninja-build zip unzip \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y --no-install-recommends \
    libproj-dev libgdal-dev libgeos-dev \
    libnetcdf-dev libcurl4-openssl-dev libcpprest-dev \
    libsqlite3-dev libboost-all-dev \
    libsodium-dev libudunits2-dev \
    zlib1g-dev libunwind-dev libssl-dev libxml2-dev \
    doxygen graphviz \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN git clone --depth 1 --branch 20230802.1 https://github.com/abseil/abseil-cpp.git /tmp/abseil && \
    cmake -S /tmp/abseil -B /tmp/abseil/build \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
      -DABSL_PROPAGATE_CXX_STD=ON \
      -DABSL_ENABLE_INSTALL=ON && \
    cmake --build /tmp/abseil/build -j"$(nproc)" && \
    cmake --install /tmp/abseil/build && \
    rm -rf /tmp/abseil

ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

RUN R -e "install.packages(c('remotes'), repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('devtools'), repos='https://cloud.r-project.org')"

# Install basic packages first (these are dependencies for others)
RUN R -e "install.packages(c('gdalcubes','plumber','useful','s2','sf','rstac','geojsonsf', 'jsonlite', 'base64enc', 'ids', 'callr' , 'sits'), repos='https://cloud.r-project.org')"

# Install torch's Lantern library (required for deep learning models like TempCNN)
# TORCH_INSTALL=1 auto-confirms the interactive prompt
RUN TORCH_INSTALL=1 R -e "torch::install_torch()"


# create directories
RUN mkdir -p /opt/dockerfiles/ && mkdir -p /var/openeo/workspace/ && mkdir -p /var/openeo/workspace/data/

# install packages from local directory
COPY . /opt/dockerfiles/

# Install openstac from GitHub and verify
RUN Rscript -e "remotes::install_github('Open-Earth-Monitor/openstac', ref = 'dev', dependencies = TRUE)" && \
    Rscript -e "if (!require('openstac', quietly=TRUE)) stop('openstac installation failed')"

# Install local openeocraft package
RUN Rscript -e "remotes::install_local('/opt/dockerfiles', dependencies = TRUE, upgrade = 'always', verbose = TRUE)" && \
    Rscript -e "if (!require('openeocraft', quietly=TRUE)) stop('openeocraft installation failed')"

# Verify critical packages are installed
RUN Rscript -e "if (!require('plumber', quietly=TRUE)) stop('plumber is not installed')" && \
    Rscript -e "if (!require('openstac', quietly=TRUE)) stop('openstac is not installed')" && \
    Rscript -e "if (!require('openeocraft', quietly=TRUE)) stop('openeocraft is not installed')" && \
    Rscript -e "if (!require('jsonlite', quietly=TRUE)) stop('jsonlite is not installed')" && \
    Rscript -e "cat('All critical packages verified successfully\n')"

# cmd or entrypoint for startup
CMD ["Rscript", "/opt/dockerfiles/docker/server.R"]

# Expose the port
EXPOSE 8000
