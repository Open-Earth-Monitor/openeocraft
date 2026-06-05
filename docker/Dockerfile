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
    libsodium-dev libudunits2-dev libuv1-dev \
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

ENV PKG_CONFIG_PATH=/usr/local/lib/pkgconfig

RUN R -e "install.packages(c('remotes'), repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('devtools'), repos='https://cloud.r-project.org')"

# Install basic packages first (these are dependencies for others)
RUN R -e "install.packages(c('gdalcubes','plumber','useful','s2','sf','rstac','geojsonsf', 'jsonlite', 'base64enc', 'ids', 'callr' , 'sits'), repos='https://cloud.r-project.org')"

# Install torch's Lantern library (required for deep learning models like TempCNN).
# TORCH_INSTALL=1 auto-confirms the interactive prompt; CUDA=cpu avoids GPU binary
# detection on build hosts without NVIDIA drivers. Retries + long timeout help CI
# survive slow downloads (~170 MB libtorch as of torch 0.17).
ENV TORCH_INSTALL=1
ENV CUDA=cpu
COPY docker/install-torch.R /tmp/install-torch.R
RUN Rscript /tmp/install-torch.R


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

# Fail build if any expected runtime package is missing (incl. sits, torch)
RUN Rscript /opt/dockerfiles/docker/verify-r-deps.R

# cmd or entrypoint for startup
CMD ["Rscript", "/opt/dockerfiles/docker/server.R"]

# Expose the port
EXPOSE 8000
