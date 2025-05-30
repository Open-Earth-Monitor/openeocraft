FROM r-base:4.5.0

# install system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    cmake \
    g++ \
    git \
    supervisor \
    wget && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

ENV TZ=Etc/UTC

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libnetcdf-dev \
    libcurl4-openssl-dev \
    libcpprest-dev \
    doxygen \
    graphviz \
    libsqlite3-dev \
    libboost-all-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libproj-dev \
    libgdal-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libsodium-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libfreetype-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# install necessary packages
RUN R -e "install.packages(c('usethis', 'pkgdown', 'rcmdcheck', 'rversions', 'urlchecker'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages('devtools', repos='https://cran.rstudio.com/')"

RUN Rscript -e "install.packages(c('plumber', 'sf', 'rstac','sits', 'stars', 'jsonlite', 'base64enc', 'ids', 'callr'))"

# create directories
RUN mkdir -p /opt/dockerfiles/ && mkdir -p /var/openeo/workspace/ && mkdir -p /var/openeo/workspace/data/

# install packages from local directory
COPY ../ /opt/dockerfiles/
RUN R -e "remotes::install_github('Open-Earth-Monitor/openstac', ref = 'dev')"
RUN R -e "remotes::install_local('/opt/dockerfiles', dependencies = TRUE, upgrade = 'always', verbose = TRUE)"

# check installed packages
RUN R -e "print(installed.packages()[, 'Package'])"

# cmd or entrypoint for startup
CMD ["R", "-q", "--no-save", "-f /opt/dockerfiles/docker/server.R"]

# Expose the port
EXPOSE 8000
