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
RUN R -e "install.packages('remotes', repos='https://cran.rstudio.com/')"

# Install basic packages first (these are dependencies for others)
RUN Rscript -e "install.packages(c('jsonlite', 'base64enc', 'ids', 'callr'), repos='https://cran.rstudio.com/')" || true

# Install plumber (critical for the API)
RUN Rscript -e "install.packages('plumber', repos='https://cran.rstudio.com/')" && \
    Rscript -e "if (!require('plumber', quietly=TRUE)) stop('plumber installation failed')"

# Install spatial packages (these may take longer and have more dependencies)
RUN Rscript -e "install.packages(c('sf', 'stars'), repos='https://cran.rstudio.com/')" || true
RUN Rscript -e "install.packages('rstac', repos='https://cran.rstudio.com/')" || true
RUN Rscript -e "install.packages('sits', repos='https://cran.rstudio.com/')" || true

# create directories
RUN mkdir -p /opt/dockerfiles/ && mkdir -p /var/openeo/workspace/ && mkdir -p /var/openeo/workspace/data/

# install packages from local directory
COPY . /opt/dockerfiles/
RUN R -e "remotes::install_github('Open-Earth-Monitor/openstac', ref = 'dev')"
RUN R -e "remotes::install_local('/opt/dockerfiles', dependencies = TRUE, upgrade = 'always', verbose = TRUE)"

# Verify critical packages are installed
RUN Rscript -e "if (!require('plumber', quietly=TRUE)) stop('plumber is not installed')" && \
    Rscript -e "if (!require('jsonlite', quietly=TRUE)) stop('jsonlite is not installed')" && \
    Rscript -e "cat('Critical packages verified successfully\n')"

# cmd or entrypoint for startup
CMD ["Rscript", "/opt/dockerfiles/docker/server.R"]

# Expose the port
EXPOSE 8000
