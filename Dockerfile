FROM r-base:4.3.0

# install system dependencies
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y software-properties-common cmake g++ git supervisor wget
ENV TZ=Etc/UTC
RUN apt-get install  -y libnetcdf-dev libcurl4-openssl-dev libcpprest-dev doxygen graphviz  libsqlite3-dev libboost-all-dev
RUN apt-get update && apt-get install -y libproj-dev libgdal-dev
RUN apt-get update && apt-get install -y libudunits2-dev libsodium-dev

RUN apt-get install -y libfreetype-dev

# install necessary packages
RUN R -e "install.packages('devtools')"

RUN Rscript -e "install.packages(c('plumber', 'sf', 'rstac','sits', 'stars', 'jsonlite', 'base64enc'))"

# create directories
RUN mkdir -p /opt/dockerfiles/ && mkdir -p /var/openeo/workspace/ && mkdir -p /var/openeo/workspace/data/

# install packages from local directory
COPY ./ /opt/dockerfiles/
RUN R -e "remotes::install_local('/opt/dockerfiles', dependencies = TRUE, upgrade = 'always', verbose = TRUE)"

# check installed packages
RUN R -e "print(installed.packages()[, 'Package'])"

# cmd or entrypoint for startup
CMD ["R", "-q", "--no-save", "-f /opt/dockerfiles/docker/server.R"]

# Expose the port
EXPOSE 8000
