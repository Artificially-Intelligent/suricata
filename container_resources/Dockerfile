# Base image https://cloud.docker.com/repository/docker/slink42/rbase_processing
# which in turn uses the base image https://hub.docker.com/u/rocker/

ARG SOURCE_IMAGE=artificiallyintelligent/shiny:latest
FROM $SOURCE_IMAGE

# build arguments
ARG PROJECT_ENV=dev
ARG PROJECT=datathon2019
ARG DOCKER_REPO_ROOT=artificiallyintelligent
ARG BUILD_DATE
ARG VERSION=0.x.x
ARG RBASE_RELEASE
ARG MAINTAINER=slink42
ARG UNDERSCORE=_

## set envionment
ENV PROJECT_ENV=${ENV}
ENV PROJECT=${PROJECT}

LABEL build_version="$DOCKER_REPO_ROOT/$PROJECT_ENV$UNDERSCORE$PROJECT$UNDERSCORE$CONTAINER_ROLE version:- ${VERSION} Build-date:- ${BUILD_DATE}"
LABEL maintainer="$MAINTAINER"
RUN echo "$DOCKER_REPO_ROOT/$PROJECT_ENV$UNDERSCORE$PROJECT$UNDERSCORE$CONTAINER_ROLE version:- ${VERSION} Build-date:- ${BUILD_DATE}"

## Copy R code files
COPY /code ${WWW_DIR}

## Copy static data files
COPY /data ${DATA_DIR}

## discover and install missing R-packages
RUN Rscript -e "source('/etc/shiny-server/install_discovered_packages.R'); discover_and_install(default_packages_csv = '/etc/shiny-server/default_install_packages.csv', discovery_directory_root = '${WWW_DIR}', discovery = TRUE);" 

# install non cran packages from github hadley/shinySignals and jcheng5/bubbles
ARG GITHUB_PACKAGES="'hadley/shinySignals','jcheng5/bubbles'"
RUN R -e "remotes::install_github(c($GITHUB_PACKAGES))"

## Start Shiny Server ##
CMD ["/usr/bin/shiny-server.sh"]
