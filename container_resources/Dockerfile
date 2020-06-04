# Base image https://cloud.docker.com/repository/docker/slink42/rbase_processing
# which in turn uses the base image https://hub.docker.com/u/rocker/

ARG SRC_TAG=suricata
ARG SRC_IMAGE=artificiallyintelligent/shiny_lite
ARG $BLD_DATE
ARG $SRC_REPO
ARG $SRC_BRANCH
ARG $SRC_COMMIT
ARG $DEST_IMAGE

FROM $SRC_IMAGE:$SRC_TAG

ENV BUILD_DATE=$BLD_DATE
ENV SOURCE_DOCKER_IMAGE=$SRC_IMAGE:$SRC_TAG
ENV SOURCE_REPO=$SRC_REPO
ENV SOURCE_BRANCH=$SRC_BRANCH
ENV SOURCE_COMMIT=$SRC_COMMIT
ENV DOCKER_IMAGE=$DEST_IMAGE

LABEL build_version="$SOURCE_REPO/$SOURCE_BRANCH version:- ${VERSION} Build-date:- ${BUILD_DATE}"
LABEL maintainer="$MAINTAINER"
RUN echo "$SOURCE_REPO/$SOURCE_BRANCH version:- ${VERSION} Build-date:- ${BUILD_DATE}"


## install package dependcies
ARG DISCOVERY=TRUE
ARG PROJECT_PACKAGES=redux,shiny,dplyr,shinydashboard,plotly,waiter,tidyjson,hrbrthemes,jsonlite,googlePolylines,RMySQL,pool,stringr,remotes,bubbles,shinySignals,shiny,shinycssloaders,shinydashboard,shinydashboardPlus,shinyjs,shinyWidgets,DT,dygraphs,gridExtra,RColorBrewer,leaflet,leaflet.extras,tibbletime,data.table,purrr,scales,lubridate,janitor,glue,dotenv,auth0,httr,shinyjqui,shinyAce,styler,shinyEffects,rgeolocate,reactlog
ARG PROJECT_PACKAGES_PLUS=
ARG DEPENDENCY=

ENV DISCOVER_PACKAGES=$DISCOVERY
ENV REQUIRED_PACKAGES=$PROJECT_PACKAGES
ENV REQUIRED_PACKAGES_PLUS=$PROJECT_PACKAGES_PLUS
ENV DEPENDENCY_INSTALL=$DEPENDENCY

RUN chmod +x ${SCRIPTS_DIR}/cont-init.d-defaults/* \ 
	&& ${SCRIPTS_DIR}/cont-init.d-defaults/04_expand_packages_dependencies.sh \
	&& ${SCRIPTS_DIR}/cont-init.d-defaults/05_install_package_dependencies.sh \
	&& ${SCRIPTS_DIR}/cont-init.d-defaults/06_install_packages.sh

COPY code $WWW_DIR

RUN ${SCRIPTS_DIR}/cont-init.d-defaults/04_expand_packages_dependencies.sh \
	&& ${SCRIPTS_DIR}/cont-init.d-defaults/05_install_package_dependencies.sh \
	&& ${SCRIPTS_DIR}/cont-init.d-defaults/06_install_packages.sh

ENV INSTALL_PACKAGE_AT_RUNTIME=FALSE

## start shiny server
RUN ln -f ${SCRIPTS_DIR}/shiny-server.sh /usr/bin/shiny-server.sh \
	&& chmod +x /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
