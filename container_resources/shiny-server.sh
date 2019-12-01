#!/bin/sh
RENV=/home/shiny/.Renviron
echo 
echo Writing DB environment variables to $RENV
echo Writing OAUTH environment variables to $RENV

printenv | grep DB > $RENV
printenv | grep OAUTH >> $RENV
printenv | grep PROJECT >> $RENV
echo >> $RENV

echo ENV variables that are accessible to shiny: 
cat $RENV

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

if [ "$APPLICATION_LOGS_TO_STDOUT" = "false" ];
then
    exec shiny-server 2>&1
else
    # start shiny server in detached mode
    exec shiny-server 2>&1 &

    # push the "real" application logs to stdout with xtail
    exec xtail /var/log/shiny-server/
fi
