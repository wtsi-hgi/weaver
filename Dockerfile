FROM rocker/shiny-verse:3.6.1
RUN install2.r -r "https://cloud.r-project.org" DT RMariaDB DBI config rhandsontable shinyjs
RUN rm -rf /srv/shiny-server/* && mkdir /srv/shiny-server/weaver
COPY weaver /srv/shiny-server/weaver