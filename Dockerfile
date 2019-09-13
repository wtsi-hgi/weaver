FROM rocker/shiny-verse:3.6.1
RUN install2.r -r "https://cloud.r-project.org" DT RMariaDB DBI config
RUN rm -rf /srv/shiny-server/* && mkdir /srv/shiny-server/weaver
COPY app.R ggplot_formatter.R /srv/shiny-server/weaver/
