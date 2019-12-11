# weaver/spaceman
weaver and spaceman are browser-based Shiny frontends to view internal Lustre volume reports and provide archival decisions on individual project directories.

## Dependencies
* config
* Shiny
* tidyverse
* DT
* DBI
* RMariaDB
* rhandsontable

## Setting up with Docker
1. Clone the repository
2. Create a MySQL database with a table called "lustre_usage" and "spaceman"
3. Enter database credentials into `config.yml`
4. Run `docker-compose up -d` in the repository directory

You should be able to find the apps running on `http://localhost/weaver` and `http://localhost/spaceman`.

## Setting up locally with RStudio
Do steps 1 to 3 as above, then open app.R in RStudio and run the app.
