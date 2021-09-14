# weaver
weaver is a browser-based Shiny frontend to view internal Lustre volume reports

## Dependencies
* config
* Shiny
* shinyjs
* tidyverse
* DT
* DBI
* RMariaDB
* rhandsontable

## Setting up with Docker
1. Clone the repository
2. Create a MySQL database, as detailed in `github/wtsi-hgi/lurge/docs/database`
3. Copy `config.example.yml` to `config.yml` and enter database credentials into `config.yml`
4. Run `docker-compose up -d` in the repository directory

You should be able to find the apps running on `http://localhost/weaver`

## Setting up locally with RStudio
Do steps 1 to 3 as above, then open app.R in RStudio and run the app.

## Setting up locally without RStudio
Do steps 1 to 3 as above, then open an R terminal in the project directory.
Ensure the dependencies are installed.

Run `library("shiny")` and then `runApp("weaver")`