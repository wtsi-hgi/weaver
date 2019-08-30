# weaver
A browser-based Shiny frontend to view internal Lustre volume reports.

## Dependencies
* Shiny
* tidyverse
* DT
* DBI
* RMySQL

## Setting up
1. Clone the repository
2. Create a MySQL database called "lustre_usage"
3. Create tables called `report-YYYYMMDD` containing Lustre Usage-Quota Report data
4. Open app.R in RStudio, run the app
