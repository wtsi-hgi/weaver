# weaver
A browser-based Shiny frontend to view internal Lustre volume reports.

## Dependencies
* Shiny
* tidyverse
* DT
* DBI
* RMariaDB

## Setting up with Docker
1. Clone the repository
2. Create a MySQL database with a table called "lustre_usage"
3. Edit app.R, enter database credentials in empty fields on lines 5 to 7
4. Run `docker build -t hgi-weaver .` in the repository directory
5. Run `docker run -d -p 80:3838 --name=weaver hgi-weaver` 
You should be able to find the app running on `http://localhost/weaver`.

## Setting up locally with RStudio
Do steps 1 to 3 as above, then open app.R in RStudio and run the app.
