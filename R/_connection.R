# Connect to the PostgreSQL database --------------------------------------

library(dplyr)

if(!exists("pg")) {

  pg <- src_postgres(dbname = "datacube", host = "ineq.wu.ac.at",
                     user = "lvineq", 
                     password = readline("Password: "), 
                     options = "-c search_path=silc")
} else {
  message("Connection pg already exists.")
}
