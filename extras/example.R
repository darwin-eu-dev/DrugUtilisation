library(dplyr)
library(dbplyr)
library(CDMConnector)
library(DBI)

db <- dbConnect(dbms = "sql server",
                server = Sys.getenv("darwinDbDatabaseServer"),
                database = Sys.getenv("darwinDbDatabase"),
                port = Sys.getenv("darwinDbDatabasePort"),
                user = Sys.getenv("darwinDbUser"),
                password = Sys.getenv("darwinDbPassword"),
                pathToDriver = here::here())
