#!/usr/bin/Rscript

library(shiny)

# Test the app
# Seems contrived, but environment variables must
# exist in the cwd as an rds file, because
# shiny-server does not pass on env variables.

Sys.setenv(GED_DB = 'ged',
           GED_USER = 'mihai',
           GED_PASSWORD = 'letmein',
           GED_HOST = '0.0.0.0',
           GED_TABLE = 'ged191',
           GED_PORT = 5555)

env <- as.list(Sys.getenv())

saveRDS(env,'env.rds')

runApp(host = '0.0.0.0', port = 1337)
