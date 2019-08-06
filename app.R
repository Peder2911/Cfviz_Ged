#!/usr/bin/Rscript

library(shiny)

Sys.setenv(GED_DB = 'ged',
           GED_USER = 'mihai',
           GED_PASS = 'letmein',
           GED_HOST = '0.0.0.0',
           GED_TABLE = 'ged191',
           GED_PORT = 5555)
runApp(host = '0.0.0.0', port = 1337)
