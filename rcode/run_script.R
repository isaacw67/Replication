## File to control what gets run:
rm(list=ls())
setwd("~/R/Replication/")
source("rcode/data_gen.R")

run_pass <- tibble(
  clean    = T,
  fig5     = T,
  table2   = T,
  fig6     = T,
  fig8     = T
)


run_data_get(run_pass)

