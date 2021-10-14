## File to control what gets run:
rm(list=ls())
setwd("~/R/Replication/")
source("rcode/data_gen.R")

run_pass <- tibble(
  clean    = F,
  fig5     = F,
  table2   = T,
  fig6     = F,
  fig8     = F
)


run_data_get(run_pass)

