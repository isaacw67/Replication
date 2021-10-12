## File to control what gets run:
rm(list=ls())
source("rcode/data_gen.R")

run_pass <- tibble(
  clean    = F,
  fig5     = F,
  table2   = F,
  fig6     = F,
  fig8     = T
)


run_data_get(run_pass)
