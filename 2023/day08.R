## Day 8
library(stringr)
instructions <- readLines("test")[1] |>
  gsub(pattern = "L", replacement = "1") |>
  gsub(pattern = "R", replacement = "2") |>
  strsplit("", fixed = T) |>
  unlist() |>
  as.numeric()

input <- read.delim("test", sep = "=", header = F, col.names = c("node", "direction"), skip = 1)

input_clean <- input |>
  mutate(direction = str_remove_all(direction, "[\\)\\(]")) |>
  mutate(direction = strsplit(direction, ","))
