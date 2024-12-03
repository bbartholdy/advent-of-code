## Day 4

library(tidyr)
library(dplyr)
library(stringr)
input <- read.delim("input/day04.txt", header = F, sep = ":")

input_clean <- input |>
  separate_wider_delim(V2, delim = "|", names = c("winning", "own")) |>
  mutate(across(everything(), str_squish)) |>
  mutate(across(c("winning", "own"), strsplit, " "))

# Part 1 ------------------------------------------------------------------

for (i in 1:nrow(input_clean)) {
  input_clean$matches[[i]] <- as.numeric(input_clean$own[[i]] %in% input_clean$winning[[i]])
  input_clean$matches[[i]] <- input_clean$matches[[i]][input_clean$matches[[i]] == 1]
}

double_vector <- c(1, 1, 2, 4, 8, 16, 32, 64, 128, 256)

for (i in 1:nrow(input_clean)) {
  if(sum(input_clean$matches[[i]]) > 0) {
    double_vec <- double_vector[1:length(input_clean$matches[[i]])]
    input_clean$score[i] <- sum(double_vec)
  } else {
    input_clean$score[i] <- 0
  }
}
sum(input_clean$score)


# Part 2 ------------------------------------------------------------------

for (i in 1:nrow(input_clean)) {
  input_clean$count[i] <- length(input_clean$matches[[i]])
}

# iteratively add to the number of copies based on the score from input_clean$new_score
input_clean$copies <- 1
for (i in 1:nrow(input_clean)) {
  start <- i + 1
  n_rows <- input_clean$count[i]
  if(n_rows == 0) next
  finish <- start + n_rows - 1
  add <- input_clean$copies[i]
  input_clean$copies[start:finish] <- input_clean$copies[start:finish] + add
}
sum(input_clean$copies)

