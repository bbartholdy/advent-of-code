## Day 2 puzzle
library(stringr)
library(dplyr)
library(tidyr)

day2_input <- readr::read_delim(
  "input/day02.txt",
  col_names = c("game", "pick"),
  delim = ":"
)

sets_sep <- day2_input |>
  separate_wider_delim(
    pick, 
    delim = ";",
    names = paste("set", 1:6, sep = "_"),
    too_few = "align_start"
  )

# column for game, set, colour, and value
games_long <- sets_sep |>
  mutate(across(-game, str_trim)) |>
  pivot_longer(-game, names_to = "set", values_to = "pick") |>
  separate_longer_delim(cols = -game, delim = ",") |>
  mutate(
    colour = str_extract(pick, "[:alpha:]+"),
    value = as.numeric(str_extract(pick, "\\d+")),
    game = as.numeric(str_extract(game, "\\d+")),
    .keep = "unused"
  )

# The Elf would first like to know which games would have been possible
# if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

red_cubes <- 12
green_cubes <- 13
blue_cubes <- 14

# Part 1 ------------------------------------------------------------------

possible_games <- games_long |>
  mutate(
    impossible = case_when(
      colour == "green" & value <= green_cubes ~ FALSE,
      colour == "red" & value <= red_cubes ~ FALSE,
      colour == "blue" & value <= blue_cubes ~ FALSE,
      is.na(value) ~ FALSE,
      TRUE ~ TRUE
    )
  ) 

possible_games |>
  group_by(game) |>
  summarise(sum = sum(impossible)) |>
  filter(sum == 0) |>
  _$game |>
  sum() # answer 2265

# Part 2 ------------------------------------------------------------------

games_long |>
  filter(!is.na(value)) |> # remove missing values
  summarise(mins = max(value, na.rm = T), .by = c(game, colour)) |> 
  summarise(power = prod(mins), .by = game) |>
  _$power |>
  sum() # 64097
