## Day 7
library(dplyr)
library(tidyr)
library(stringr)

input <- read.delim("input/day07.txt", sep = " ", header = F, col.names = c("hand", "bid"))
#input <- read.delim("test", sep = " ", header = F, col.names = c("hand", "bid"))

calc_cards_winnings <-function(x){
  x |>
    pivot_wider(id_cols = -c(counts,cards), names_from = order, values_from = card_ranks) |>
    arrange(desc(hand_type), `1`, `2`, `3`, `4`, `5`) |> # arrange by hand type; ties broken by card rank
    mutate(
      hand_rank = nrow(input):1,
      winnings = bid * hand_rank
    )
}

card_rank <- c(2:9, "T", "J", "Q", "K", "A")
names(card_rank) <- 1:13
card_rank <- 13:1
names(card_rank) <- c(2:9, "T", "J", "Q", "K", "A")
cards_long <- input |>
  # create a copy of hand
  mutate(cards = hand) |>
  separate_longer_position(cards, 1) |>
  # count number of each card
  mutate(counts = table(cards), .by = c(hand, cards)) |>
  # each hand type will get a unique sum (higher == better)
  mutate(
    hand_type = sum(counts), 
    order = row_number(),
    .by = hand
    )

cards_long$card_ranks <- card_rank[cards_long$cards]

cards_winnings <- calc_cards_winnings(cards_long)

# Part 1 ------------------------------------------------------------------

sum(cards_winnings$winnings) # 250453939


# Part 2 ------------------------------------------------------------------

# new card rank
names(card_rank) <- c("J", 2:9, "T", "Q", "K", "A")

# recalculate card_ranks
new_cards_long <- cards_long
new_cards_long$card_ranks <- card_rank[new_cards_long$cards]

new_cards_long <- new_cards_long |> 
  # count all cards
  mutate(counts = case_when(
    cards == "J" ~ 1,
    .default = as.numeric(counts)
  ), .by = hand) |>
  # show the highest card in the hand, either as the card with most matches, or lowest rank if only one of each card
  mutate(high = if_else(
    sum(counts) > 5, # all hands at least one pair or better
    max(cards[which(counts == max(counts))]),
    names(card_rank)[card_rank %in% min(card_ranks)] # high card based on highest rank (lowest integer)
    ),
  .by = hand) |>
  # number of jokers
  mutate(n_joker = str_count(hand, "J")) |>
  # add a count to the highest card
  mutate(counts = case_when(
    cards == high & str_detect(hand, "J") & cards != "J" ~ counts + n_joker,
    TRUE ~ counts
    ), .by = hand
  ) |>
  mutate(max_count = max(counts), .by = hand) |>
  mutate(counts = case_when(
    cards == "J" & high != "J" ~ max_count,
    cards == "J" & high == "J" ~ n_joker,
    TRUE ~ as.numeric(counts)
  ), .by = hand) |>
  # recalculate hand_type
  mutate(
    hand_type = sum(counts), 
    order = row_number(),
    .by = hand
  )

new_card_winnings <- calc_cards_winnings(new_cards_long)
sum(new_card_winnings$winnings) #248652697 too high
