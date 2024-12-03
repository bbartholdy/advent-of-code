## Day 1

library(dplyr)
library(stringr)

input <- readLines("input/day01.txt")

# function to extract first and last element
first_last <- function(x){
  out <- vector(mode = "character", length = length(x))
  for (i in 1:length(x)) {
    len_num <- length(x[[i]])
      out[i] <- paste0(x[[i]][1], x[[i]][length(x[[i]])]) # first and last (duplicates single elements)
  }
  out
}

replace_word <- function(x){
  out <- case_match(
    x,
    "on" ~ "1",
    "tw" ~ "2",
    "thre" ~ "3",
    "four" ~ "4",
    "fiv" ~ "5",
    "six" ~ "6",
    "seve" ~ "7",
    "eigh" ~ "8",
    "nin" ~ "9",
    .default = x
  )
  out
}


# Part 1 ------------------------------------------------------------------

# replace all non-numbers with blank then split into separate strings
num <- gsub("\\D", "", input)
num_list <- strsplit(num, "")

# extract first and last element
out <- first_last(num_list)

sum(as.numeric(out)) # answer 54953


# Part 2 ------------------------------------------------------------------

# extract numbers and number-words
all_num_list <- str_extract_all(input, "[0-9]|on(?=e)|tw(?=o)|thre(?=e)|four|fiv(?=e)|six|seve(?=n)|eigh(?=t)|nin(?=e)") # not matching overlaps

num_conv <- lapply(all_num_list, replace_word)

out <- first_last(num_conv)

sum(as.numeric(out)) # answer 53868
