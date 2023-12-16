## Alternative solution to Day 1

library(stringi)
# read through the file line-by-line
input <- readLines("input/day01.txt")

replace_word <- function(x){
  out <- dplyr::case_match(
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

# read through the file line-by-line
input <- readLines("input/day01.txt")
out <- vector(mode = "numeric", length = 1) # prepare output vector
for (i in 1:length(input)) {
  input[i] <- paste0(
    stri_extract_first_regex(input[i], "\\d"), # first digit
    stri_extract_last_regex(input[i], "\\d")) # last digit
  out <- out + as.numeric(input[i])
}
out # answer 54953

# Part 2 ------------------------------------------------------------------

# read through the file line-by-line
input <- readLines("input/day01.txt")
pattern <- "[0-9]|on(?=e)|tw(?=o)|thre(?=e)|four|fiv(?=e)|six|seve(?=n)|eigh(?=t)|nin(?=e)"

out <- vector(mode = "list", length = length(input)) # prepare output vector
for (i in 1:length(input)) {
  out[[i]] <- c(
    stri_extract_first_regex(input[i], pattern), # first digit
    stri_extract_last_regex(input[i], pattern)) # last digit
}
out <- unlist(sapply(out, 
  function(x) paste0(replace_word(x), collapse = ""), 
  simplify = F))
sum(as.numeric(out)) # answer 53868
