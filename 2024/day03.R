## Day 3
library(stringi)

#input_comb <- "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" # part 1 test
#input_comb <- "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" # part 2 test
input <- readLines('2024/input/day-3_input.txt')


# Part 1 -----------------------------------------------------------------

input_comb <- paste0(input, collapse = "")
all_muls <- stri_extract_all(input_comb, regex = "mul\\([\\d]+,[\\d]+\\)")

first_num <- as.numeric(sapply(all_muls, stri_extract, regex = "\\d+"))
second_num <- as.numeric(sapply(all_muls, stri_extract, regex = "(?<=,)\\d+"))

sum(first_num * second_num) # answer


# Part 2 -----------------------------------------------------------------

# remove everything between don't and do
input_rm <- stri_replace_all(input_comb, "", regex = "(?<=don't\\(\\))(.*?)(?=do\\(\\))")

# repeat part 1 steps
all_muls <- stri_extract_all(input_rm, regex = "mul\\([\\d]+,[\\d]+\\)")
first_num <- as.numeric(sapply(all_muls, stri_extract, regex = "\\d+"))
second_num <- as.numeric(sapply(all_muls, stri_extract, regex = "(?<=,)\\d+"))

sum(first_num * second_num) # answer

