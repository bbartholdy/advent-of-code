## Day 2

input <- readLines('2024/input/test.txt')
input <- readLines('2024/input/day-2_input.txt')


# Part 1 -----------------------------------------------------------

# The engineers are trying to figure out which reports are safe.
# The Red-Nosed reactor safety systems can only tolerate levels that are
# either gradually increasing or gradually decreasing. So, a report only
# counts as safe if both of the following are true:
# 
#  - The levels are either all increasing or all decreasing.
#  - Any two adjacent levels differ by at least one and at most three.

# input as list

input_list <- strsplit(input, " ")
diff_list <- lapply(input_list, function(x) diff(as.numeric(x)))

out <- as.data.frame(matrix(nrow = length(input), ncol = 2))
for (i in seq_along(diff_list)) {
  out[i,1] <- all(diff_list[[i]] > 0) & all(diff_list[[i]] <= 3)
  out[i,2] <- all(diff_list[[i]] < 0) & all(diff_list[[i]] >= -3)
}


# remove false entries since both statements cannot be true
false_entries <- which(out$V1 == TRUE & out$V2 == TRUE)
sum(out[-false_entries,]) # 516


# Part 2 -----------------------------------------------------------

# Now, the same rules apply as before, except if removing a single level
# from an unsafe report would make it safe, the report instead counts as safe.

diff_list

# how many issues in each report?

out <- as.data.frame(matrix(nrow = length(input), ncol = 3))
for (i in seq_along(diff_list)) {
  out[i,1] <- ifelse(sum(diff_list[[i]] > 0) == length(diff_list[[i]]), 0, sum(diff_list[[i]] > 0))
  out[i,2] <- ifelse(sum(diff_list[[i]] < 0) == length(diff_list[[i]]), 0, sum(diff_list[[i]] < 0))
  #out[i,3] <- sum(diff_list[[i]] == 0)
  out[i,3] <- sum(abs(diff_list[[i]]) > 3)
}

test <- out
test$issues <- rowSums(out == 0)

# column 1 how many positives
# column 2 how many negatives
# column 2 how many values are greater than 3?
