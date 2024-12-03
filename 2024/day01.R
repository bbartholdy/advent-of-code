## Day 1

# Part 1 -----------------------------------------------------------------

input <- read.table("2024/input/test.txt", header = F)
input <- read.table("2024/input/day-1.txt", header = F)

# Pair up the smallest number in the left list with the smallest number
# in the right list, then the second-smallest left number with the 
# second-smallest right number, and so on.

input$V1 <- sort(input$V1)
input$V2 <- sort(input$V2)

# absolute value of difference between left and right
dist <- sum(abs(input$V1 - input$V2))
dist # answer


# Part 2 -----------------------------------------------------------------

# This time, you'll need to figure out exactly how often each number from
# the left list appears in the right list. Calculate a total similarity score
# by adding up each number in the left list after multiplying it by the number
# of times that number appears in the right list.

for(i in seq_along(input$V1)) {
  input$matches[i] <- sum(input$V1[i] == input$V2)
}

# multiply left input by number of matches in right
sum(input$V1 * input$matches) # answer
