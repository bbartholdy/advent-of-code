## Day 15

input <- strsplit(readLines("input/day15.txt"), ",") |> 
  unlist()
# input <- strsplit(readLines("test"), ",") |> 
#   unlist()

# Part 1 ------------------------------------------------------------------

hash <- function(string){
  x <- 0 # Start with current value of 0
  ascii <- utf8ToInt(string) # Determine the ASCII code for the current character of the string.
  for (i in 1:length(ascii)) {
    x <- x + ascii[i] # Increase the current value by the ASCII code you just determined.
    # Set the current value to itself multiplied by 17.
    # Set the current value to the remainder of dividing itself by 256.
    x <- (x * 17) %% 256
    # repeat
  }
  return(x)
}

sum(sapply(input, hash)) # answer 515495

