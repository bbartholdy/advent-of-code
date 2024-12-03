## Day 9

input <- as.list(readLines("test"))
input <- as.list(readLines("input/day09.txt"))

input_split <- sapply(input, strsplit, " ")
input_num <- sapply(input_split, as.numeric, simplify = F)

out <- vector(mode = "list", length = length(input))
for (i in 1:length(input_num)) {
  out[i] <- list(input_num[i])
}
# create histories
for(i in 1:length(input_num)){
  out[[i]][[2]] <- diff(out[[i]][[1]])
  repeat{
    j <- length(out[[i]]) # counter
    k <- j + 1
    out[[i]][[k]] <- diff(out[[i]][[j]])
    if (all(out[[i]][[k]] == 0)) break # stop when all zeros
  }
}

# Part 1 ------------------------------------------------------------------

# extrapolate
new_out <- out
for(i in 1:length(new_out)){
  for(j in length(new_out[[i]]):1) {
    if(j == length(new_out[[i]])){
      k <- j
    } else {
      k <- j + 1
  }
    n <- length(new_out[[i]][[j]])
    add <- new_out[[i]][[k]][[n]]
    new_out[[i]][[j]][[n + 1]] <- new_out[[i]][[j]][[n]] + add
  }
}

values <- sapply(new_out, function(x) x[[1]][length(x[[1]])])
sum(values) # answer 1974232246


# Part 2 ------------------------------------------------------------------

# Reverse input (can then just re-use code)
new_out <- lapply(out, function(x){
  for (i in 1:length(x)){
    x[[i]] <- rev(x[[i]])
  }
  return(x)
} ) 

# re-extrapolate
for(i in 1:length(new_out)){
  for(j in length(new_out[[i]]):1) {
    if(j == length(new_out[[i]])){
      k <- j
    } else {
      k <- j + 1
    }
    n <- length(new_out[[i]][[j]])
    add <- new_out[[i]][[k]][[n]]
    new_out[[i]][[j]][[n + 1]] <- new_out[[i]][[j]][[n]] - add # need to subtract instead
  }
}

values <- sapply(new_out, function(x) x[[1]][length(x[[1]])])

sum(values) # answer 928


