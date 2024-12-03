## Day 5

input <- readLines("input/day05.txt")
seeds <- strsplit(input[1], " ")
map <- input[-1]

char_index <- grep("^[[:alpha:]]", map)
map_names <- map[char_index]

par <- which(map == "")
map_names <- gsub(" map:", "", map_names)
map_names <- gsub("-", "_", map_names)

map_list <- strsplit(map, " ")

out <- vector(mode = "list")
for (i in 1:length(par[-8])) {
  out[[i]] <- do.call(rbind, map_list[par[i]:par[i+1]])
}
names(out) <- map_names
map_list_datf <- lapply(out, function(x){
  datf <- as.data.frame(x[-1,])
  names(datf) <- c("destination", "source", "length")
  datf
})

map_list_datf <- lapply(map_list_datf, function(x) apply(x, 2, as.numeric))
map_to_datf <- function(x){
  out <- vector(mode = "list", length = nrow(x))
  for (i in 1:nrow(x)){
    start_dest <- x[i,1]
    start_source <- x[i,2]
    length_both <- x[i,3]
    dest <- seq(from = start_dest, to = start_dest + length_both, by = 1)
    source <- seq(from = start_source, to = start_source + length_both, by = 1)
    out[[i]] <- cbind(dest, source)
  }
}
map_to_datf(map_list_datf[[1]])
list2env(out, envir = .GlobalEnv)

