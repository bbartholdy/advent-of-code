## Day 3
library(stringr)
input <- readr::read_lines("input/day03.txt")

# convert to matrix with one cell per character
input_matrix <- do.call(rbind, strsplit(input,""))

char_index <- apply(input_matrix, 1, 
  function(x) which(stringr::str_detect(x, pattern = "[^0-9\\.]"))
)
dot_index <- apply(input_matrix, 1, 
  function(x) which(stringr::str_detect(x, pattern = "\\."))
)

num_index <- lapply(
  input_matrix, 
  function(x) which(stringr::str_detect(x, pattern = "\\d"))
)

i_matrix <- matrix(0, ncol = ncol(input_matrix), nrow = nrow(input_matrix))

for (i in 1:nrow(i_matrix)) {
  i_matrix[i,][char_index[[i]]] <- 1
  i_matrix[i,][num_index[[i]]] <- 2
}

char_index_vec <- grep("[^0-9\\.]", input_matrix)

for (i in 1:nrow(i_matrix)) {
  i_matrix[i,]
}

out <- vector("list", length = length(char_index_vec))
for (i in 1:length(char_index_vec)) {
  x <- char_index_vec[i]
  nw <- input_matrix[x - 139]
  w <- input_matrix[x - 140]
  sw <- input_matrix[x - 141]
  n <- input_matrix[x - 1]
  s <- input_matrix[x + 1]
  ne <- input_matrix[x + 139]
  e <- input_matrix[x + 140]
  se <- input_matrix[x + 141]
  above <- c(nw, n, ne)
  leftright <- c(w, e)
  below <- c(sw, s, se)
  if(sum(grepl("[0-9]", above)) > 0){
    x_up <- x - 1
    #up <- 139 + 140*2
    up <- 140 * 3
    row_up <- input_matrix[seq(from = x_up - up, to = x_up + up, by = 140)]
  } else {
    row_up <- NA
  }
  if(sum(grepl("[0-9]", leftright)) > 0){
    same <- 140*3
    row <- input_matrix[seq(from = x - same, to = x + same, by = 140)]
  } else {
    row <- NA
  }
  if(sum(grepl("[0-9]", below)) > 0){
    x_down <- x + 1
    #down <- 141 + 140*2
    down <- 140 * 3
    row_down <- input_matrix[seq(from = x_down - down, to = x_down + down, by = 140)]
  } else {
    row_down <- NA
  }
  #out[[i]] <- c(nw, w, sw, n, s, ne, e, se)
  out[[i]] <- c(row, row_up, row_down)
}
# out is catching too many numbers, e.g. out[[731]] row_up has a 5 not adjacent to symbol
out_comb <- sapply(out, paste, collapse = "")
out_num <- sapply(str_extract_all(out_comb, "[0-9]+"), as.numeric)
sum(sapply(out_num, sum))
