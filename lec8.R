#setwd("V:/MPA-BTB/MPA-PRG/exercise_08")


FindSorted <- function(permutation){
  len <- length(permutation)
  for (i in 1:(len-1)){
    if (permutation[i+1] != (permutation[i]+1)){
      return((i+1))
    }
  }
  return(-1)
}

IndicateAscending <- function(permutation){
  len <- length(permutation)
  vector <- rep(0,len)
  #vector[1] <- 1
  #vector[len] <- 1
  for (i in 1:(len-1)){
    if (permutation[i+1] - permutation[i] == 1){
      vector[i] <- 1
      vector[i+1] <- 1
    }
  }
  return(vector)
}

BreakPointSort <- function(permutation){
  maxim <- max(permutation)+1
  
  #if (permutation[1] != 0){
  #  new_perm <- c(0, permutation, maxim)
  #}
  #else {
  #  new_perm <- c(permutation, maxim)
  #}
  perm <- c(0, permutation, maxim)

  while (TRUE){
    start <- FindSorted(perm)
    if (start == -1) break
    
    if (any(diff(perm) != 1)){
      x <- perm[start - 1] + 1
      end <- which(perm == x)
      if (length(end) == 0) break
      end <- end[1]
      
      perm[start:end] <- rev(perm[start:end])
      
    } else {
      asc <- IndicateAscending(perm)
      
      i <- 1
      while (i < length(perm) && asc[i + 1] == 1){
        i <- i + 1
      }
      
      if (i > 2){
        perm[2:i] <- rev(perm[2:i])
      } else {
        perm[2:3] <- rev(perm[2:3])
      }
    }
  }
  return(perm[2:(length(perm)-1)])
}

perm1 <- c(0,1,2,3,6,7,4,5,8)
perm2 <- c(0,4,5,3,2,1,6,7,8)
perm3 <- c(5,1,4,3,7,8,9,2,6)
perm4 <- c(0, 1, 2, 5, 6, 3, 4, 7, 8, 9)
perm5 <- c(0, 1, 2, 3, 4, 8, 5, 6, 7, 9)
perm6 <- c(0,5,8,7,1,2,4,6,3,9)
perm <- perm4
FindSorted(perm)
IndicateAscending(perm)
BreakPointSort(perm)
