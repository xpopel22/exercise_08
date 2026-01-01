#setwd("V:/MPA-BTB/MPA-PRG/exercise_08")


FindSignedBreakpoint <- function(perm){
  for (i in 1:(length(perm) - 1)){
    if (perm[i + 1] != perm[i] + 1){
      return(i + 1)
    }
  }
  return(-1)
}


IndicateAscendingSigned <- function(perm){
  n <- length(perm)
  asc <- rep(0, n)
  
  for (i in 1:(n - 1)){
    if (perm[i + 1] == perm[i] + 1){
      asc[i] <- 1
      asc[i + 1] <- 1
    }
  }
  return(asc)
}

SignedReversal <- function(perm, i, j){
  perm[i:j] <- -rev(perm[i:j])
  return(perm)
}

HasDescendingSigned <- function(perm){
  any(diff(perm) != 1)
}

SignedBreakpointSort <- function(permutation){
  n <- length(permutation)
  perm <- c(0, permutation, n + 1)
  
  while (TRUE){
    start <- FindSignedBreakpoint(perm)
    if (start == -1) break
    
    x <- perm[start - 1] + 1
    end <- which(abs(perm) == x)
    
    if (length(end) > 0){
      end <- end[1]
      perm <- SignedReversal(perm, start, end)
      
      # oprava znaménka
      if (perm[start] != x){
        perm[start] <- -perm[start]
      }
      next
    }
    
    # fallback – jen jako pojistka
    asc <- IndicateAscendingSigned(perm)
    i <- 1
    while (i < length(perm) && asc[i + 1] == 1){
      i <- i + 1
    }
    
    perm <- SignedReversal(perm, 2, max(3, i))
  }
  
  return(perm[2:(length(perm) - 1)])
}



perm1 <- c(-3, +1, -2, +4)
perm <- perm1
SignedBreakpointSort(perm)
