nullToNA <- function(x) {
x[sapply(x, is.null)] <- NA
return(x)
}


## Round of 2
round_of_two <- function(a){
  return(round(a,2))
}