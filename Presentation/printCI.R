# Print pretty value (lower ci - upper ci)

f.ci <- function(x) {
  if (is.numeric(x)==FALSE) x <- as.numeric(x)
  x <- round(x,2)
  y <- paste0(x[1],' (',x[2],'-',x[3],')')
  return(y)
}
