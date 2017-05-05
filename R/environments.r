my_env <- new.env(parent = emptyenv())
my_env$a <- 1

blah <- function(){
  return(my_env$a)
}
