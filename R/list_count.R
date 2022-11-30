list_count <- function(mylist){
  unlist(lapply(mylist, function(x) length(x)))
}
