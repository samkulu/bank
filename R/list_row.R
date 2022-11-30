list_row <- function(x, nms){
  row <- gsub("NULL", "", as.character(x[nms]))
  names(row) <- nms
  row
}
