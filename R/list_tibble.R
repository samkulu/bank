list_tibble <- function(mylist){
  # find column names
  nms <- list_unique_names(mylist)

  # Fill list with missing cols
  df <- lapply(mylist, list_row,
               nms = nms )

  # list to data.frame
  result <- do.call(rbind.data.frame, df)
  colnames(result) <- nms

  # Return tibble
  as_tibble(result)
}
