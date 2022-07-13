#' Write named Txt-Files
#'
#' @param txtList
#'
#' @return
#' @export
#'
#' @examples
file_write <- function(txtList){
  stopifnot(!is.null(names(txtList)))

  nms <- names(txtList)
  idx <- 1:length(txtList)

  sapply(idx, function(x) writeLines(txtList[[x]], nms[[x]]))
}
