#' Text Space
#'
#' @param x Length of space
#'
#' @return
#' @export
#'
#' @examples
#' text_space(10)
#' text_space(100)
text_space <- function(x){
  paste0(rep(" ", x), collapse="")
}
