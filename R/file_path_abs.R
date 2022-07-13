#' File Path to Account with Pattern [xx]
#'
#' @param fls
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
#' path <- ""
#' fls <- list.files(path, recursive = TRUE, full.names = TRUE)
file_path_abs <- function(fls,
                          pattern = "\\["){
  findPath <- function(x) gregexpr(pattern, x)[[1]][1] - 1
  substr(fls, 1, unname(sapply(fls, findPath)))
}
