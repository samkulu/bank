#' Workaround for unicode characters
#'
#' RStudio can't deal with file names with unicode characters.
#' See https://community.rstudio.com/t/rstudio-cant-deal-with-file-names-with-unicode-characters/126601
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
file_name_ascii <- function(x){

  x %>%
    gsub("à", "a", ., ignore.case = TRUE) %>%
    gsub("é", "e", ., ignore.case = TRUE) %>%
    gsub("è", "e", ., ignore.case = TRUE) %>%
    gsub("ä", "ae", ., ignore.case = TRUE) %>%
    gsub("ö", "oe", ., ignore.case = TRUE) %>%
    gsub("ü", "ue", ., ignore.case = TRUE)
}
