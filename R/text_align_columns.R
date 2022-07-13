#' Text Align Columns with Fix Space Length
#'
#' @param x
#' @param sPo
#' @param numSpace
#'
#' @return
#' @export
#'
#' @examples
#' ss <- txt
#' text_align_columns(ss[35])
#' xx <- unname(sapply(ss, text_align_columns))
text_align_columns <- function(x, sPo = 50,
                               findSpace = 8,
                               fixSpace = 15){
  tmp <- substr(x, sPo, nchar(x))
  pttrn <- paste0("[ ]{", findSpace,",}")
  # m <- gregexpr(pttrn, tmp)

  paste0(
    substr(x, 1, sPo - 1),
    gsub(pttrn, text_space(fixSpace), tmp)
  )
}
