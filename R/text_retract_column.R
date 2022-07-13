#' Text Retract Column to same Next Position
#'
#' @param x
#' @param sPo
#' @param shift
#' @param findLength
#' @param fixLength
#'
#' @return
#' @export
#'
#' @examples
#' ss <- txt
#' xx <- unname(sapply(ss, text_retract_column))
text_retract_column <- function(x, sPo = 50,
                          shift = FALSE,
                          findLength = 3,
                          fixLength = 5){


  tmp <- substr(x, sPo, nchar(x))
  pttrn <- paste0("[ ]{", findLength,",}")
  m <- gregexpr(pttrn, tmp)[[1]]
  p <- as.integer(m)[1]

  # Retract if there is a bigger space found
  if (p < 0 ) return(x)
  l <- attr(m, "match.length")[1]
  # Retract only of the space is bigger than desired
  # Otherwise you get a shift, the opposite of retract
  if(!shift && l < fixLength) return(x)

  # Return
  paste0(
        substr(x, 1, p + sPo),
        text_space(fixLength),
        substr(x, sPo + p + l - 1, nchar(x))

      )

}



