#' Parse Double
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' parse_double("702'°690.52 -")
#' parse_double("702'690,52 +")
#' parse_double("(702'690,52)")
#' parse_double("")
#' parse_double("2'245'595,54 CR")
#' parse_double("962'499 82 DE")
parse_double <- function(x){

  if (class(x) == "numeric") return(x)
  if (class(x) == "integer") return(x)
  if (class(x) == "double") return(x)

  if (class(x) == "character"){
     x %>%
    # Replace capital "o" with zeros
    gsub("O","0", . ) %>%
    # Remove
    gsub("'|°|’","", . ) %>%
    # Trailing + or CR
    gsub("\\+|CR", "", .) %>%
    # Trailing -
    gsub("(.*)(-|DB|DE)","-\\1", . ) %>%
    # Braket negtive number
    gsub("\\(", "-", .) %>% gsub("\\)", "", .) %>%
    # Remove doubles
    gsub("--", "-", .) %>%
    gsub("--", "-", .) %>%
    # Decimals
    gsub("(.*)(,| )(\\d{2})","\\1.\\3", . ) %>%
    # Remove spaces
    gsub(" ", "", .) %>%
    # Numeric
    as.double() %>%
    # Return
    return()

  } else {
    stop("unknown")
  }
}
