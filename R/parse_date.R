#' Parse DateString to POSIXct
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' parse_date(data$EROEFFNUNGSDATUM)
#' parse_date(data$LETZTES_MUTATIONSDATUM)
#' parse_date(data$ERSUCHEN_DATUM)
#' parse_date(data$WEITERLEITUNG_DATUM)
#' parse_date(data$ENTSCHEID_DATUM)
#' parse_date(data$EINGANG_BJ)
parse_date <- function(x){
  # Return NA if all NA
  if (all(is.na(x))) return(x)
  # Return same if already POSIXct
  if(class(x)[1] == "POSIXct") return(x)
  # Return POSIXct if Date
  if(class(x)[1] == "Date") return(x %>%  format("%Y-%m-%d") %>% as.POSIXct())

  # Replace "YYYYMMDD hh_mm" to "YYYYMMDD hh:mm"
  x <- gsub("\\_","\\:", x)

  # Replace ";" with nothing
  x <- gsub(";", "", x)

  # Replace "" with NA character
  x <- if_else(x == "", NA_character_, x)

  # Check String Length
  r <- nchar(x) %>% as_tibble() %>%
       # Take Maximum-Likelihood (ML) first
       group_by(value) %>% summarise(COUNT = n()) %>%
	   # Descending order
       arrange(-COUNT)

  # Analysis string length
  n <- r %>%
       # rm.na (omit NAs)
       filter(!is.na(value)) %>%
       # first ranking only
       pull(value) %>% first

  # Checks
  if (nrow(r) > 2){
    # Warning
    bad <- which(nchar(x) != n)
    names(bad) <- x[bad]
    warning("Bad values!")
    print(bad)
  }


  if (n == 8) {

    if (length(grep("\\.",x)) == 0)
      return(as.POSIXct(x, format="%Y%m%d")) # "YYYYMMDD"
    else if (length(grep("\\.",x)) > 0)
      return(as.POSIXct(x, format="%d.%m.%y")) # "DD.MM.YY"
    else if (length(grep("/",x)) > 0)
      return(as.POSIXct(x, format="%d/%m/%y")) # "DD/MM/YY"

  } else if (n == 10) {

    if (length(grep("\\.",x)) > 0)
      return(as.POSIXct(x, format="%d.%m.%Y")) # "DD.MM.YYYY"
    else if (length(grep("/",x)) > 0)
      return(as.POSIXct(x, format="%d/%m/%Y")) # "DD/MM/YYYY"
    else if (length(grep("-\\d{4}",x)) > 0)
      return(as.POSIXct(x, format="%d-%m-%Y")) # "DD-MM-YYYY"
    else if (length(grep("\\d{4}-",x)) > 0)
      return(as.POSIXct(x, format="%Y-%m-%d")) # "YYYY-MM-DD"
    else
      return(NULL)

  } else if (n == 12) {
    # "YYYYMMDDhhmm"
    return(as.POSIXct(x, format="%Y%m%d%H%M"))
  } else if (n == 16) {
    # "YYYYMMDD hh:mm" or "YYYYMMDD hh_mm"
    return(as.POSIXct(x, format="%d.%m.%Y %H:%M"))
  } else {
    stop("Not suitable format!")
  }

}
