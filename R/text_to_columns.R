#' Get Header Columns
#'
#' @param page Input text as page or line splitted strings
#' @param pHeader Pattern for Header
#' @param pDescription Pattern for Description
#' @param pTransaction Pattern for Transactions
#' @param preProcess Pre-Process txt with list of gsub replacements
#' @param postProcess
#'
#' @return
#' @export
#'
#' @examples
#' text_to_columns(page)
text_to_columns <- function(page,
                               pHeader = "Reference.*Trade_date.*Value_date",
                               pFooter = "",
                               pDescription = "Text|Description|Descrizione|Information|Kurzform|Forma|Forme",
                               pTransaction = "([0-9]{2}\\.[0-9]{2}\\.[0-9]{4}).*([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})?",
                               preProcess = NULL,
                               postProcess = NULL,
                               verbose = TRUE){

  # Helper Functions
  findCurrency <- function(x){
    # Find occurence
    m <- gregexpr("USD|CHF|EUR|GBP|CAD|JPY|XAU",x)
    # Take first only
    regmatches(x,m)[[1]][1]
  }

  findAccount <- function(x){
    # take info only from lines above header
    y <- x[1:idxHeader]

    # find lines and take only first
    tmp <- y[grep("acc.*number|IBAN", x, ignore.case = T)[1]]



    # remove all unnecessiary spaces
    # trimws(gsub("\\s+", " ", tmp))
    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", tmp, perl=TRUE)
  }

  findDate <- function(x){
    tmp <- x[1:idxHeader]
    idx <- grep("([0-9]{2}.[0-9]{2}.(19|20)[0-9]{2})|((19|20)[0-9]{2}.[0-9]{2}.[0-9]{2})",tmp)
    # Take last obs, nearest to header
    result <- trimws(tmp[tail(idx,1)])
    # Return
    gsub("\\s+", " ", result)
  }

  xsub <- function(pattern, replacement, x){
    gsub(pattern, replacement, x, ignore.case = TRUE, perl = TRUE)
  }

  lineSplit <- function(line, x){
    # Auto-correction for shifts
    # 1. Get spaces (good spliting positions)
    spaces <- c(gregexpr(" ", line)[[1]], 1000000L)
    # 2. Find badly proposed splitting parameters
    idx <- which(!x$Split %in% spaces)
    # 3. Replace bad with good splitting position
    for (i in idx) {
      # Where are the next best spaces
      minLeft <- tail(spaces[x$Split[i] > spaces],1)  # Old
      minRight <- head(spaces[x$Split[i] < spaces],1) # New 2021-08-30

      if (length(minLeft)  == 0)  minLeft <- minRight
      if (length(minRight) == 0) minRight <- minLeft

      # Which one demands for the tiniest change in position
      # (x$Split[i] - minLeft) < abs(x$Split[i] - minRight)
      # is same as
      # 2 * x$Split[i] < (minRight + minLeft)
      if (2 * x$Split[i] < (minRight + minLeft))
        x$Split[i] <- minLeft
      else
        x$Split[i] <- minRight
      # Adapt raster's ending position
      x$sPo[i+1] <- x$Split[i] + 1
    }

    # Apply splits on line
    apply(x, 1, function(x)  trimws(substring(line[[1]], x[1], x[4])[[1]]))
  }

  getRaster <- function(header){
    # Structure of Header line
    m <- gregexpr("[A-Za-z_]+", header)
    hh <- regmatches(header,m)[[1]]

    # Column with Description
    if (verbose && length(grep(pDescription,header, ignore.case = TRUE)) == 0){
      message("Missing Description Column!")
      cat("Focus are the transactions. \n")
      cat("Line in between are interpreted as wrapped text. \n")
      cat("Please provide column name where to add the multiple line text. \n")
      browser()
    }

    # Raster with structure
    sPo <- as.vector(m[[1]])
    l <- attr(m[[1]],"match.length")
    ePo <- sPo + l - 1

    result <- data.frame(sPo = sPo - 1 ,
                         ePo = ePo,
                         Length = l,
                         Split = c(sPo,1000000L+2)[-1] -2 )

    if(length(hh) == 0) return(NULL)

    rownames(result) <- hh

    # Keep beginning
    # paste0(rep(" ", sPo[1] - 1), collapse = "") == substr(header,1,sPo[1] - 1)
    if(gsub(" ", "", substr(header, 1, sPo[1]-1)) == "")
      result[1,1] <- 1


    # Return
    result
  }

  getReminder <- function(result){

    # Helper functions
    pasteX <- function(x) paste0(ss[x[1]:x[2]], collapse = "")
    cleanT <- function(x) gsub("\\t", "", x)
    cleanS <- function(x) gsub("\\s+", " ", x)

    result %>%
      # Get Start + End
      mutate(
        NumberOfLines = ifelse(is.na(NumberOfLines),1,NumberOfLines),
        AddFromLine =  ifelse(NumberOfLines == 1, 0, Line + 1 ),
        AddToLine = ifelse(NumberOfLines == 1, 0, Line + NumberOfLines - 1)
      ) %>%
      # Select only From:To
      select(AddFromLine, AddToLine) %>%
      # Apply function
      apply(., 1, pasteX) %>%
      cleanT %>% cleanS %>% trimws()
  }

  # Split Lines of Page
  if (length(page) == 1){
    ss <- strsplit(page, split = "\n")[[1]]
  } else {
    ss <- page
    page <- paste0(ss, collapse = "\n")
  }


  # Pre-Processing
  # Replace spaces in header names
  # In order they get not mixed with vertical line splits
  for(pre in preProcess){
    ss <- xsub(pre[1], pre[2], ss)
  }

  # Find Header and Footer (first)
  idxHeader <- grep(pHeader,ss, ignore.case = TRUE, perl = TRUE)[1]
  idxFooter <- grep(pFooter,ss, ignore.case = TRUE, perl = TRUE)[1]

  if (length(idxHeader) == 0 | is.na(idxHeader))
    return(NULL)
  else
    header <- ss[idxHeader]

  if(length(idxFooter) == 0)
    idxFooter <- length(ss)

  # Print Header
  message("Header: ", header)

  # Get Raster
  raster <- getRaster(header)

  # Filter for Transactions
  # idxTransaction <- rep(TRUE, length(ss))
  idxTransaction <- grepl(pTransaction, ss)

  if (length(idxTransaction) == 0) return(NULL)

  # Remove falsely indexed
  idxTransaction[1:idxHeader] <- FALSE
  transactions <- ss[idxTransaction]

  # Apply Line Splits on Transactions only
  dd <- lapply(transactions, function(x) lineSplit(x,raster))

  # Tidy data
  result <- do.call(rbind, dd) %>%
            as_tibble() %>%
            # Post Processing
            mutate(
              # Where are the transactions?
              Line = which(idxTransaction),
              # How many lines come with each transaction?
              NumberOfLines = c(diff(Line), idxFooter - last(Line))
              ) %>%
            # Reminder Text
            mutate(
              REFERENCE = getReminder((.)),
              CURRENCY = findCurrency(page),
              ACCOUNT = findAccount(ss),
              DATES = findDate(ss)
            )


  # Post-Processing
  if (!is.null(postProcess)) stop("Not Implmentated Exception!")



  # Return
  result
}
