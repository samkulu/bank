#' Create and Write OCR Text
#'
#'
#'
#' @param fls
#' @param dest
#' @param subfolder
#'
#' @return
#' @export
#'
#' @examples
write_ocr_txt <- function(fls, dest, subfolder = "RSearch/tesseract"){
  # Process only files in path
  stopifnot(all(substring(fls, 1, nchar(dest)) == dest))

  # Throw Implementation Exception
  # pdf_ocr needs implementation workaround
  # for filenames with special characters
  stopifnot(fls == file_name_ascii(fls))

  # Files in subfolder
  childs <- substring(fls,nchar(dest) + 1)

  dest2 <- file.path(dest, subfolder, childs)
  dest2 <- gsub("\\\\","\\/",dest2)
  dest2 <- gsub("\\/\\/","\\/",dest2)
  dest2 <- gsub(".pdf$", " [pdf]", dest2)

  num <- length(fls)

  browser()
  # Process files
  result <- list()
  for(i in 1:length(fls)){
    # Show file progress
    message(basename(fls[i]))

    ### BEGIN from getting Text
    txt <- pdf_ocr(fls[i]) # pdf_txt(fls[i])
    ### END

    N <- length(txt)
    result[[i]]

    # Filenames
    p <- gsub("%N%", nchar(N), "page_%0%N%d.txt")
    pages <- sprintf(p,1:N)
    pages <- file.path(dest2[i],pages)

    sapply(1:N, function(x) write_txt(pages[x], txt[x]))
  }


  # Return
  result
}
