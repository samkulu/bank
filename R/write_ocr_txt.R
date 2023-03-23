#' Write OCR Text from tesseract engine
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
#' write_ocr_txt(fls, path)
write_ocr_txt <- function(fls, dest,
                          subfolder = "RSearch/tesseract",
                          max_file_length = 250){

  if(file_check(fls, max_file_length)){

    # BEGIN
    # Process only files in path
    write_fun_txt(fls, dest, subfolder = subfolder, pdf_fun = pdf_ocr)
    # END

  } else {
    stop("Contains too long file names")
  }

}
