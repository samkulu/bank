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
  write_fun_txt(fls, dest, subfolder = subfolder, pdf_fun = pdf_ocr)

}
