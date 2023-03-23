#' Write PDF Text from pdftext
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
#' write_pdf_txt(fls, path)
write_pdf_txt <- function(fls, dest,
                          subfolder = "RSearch/pdftxt",
                          max_file_length = 250){

  if(file_check(fls, max_file_length)){

    # BEGIN
    # Process only files in path
    write_fun_txt(fls, dest, subfolder = subfolder, pdf_fun = pdf_txt)
    # END

  } else {
    stop("Contains too long file names")
  }

}
