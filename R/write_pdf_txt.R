#' Write PDF Text
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
write_pdf_txt <- function(fls, dest, subfolder = "RSearch/pdftxt"){

  # Process only files in path
  write_fun_txt(fls, dest, subfolder = subfolder, pdf_fun = pdf_txt)

}
