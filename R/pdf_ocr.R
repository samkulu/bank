#' OCR text extraction
#'
#' Perform OCR text extraction. This requires you have the tesseract package.
#'
#' @param pdf
#' @param pages
#' @param lang passed to tesseract to specify the languge of the engine.
#' @param dpi resolution to render image that is passed to tesseract::ocr.
#'
#' @return
#' @export
#'
#' @examples
pdf_ocr <- function(pdf, pages = NULL,
                    opw = "", upw = "",
                    lang = "eng", dpi = 600){

  # https://www.rdocumentation.org/packages/pdftools/versions/3.2.1/topics/pdf_ocr_text
  pdftools::pdf_ocr_text(pdf, pages,
                         opw = opw,
                         upw = upw,
                         language = lang,
                         dpi = dpi)
}
