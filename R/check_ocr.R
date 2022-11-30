check_ocr <- function(path){

  fls <- list.files(path, recursive = TRUE, full.names = TRUE)

  idx <- grep("RSearch", fls)


  flsData <- fls[-idx]
  flsForensic <- fls[idx]


  ## Provided Data files
  # PDF-Files
  flsPDF <- flsData[grepl("\\.pdf", flsData)]

  # Non-PDF Files
  flsExtra <- flsData[!grepl("\\.pdf", flsData)]


  ## OCR files
  flsOCR <- flsForensic[grepl("(pdftxt|tesseract).*\\.txt", flsForensic)]

  # Check Case numbers
  casePDF <- filter_casenumber(flsPDF)
  caseOCR <- filter_casenumber(flsOCR)

  nums <- unique(casePDF)
  missingCASE <- nums[!nums %in% unique(caseOCR)]


  # Creation Time
  infoPDF <- file.info(flsPDF)
  infoOCR <- file.info(flsOCR)

  xx <- pdf_length(flsPDF)

  checkPDF <- tibble(
                FILE = flsPDF,
                MESSAGE = as.character(xx),
                NUM = suppressWarnings(as.numeric(MESSAGE))
              ) %>%
            # More Info
            relocate(NUM, .before = MESSAGE)


  # checkOCR <- tibble(
  #               FILE = flsOCR,
  #
  #             )


  # When was update
  updPDF <- tibble(CASE=casePDF,UPDATE=infoPDF$ctime,FILE=flsPDF)
  updOCR <- tibble(CASE=caseOCR,UPDATE=infoOCR$ctime,FILE=flsOCR)



  result <- left_join(
                updPDF %>% group_by(CASE) %>% summarise(UPDATEPDF=max(UPDATE)),
                updOCR %>% group_by(CASE) %>% summarise(UPDATEOCR=max(UPDATE)),
                by = "CASE"
             ) %>%
             # Check if OCR is Up To Date
             mutate(CHECK = UPDATEPDF < UPDATEOCR )

  data <- list(OVERVIEW = result,
               infoPDF = infoPDF %>% mutate(file = flsPDF) %>% as_tibble(),
               infoOCR = infoOCR %>% mutate(file = flsOCR) %>% as_tibble(),
               checkPDF = checkPDF,
               PATH = path %>% as_tibble())

  attr(data, "Name") <- paste0(format(Sys.Date(),"%Y%m%d"),"_CheckUpdate")

  write_xl(data)
}
