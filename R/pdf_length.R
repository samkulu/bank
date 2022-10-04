pdf_length <- function(pdf){
  if(length(pdf) > 1){
    result <- sapply(pdf, function(y) bankdata::pdf_length(y))
    # browser()
    return(result)
  } else {
    # Need of implementation workaround for filenames with special characters?
    if(pdf == file_name_ascii(pdf)){
      # w/o Workaround
      return(try(pdftools::pdf_length(pdf)))
    } else{
      # With Workaround
      temp <- tempfile()
      if(file.copy(pdf, temp)){
        # Ususally everything works fines
        on.exit(if(file.exists(temp)) try(file.remove(temp)))

        result <- tryCatch({
                    pdftools::pdf_length(temp)
                  }, warning = function(w) {
                    # warning-handler-code
                    message("File warning:")
                    cat(pdf, " \n")
                    paste0(as.character(w$call)[1], ": ", trimws(strsplit(basename(w$message),split=":")[[1]][2]))
                  }, error = function(e) {
                    # error-handler-code
                    message("File with corruption:")
                    cat(pdf, " \n")
                    paste0(as.character(e$call)[1], ": ", trimws(strsplit(basename(e$message),split=":")[[1]][2]))
                    # browser()
                  }, finally = {
                    # cleanup-code
                  })

        return(result)
      } else {
        # In rare cases you have maybe
        # - Named folders with ".pdf" in the end
        # - Files with copy protection
        # - Filename that contains .pdf but is actually a .jpg
        message("File with copy protection:")
        cat(pdf, " \n")
        # browser()
      }

    }

  }
}
