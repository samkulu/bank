file_info <- function(fls){
  tibble::tibble(
            ACC = gsub(".*(\\[\\d{1,3}\\]).*", "\\1", fls),
            PDF = basename(dirname(fls)),
            PAGE = basename(fls),
            KEY = paste0(PDF, "/", PAGE),
            FULLNAME = fls,
            DEST = file_path_abs(fls),
            FILE = file_path_sub(fls)
          )
}
