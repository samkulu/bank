file_rename <- function(fls,
                        pattern = "_align.txt",
                        replacement = ".txt"){

  fls2 <- gsub(pattern, replacement, fls)
  idx <- !fls2 %in% fls

  # if(length(which(!idx)) > 0)

  file.rename(fls[idx], fls2[idx])
}
