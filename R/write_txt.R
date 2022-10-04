write_txt <- function(fi, text){
  # Create directory
  fo <- dirname(fi)
  if (!dir.exists(fo)) dir.create(fo, recursive = T)

  # Export txt into File with name fi
  writeLines(text, con = fi, sep = "\n", useBytes = FALSE)
}
