filter_casenumber <- function(x){
  gsub(".*(\\[[0-9]{2}?\\]).*", "\\1", x)
}
