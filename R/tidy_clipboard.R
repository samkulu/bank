#' Tidy Clipboard
#'
#' @return
#' @export
#'
#' @examples
#' tidy_clipboard()
tidy_clipboard <- function(){
  readClipboard() %>%
    trimws() %>%
    as_tibble() %>%
    filter(!grepl("Balance|^$|^\\.$", value)) %>%
    distinct() %>%
    arrange(value) %>%
    pull(value) %>%
    writeClipboard()
}
