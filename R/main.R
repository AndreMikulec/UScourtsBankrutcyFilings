#' runs the main
#'
#' @return xts bankruptcy filings
#' @examples
#' \dontrun{
#' EXAMPLE
#' # EXAMPLE
#' }
#' @export
returnsBankrutcyFilings <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # A WORK IN PROGRESS
  pageURLs()[1:3] %>% readHTML -> out
  webPages(out) -> out2
  str(out2)
  # xor just
  # pageURLs()[1:3] %>% readHTML %>% webPages -> out3


  invisible()

})}
