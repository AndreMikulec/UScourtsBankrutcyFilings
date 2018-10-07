#' garantee a date is a date or and empty date
#'
#' @param date date
#' @return date
#' @examples
#' \dontrun{
#' # > initDate(date = NULL)
#' # Date of length 0
#' }
#' @export
initDate <- function(date = NULL) {
  tryCatchLog::tryCatchLog({
  if(is.null(date)) {
    date <- zoo::as.Date(0L)[0]
  }
  date

})}


#' garantee a passed xts object or a zero length xts object
#'
#' @param xTs xts object
#' @return xts object
#' @examples
#' \dontrun{
#' # > initXts(xTs = NULL)
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' # Date of length 0
#' #
#' # > initXts(zoo::as.Date(0)[0])
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' # Date of length 0
#' }
#' @export
initXts <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(is.null(xTs)) {
    # empty xts
    xTs <-  xts(, zoo::as.Date(0)[0])
  } else if (is.timeBased(xTs)) {
    xTs <-  xts(, xTs)
  } else {}
  xTs

})}


#' garentees return value value is a 'matrix of at least one dimension'or NULL
#'
#' handles the edge case of coredata(<vector of element of size 1 or size 0>)
#' meant specifically to input empty coredata data into xts(, index)
#' xts(,index) only accepts a non zero-dimension matrix or a NULL

#' @param xTs xts object
#' @return matrix
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > Coredata(NULL)
#' # NULL
#' #
#' # > Coredata(numeric(0))
#' # NULL
#' #
#' # > Coredata(11:13)
#' #      [,1]
#' # [1,]   11
#' # [2,]   12
#' # [3,]   13
#' }
#' @export
Coredata <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  cd <- coredata(xTs)
  cd <- as.matrix(cd)

  if(is.null(cd)) return(NULL)
  if((NROW(cd) == 0) || (NCOL(cd) == 0)) return(NULL)

  if(length(dim(cd)) > 2) stop("coredata is greater than two dimensions")

  if(is.matrix(cd)) {
    return(cd)
  } else {
    stop("could not convert coredata to a matrix")
  }
  invisible()

  })}





#' sets the enviroment
#'
#' space-saver - meant to be used at the beginning of a function
#'
#' @param init list with names entries of alernate/other options
#' @param envir where to return the options
#' @return environment is set
#' @examples
#' \dontrun{
#' # > options(max.print=88888L)
#' # initEnv()
#' # > getOption("max.print")
#' # [1] 99999
#' }
#' @export
initEnv <- function(init = NULL, envir = parent.frame()) {
  tryCatchLog::tryCatchLog({
  require(xts)

  # debugging
  # 1st round (where the error occurs)
  # tryCatchLog
  # 2nd round (env variables around where the error occurred)
  # options(error = quote(dump.frames("testdump", TRUE)))
  # utils/html/debugger.html
  # 3rd round
  # browser(expr = { <where condition> }
  eval(parse(text="assign(\"env\", environment())"), envir = parent.frame())

  assign("parse_expr",     rlang::parse_expr, envir = environment())
  assign("env_parent",     rlang::env_parent,     envir = environment())
  assign("eval_tidy",      rlang::eval_tidy,      envir = environment())

  # action <- parse_expr("assign(\"env\", environment())")
  # does not work
  # eval_tidy(action, env = parent.frame())

  # convenience
  assign("%>%",     magrittr::`%>%`,     envir = envir)
  assign("as.Date",      zoo::as.Date,   envir = envir)
  assign("str_c",    stringr::str_c,     envir = envir)
  assign("read_xml",    xml2::read_xml,  envir = envir)
  assign("read_html",   xml2::read_html, envir = envir)
  assign("map",        purrr::map,       envir = envir)
  assign("html_nodes", rvest::html_nodes, envir = envir)

  ops <- options()

  # tryCatchLog: what level to activate
  futile.logger::flog.threshold(futile.logger::ERROR)

  options(warn=2L)
  options(width=10000L) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits=if(is.null(init[["digits"]])) { 22L } else {init[["digits"]]})
  options(max.print=99999L)
  options(scipen=255L) # Try these = width
  #
  assign("ops", ops, envir = envir)

  #correct for TZ
  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }
  #
  assign("oldtz", oldtz, envir = envir)

  invisible()

})}


#' sets the printing enviroment
#'
#' space-saver - meant to be used at the beginning of a function
#'
#' @return printing environment is set
#' @examples
#' \dontrun{
#' # initPrintEnv()
#' }
#' @export
initPrintEnv <- function() {
  tryCatchLog::tryCatchLog({
  init <- list()
  init[["digits"]] <- 5L
  initEnv(init = init)
  assign("ops"  , ops,   envir = parent.frame())
  assign("oldtz", oldtz, envir = parent.frame())

  invisible()

})}


#' unsets the enviroment
#'
#' space-saver - meant to be used at the beginning of a function
#'
#' @return environment is un-set
#' @examples
#' \dontrun{
#' # > uninitEnv()
#' # getOption("digits")
#' # [1] 5
#' }
#' @export
uninitEnv <- function() {
  tryCatchLog::tryCatchLog({
  Sys.setenv(TZ=get("oldtz", envir = parent.frame()))
  options(get("ops", envir = parent.frame()))

  invisible()

})}


#' @export
publishDates <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

    #                                                   # last day of the quarter/first day of next quarter
    #                                                   # tries to get 'that days publishing'
    Published <- seq(from = as.Date("2001/04/01"), to = (Sys.Date() + 1) , by = "quarter") - 1
    Published

})}


#' @export
formattedDateStrings <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})


  publishDates() %>%
    format("%Y/%m/%d") ->
      Formatted
  Formatted

})}


#' @export
pageURLs <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  URLs <- str_c("http://www.uscourts.gov/statistics/table/f-2-three-months/bankruptcy-filings/", formattedDateStrings())
  as.list(URLs)

})}



#' reads body of resulting URL and returns a ("xml_document" "xml_node")
#'
#' @export
readHTML         <- function(x = NULL, ...) UseMethod("readHTML")
#' @rdname readHTML
#' @export
readHTML.default <- function(x = NULL) stop()
#' @rdname readHTML
#' @param x character vector of strings that are urls
#' @return list of ("xml_document" "xml_node")
#' @export
readHTML.character <- function(x = NULL, verbose = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  x %>% map( ~ {

    if(is.null(verbose)) { verbose <- TRUE } else { verbose <- FALSE }

    if(verbose == TRUE) message(str_c("Starting page read: ", .x))
    Read <- try(read_html(x = .x), silent = TRUE)
    if(verbose == TRUE) message(str_c("  Ending page read: ", .x))

                                            # "xml_document" "xml_node" like-a-constructor
    if(inherits(Read, "try-error")) Read <- read_xml("<html></html>")
    Read
    # "xml_document" "xml_node"

  })

})}
#' @rdname readHTML
#' @param x list of single element character vectors of url strings
#' @return list of ("xml_document" "xml_node")
#' @export
readHTML.list <- function(x = NULL, verbose = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  readHTML(do.call(c,x), verbose = verbose)

})}



#' html content, reads one
#'
#' @export
htmlContent         <- function(x, ...) UseMethod("htmlContent")
#' @rdname htmlContent
#' @export
htmlContent.default <- function(x) stop()
#' @rdname htmlContent
#' @param x xml_document or xml_node
#' @return one large characer vector element
#' @export
htmlContent.xml_node <- function(x = NULL, verbose = NULL) {
  tryCatchLog::tryCatchLog({    # "xml_document" "xml_node"
  initEnv();on.exit({uninitEnv()})

  # "xml_nodeset"
  Content <- try(html_nodes(x, "#content"), silent = TRUE)
                                                # "xml_nodeset" like constructor
  if(inherits(Content, "try-error")) Content <- html_nodes(read_xml("<html></html>"), "#content")

  Content <- as.character(Content)
  Content

})}


#' web pages
#'
#' @export
webPages <- function(x = NULL, ...) UseMethod("webPages")
#' @rdname webPages
#' @export
webPages.default <- function(x = NULL) stop()
#' @rdname webPages
#' @param  x url of a character vector
#' @return list of large "character" elements
#' @export
webPages.character <- function(x = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  as.list(x) %>% webPages

})}
#' @rdname webPages
#' @param x list containing (xml_document or xml_node)
#' @return list containing large "character" elements
#' @export
webPages.list <- function(x = NULL, verbose = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  x %>%
      map( ~ { Sys.sleep(1.0); htmlContent(x = .x) }) -> Pages
  Pages
})}


# pageURLs()[1:3] %>% readHTML -> out
# webPages(out) -> out2
# str(out2)
# xor just
# pageURLs()[1:3] %>% readHTML %>% webPages -> out3

# LEFT_OFF
# LINKS

#' web pages
#'
#' @export
dataLinks <- function(x = NULL, ...) UseMethod("dataLinks")
#' @rdname dataLinks
#' @export
dataLinks.default <- function(x = NULL) stop()
#' @rdname dataLinks
#' @param  x character vector of Content (means HTML text)
#' @return list of URL links
#' @export
dataLinks.character <- function(x = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  as.list(x) %>% dataLinks

})}
#' @rdname dataLinks
#' @param x list containing character vectors of Content (means HTML text)
#' @return list of URL links
#' @export
dataLinks.list <- function(x = NULL, verbose = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  x %>%
      map( ~ {
        # .x
        # LEFT_OFF
      }) -> Links
  Links
})}





