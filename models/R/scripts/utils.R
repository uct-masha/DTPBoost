#function to read in all sheets in a workbook
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

LEVEL <<-  list(
  'trace',
  'info',
  'error',
  'fatal',
  'warn',
  'debug'
)
names(LEVEL) <- toupper(LEVEL)
# https://stackoverflow.com/questions/7307987/logging-current-function-name
curfnfinder<-function(skipframes=0, skipnames="(FUN)|(.+apply)|(replicate)",
                      retIfNone="Not in function", retStack=FALSE, extraPrefPerLevel="\t")
{
  prefix<-sapply(3 + skipframes+1:sys.nframe(), function(i){
    currv<-sys.call(sys.parent(n=i))[[1]]
    return(currv)
  })
  prefix[grep(skipnames, prefix)] <- NULL
  prefix<-gsub("function[ \t]*\\(.*", "do.call", prefix)
  if(length(prefix)==0)
  {
    return(retIfNone)
  }
  else if(retStack)
  {
    return(paste(rev(prefix), collapse = "|"))
  }
  else
  {
    retval<-as.character(unlist(prefix[1]))
    if(length(prefix) > 1)
    {
      retval<-paste(paste(rep(extraPrefPerLevel, length(prefix) - 1), collapse=""), retval, sep="")
    }
    return(retval)
  }
}
makeLogger <- function(default_level='trace') {
  LOG <<- function(msg, level=default_level, .envir=parent.frame()) {
    FROM <- tryCatch(this.path::this.path(),error=function(e) '.')
    FUNC <- stringr::str_trim(curfnfinder(skipframes = 1))
    msg <- as.character(glue::glue(as.character(msg), .envir=.envir))
    PRE <- paste0('(',
                  crayon::italic(paste0(fs::path_rel(FROM), ':', FUNC)),
                  ') ')
    MSG <- paste0(PRE, msg)
    switch (tolower(level),
            'trace' = rlog::log_trace,
            'info'  = rlog::log_info,
            'error' = rlog::log_error,
            'fatal' = rlog::log_fatal,
            'warn'  = rlog::log_warn,
            'debug' = rlog::log_debug
    )(MSG)
  }
  LOG
}
setLogLevel <- function(level) {
  Sys.setenv(LOG_LEVEL=toupper(level))
}
