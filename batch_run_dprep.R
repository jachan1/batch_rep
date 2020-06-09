## arg function ------ 
## courtesy of https://cwcode.wordpress.com/2013/04/16/the-joys-of-rscript/
##' commandArgs parsing
##' 
##' return a named list of command line arguments
##'
##' Usage:
##' call the R script thus
##'   ./myfile.R --args myarg=something
##' or
##'   R CMD BATCH --args myarg=something myfile.R
##'
##' Then in R do
##'   myargs <- getArgs()
##' and myargs will be a named list
##' > str(myargs)
##' List of 2
##' $ file : chr "myfile.R"
##' $ myarg: chr "something"
##'
##' @title getArgs
##' @param verbose print verbage to screen 
##' @param defaults a named list of defaults, optional
##' @return a named list
##' @author Chris Wallace
getArgs <- function(verbose=FALSE, defaults=NULL) {
  myargs <- gsub("^--","",commandArgs(TRUE))
  setopts <- !grepl("=",myargs)
  if(any(setopts))
    myargs[setopts] <- paste(myargs[setopts],"=notset",sep="")
  myargs.list <- strsplit(myargs,"=")
  myargs <- lapply(myargs.list,"[[",2 )
  names(myargs) <- lapply(myargs.list, "[[", 1)
  
  ## logicals
  if(any(setopts))
    myargs[setopts] <- TRUE
  
  ## defaults
  if(!is.null(defaults)) {
    defs.needed <- setdiff(names(defaults), names(myargs))
    if(length(defs.needed)) {
      myargs[ defs.needed ] <- defaults[ defs.needed ]
    }
  }
  
  ## verbage
  if(verbose) {
    cat("read",length(myargs),"named args:\n")
    print(myargs)
  }
  myargs
}

### code ------

options(repos=structure(c(CRAN="https://cloud.r-project.org")))

if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if(!"dplyr" %in% rownames(installed.packages())) install.packages("dplyr")
require(dplyr)

bargs <- getArgs(defaults=list(rfile="report.R", outloc="/", prefix="cr", 
                               archive=1, verbose_objects=0))


full_outloc <- paste0(getwd(), "/", bargs$outloc)


# bargs$rfile <- "p_dataprep_dataprep.R"
# bargs$prefix <-  "cr"
# full_outloc <- "Z:/ATN/AIR-P STUDIES/RFA 09 Studies/PETRA Expansion/Analyses/dataprep/code/"
# setwd(full_outloc)
# bargs$archive <- 1
# bargs$verbos_objects <- 0


today <- format(Sys.Date(), "%Y%m%d")

todays_files <- list.files(full_outloc, pattern=paste0("^", gsub(".R", "", bargs$rfile), "_", bargs$prefix, today, ".*txt$"))

if(length(todays_files) == 0) {
  txt_file <- file.path(full_outloc, paste0(gsub(".R", "", bargs$rfile), "_", bargs$prefix, today, ".txt"))
} else {
  file_n <- as.numeric(gsub(paste0(paste0(gsub(".R", "", bargs$rfile), "_", bargs$prefix, today), "_|.txt"), "",todays_files))
  file_n <- ifelse(is.na(file_n), 0, file_n)
  txt_file <- file.path(full_outloc, paste0(gsub(".R", "", bargs$rfile), "_", bargs$prefix, today, "_", max(file_n) + 1, ".txt"))
}

old_files <- grep(sprintf("^%s", bargs$prefix), list.files(full_outloc, pattern=".txt"), ignore.case = T, value=T)
if(length(old_files) > 0 & bargs$archive==1){
  old_folder <- paste0(full_outloc, "/", bargs$prefix, "previous_versions")
  if(!old_folder %in% list.dirs(full_outloc)){
    dir.create(old_folder)
  }
  sapply(old_files, function(x){
    file.rename(from=paste(full_outloc, x, sep="/"), to=paste(old_folder, x, sep="/"))
  })
}

con <- file(txt_file, open="wt")
sink(con, append=T)
sink(con, append=T, type="message")

src_check <- tryCatch(source(bargs$rfile, echo=T, spaced=T, max.deparse.length=10000),
                      error = function(e) e)

if("error" %in% class(src_check)){
  
  cat("\n\n______________ERROR INFO___________\n\n")
  
  print(sprintf("Source produced an error: %s", src_check))

}

object_details_df <-data.frame(object= ls(), stringsAsFactors = F) %>% 
  group_by(object) %>% 
  mutate(size_KB = format(object.size(get(object)), units="KB"),
         length = unlist(lapply(object, function(x) length(get(x)))),
         nrows = unlist(lapply(object, function(x) ifelse(is.null(dim(get(x))), NA, dim(get(x))[1]))),
         ncols = unlist(lapply(object, function(x) ifelse(is.null(dim(get(x))), NA, dim(get(x))[2]))),
         start_paste = unlist(lapply(object, function(x) tryCatch(paste0(class(get(x)), collapse=", "), error= function(e) NA))))

write_fwf = function(dt, width, 
                     justify = "l", replace_na = "NA") {
  fct_col = which(sapply(dt, is.factor))
  dt_chr <- dt %>% ungroup()
  if (length(fct_col) > 0) {
    for (i in fct_col) {
      dt_chr[,i] <- as.character(dt_chr[,i])
    }
  }
  dt_chr <- dt_chr %>% 
    mutate_all(as.character)
  dt_chr[is.na(dt_chr)] = replace_na
  n_col = ncol(dt_chr)
  justify = unlist(strsplit(justify, ""))
  justify = as.character(factor(justify, c("l", "r"), c("-", "")))
  if (n_col != 1) {
    if (length(width) == 1) width = rep(width, n_col)
    if (length(justify) == 1) justify = rep(justify, n_col)
  }
  sptf_fmt = paste0(
    paste0("%", justify, width, "s"), collapse = ""
  )
  tbl_content = do.call(sprintf, c(fmt = sptf_fmt, dt_chr))
  tbl_header = do.call(sprintf, c(list(sptf_fmt), names(dt_chr)))
  out = c(tbl_header, tbl_content)
  writeLines(out)
}

cat("\n\n______________All Objects Created___________\n\n")

write_fwf(object_details_df, width=c(25, 15, 8, 8, 8, 50))

if(bargs$verbose_objects > 0){
  
  lapply(ls(), function(x) str(get(x)))
}

cat("\n\n______________Full R Code Run___________\n\n")
tryCatch(writeLines(readLines(bargs$rfile)), 
         error=function(e) sprintf("Error: Could not print code\n\n%s", e))

cat("\n\n______________Full Session Info___________\n\n")

print(sessionInfo())
sink(type="message")
sink()
close(con)
