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

if(!"rmarkdown" %in% rownames(installed.packages())) install.packages("rmarkdown")
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if(!"glue" %in% rownames(installed.packages())) install.packages("glue")
require(glue)

bargs <- getArgs(defaults=list(portrait=T, rmdfile="report.Rmd", outloc="/../docs", prefix="report_", 
                               wkloc="C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe",
                               archive=1))

if(bargs$portrait) {
  ortn = "portrait"
} else ortn = "landscape"

full_outloc <- paste0(getwd(), "/", bargs$outloc)

today <- format(Sys.Date(), "%Y%m%d")
today_time <- paste(format(Sys.Date(), "%Y-%m-%d"), format(Sys.time(),"%H:%M"))
html_file <- file.path(full_outloc, paste0(bargs$prefix, today, ".html"))
pdf_file <- file.path(full_outloc, paste0(bargs$prefix, today, ".pdf"))

old_files <- grep(sprintf("^%s", bargs$prefix), list.files(full_outloc, pattern=".html|.pdf"), ignore.case = T, value=T)
if(length(old_files) > 0 & bargs$archive==1){
  old_folder <- paste0(full_outloc, "/", bargs$prefix, "previous_versions")
  if(!old_folder %in% list.dirs(full_outloc)){
    dir.create(old_folder)
  }
  sapply(old_files, function(x){
    file.rename(from=paste(full_outloc, x, sep="/"), to=paste(old_folder, x, sep="/"))
  })
}

con <- file(paste0(getwd(), "/", substr(bargs$rmdfile, 1, nchar(bargs$rmdfile)-4), ".txt"), open="wt")
sink(con, append=T)
sink(con, append=T, type="message")
rmd_check <- tryCatch(rmarkdown::render(bargs$rmdfile, 
                                        params=list(out=full_outloc,
                                                    dir=getwd())),
                      error = function(e) e)
if("error" %in% class(rmd_check)){
  print(sprintf("Render produced an error: %s", rmd_check))
} else {
  file.rename(rmd_check, html_file)
}
                        
cat("\n\n______________Full Rmd Code Run___________\n\n")
tryCatch(writeLines(readLines(bargs$rmdfile)), 
         error=function(e) sprintf("Error: Could not print code\n\n%s", e))

cat("\n\n______________Full Session Info___________\n\n")
print(sessionInfo())
sink(type="message")
sink()
close(con)
if(rmd_check != "Rmd file could not be run"){
  sys_str <- glue("\"{bargs$wkloc}\" -O {ortn} 
                  -s Letter -L 10 -R 10 -T 15 -B 12 --zoom 1.3 \"{html_file}\" 
                  --header-center \" \" 
                  --footer-right [page]/[topage] 
                  --footer-left \"Report Generated on {today_time}\" 
                  --footer-font-size 9
                  --footer-spacing 4
                  --disable-javascript \"{pdf_file}\"")
  tryCatch(system(gsub("\n", " ", sys_str)),
           error = function(e) print("PDF file could not be created"))
}

