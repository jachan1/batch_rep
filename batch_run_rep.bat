:: This needs to be copied into the code folder with the Rmd file to be run
:: Rscript should be given r batch script location
:: named arguments and defaults are below. All arguments must be named and come after --args
:: note - outloc must be relative to Rmd file
:: archive should be set to 0 if don't want old reports to be saved in separate folder.
:: portrait=T rmdfile=report.Rmd outloc=/../docs prefix=report_ wkloc=C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe archive=1

pushd "%~dp0"
Rscript -e "source('https://raw.githubusercontent.com/jachan1/batch_rep/master/UA_rep_extra.R')" --args rmdfile=analysis.Rmd prefix=Report_
popd
PAUSE
