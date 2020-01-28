:: This needs to be copied into the code folder with the Rmd file to be run
:: Rscript should be given r batch script location
:: named arguments and defaults are below. All arguments must be named and come after --args
:: note - outloc must be relative to Rmd file
:: archive should be set to 0 if don't want old reports to be saved in separate folder.
:: portrait=T rmdfile=report.Rmd outloc=/../docs prefix=report_ wkloc=C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe archive=1

pushd "%~dp0"
set "pandoc_loc=C:/Program Files/RStudio/bin/pandoc"
set "batch_loc=https://raw.githubusercontent.com/jachan1/batch_rep/master/UA_rep_extra.R"
Rscript -e "Sys.setenv(RSTUDIO_PANDOC='%pandoc_loc%');source('%batch_loc%')" ^
	--args ^
	rmdfile=analysis.Rmd ^
	prefix=Report_ ^
	outloc="/../docs" ^
	archive=1 ^
	portrait=T ^
	wkloc="C:/Program Files/wkhtmltopdf/bin/wkhtmltopdf.exe"

popd
PAUSE
