:: This needs to be copied into the code folder with the R file to be run
:: named arguments and defaults are below. All arguments must be named and come after --args
:: sys.setenv command is needed to locate pandoc correctly. 
:: if pandoc is in a different location in your system this should be changed. 
:: note - outloc must be relative to Rmd file
:: When changing the _loc variables don't add spaces around the = sign


pushd "%~dp0"pushd "%~dp0"
set "pandoc_loc=C:/Program Files/RStudio/bin/pandoc"
set "batch_loc=H:/BG/R/git_temps/batch_rep/batch_run_dprep.R"
Rscript -e "Sys.setenv(RSTUDIO_PANDOC='%pandoc_loc%');source('%batch_loc%')" ^
	--args ^
	rfile=alsss_dataprep.R ^
	prefix=cr ^
	outloc="/" ^
	archive=1 ^
	verbos_objects=0

popd
PAUSE
