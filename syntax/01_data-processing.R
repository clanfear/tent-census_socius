# This script runs everything from scratch.

processing_scripts <- list.files("./syntax/processing/", full.names = TRUE)
for(script in processing_scripts){
  message(paste0(Sys.time(), ": Running '", script, "'"))
  source(script)
}
