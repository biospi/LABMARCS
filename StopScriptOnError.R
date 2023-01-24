#from https://stackoverflow.com/questions/42256291/make-execution-stop-on-error-in-rstudio-interactive-r-session

# Define our 'eat_input' function to prevent output of subsequent, not-run input:
eat_input=function(){
  cat("\033[1A")
  while((x=readline())!='')cat("\033[1A")
}

# Set the 'error' option to execute our pause function:

options(error=eat_input)

# To reset to standard behavior run
# options(error=NULL)
