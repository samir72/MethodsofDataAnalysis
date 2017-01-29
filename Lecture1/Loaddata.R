Loaddata <- function(file)
{
#  browser()
    ## Read the csv file
  Dataload <- read.csv(file, header = TRUE,stringsAsFactors = FALSE)
  return(Dataload)
}


