Clean <- function(Data1)
{
# Remove all rows/columns with special characters and provide me the count of rows/colmuns deleted for each instance.
SpecialChar <- c('?','#','%')# List of special characters checked in this function.
numspecial <- length(SpecialChar)
Badrowcount <- FALSE
Badcolcount <- FALSE
for (j in 1:numspecial)
{
  #cat("Cleansing character ", SpecialChar[j], "\n" )
  NumCols <- ncol(Data1) # Get number of columns for the loop.
  NumRows <- nrow(Data1) # Get the number of rows in the dataset.
  i=0
  while (i < NumCols)
  {
    i=i+1
    rowcount = nrow(subset(Data1, trimws(Data1[, i]) == SpecialChar[j]))
    if (nrow(subset(Data1, trimws(Data1[, i])== SpecialChar[j])) == 0) 
    {
      #cat("Column", names(Data1[i]), "is being skipped as it contains no bad data","\n" )
      next
    }
    #Delete the rows if bad data is less than 10% of the complete dataset.
    if ((rowcount/NumRows)*100 < 10)
    {
      cat("Column", names(Data1[i]), "contains",SpecialChar[j],"hence" , rowcount, "rows are being deleted")
      Data1 <- subset(Data1, trimws(Data1[, i])!= SpecialChar[j])
      Badrowcount <- TRUE
    }
    #Drop the column if it has bad data for more than 90%.
    if ((rowcount/NumRows)*100 > 90)
    {
      cat("Column", names(Data1[i]), "contains",(rowcount/NumRows)*100, "%", SpecialChar[j],"hence this column is being deleted")
      Data1[i] <- NULL
      i<- i -1
      NumCols <- NumCols-1
      Badcolcount <- TRUE
    }
    cat("\n")
  }
  cat("\n")
}
#browser()
# Cleanse the data set of NA's
Data1 <- na.omit(Data1)
#Display a message if the dataset is clean.
if (!Badcolcount && !Badrowcount )
{
  cat("No monitored special characters were found in the dataset.")
}

return(Data1)
}

