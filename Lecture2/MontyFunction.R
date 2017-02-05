monty <- function(N)
{
  doors <- c('A', 'B', 'C')
  xdata <- data.frame(play=character(),
                      stringsAsFactors=FALSE)
  for (i in 1:N)
  {
    prize <- sample(doors)[1]
    pick <- sample(doors)[1]
    open <- sample(doors[which(doors != pick & doors != prize)])[1]
    switchyes <- doors[which(doors != pick & doors != open)]
    if(pick==prize)
    {
      xdata[i,1]= "Stay"
    }
    if(switchyes==prize)
    {
      xdata[i,1]= "Switch"
    }
  }
  return(xdata)
}