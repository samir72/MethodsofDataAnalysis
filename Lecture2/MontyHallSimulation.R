rm(list=ls())
# Clear Console:
cat("\014")
# Function to simulate Monty Hall game.
monty <- function(N)
{
  doors <- c('A', 'B', 'C')
  df <- data.frame(play=character(),
                      stringsAsFactors=FALSE)
  for (i in 1:N)
  {
    prize <- sample(doors)[1] # Prize is behind this door.
    pick <- sample(doors)[1] # Player first picks this door
    open <- sample(doors[which(doors != pick & doors != prize)])[1] # Monty now opens the second door
    switch <- doors[which(doors != pick & doors != open)] # Player could now switch the door.
    if(pick==prize)
    {
      df[i,1]= "Stay"
    }
    if(switch==prize)
    {
      df[i,1]= "Switch"
    }
  }
  return(df)
}

N= 1000 # This simulation with 1000 iterations.
dists <- monty(N) # call the function
table(dists$play) # Create a frequency table
count <- table(dists) 
staycount <- count[1]# Get the count of stays
Switchcount <- count[2]# Get the count of switches.
cat(paste('Stay would win the prize by : ', (staycount/N)*100,'%\n'))
cat(paste('Switching would win the prize by : ',(Switchcount/N)*100,'%\n'))
require(ggplot2)
# Create a bar plot to show the difference in 
ggplot(dists, aes(dists$play)) + ## Specify the data frame and columns. Note the + chain operator
  geom_bar()+
  xlab('Play') +  
  ggtitle('Bar Plot For Monty Hall')



