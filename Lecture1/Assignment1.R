# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
source("CleanData.R")

read.energy = function(file = 'EnergyEfficiencyData_1.csv'){
  ## Read the csv file
  energy.eff <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
}
energy.eff = read.energy()

# Cleanse the dataset off special characters (?,%,#)
Cleansedenergy.ff <- Clean(energy.eff)
# Cleanse the data set of NA's
Cleansedenergy.ff<- na.omit(Cleansedenergy.ff)

#Check out the structure of the cleansed object.
str((Cleansedenergy.ff))

#Removing column "BadData_1" as it serves  no purpose in the analysis.
Cleansedenergy.ff$BadData_1 <- NULL

#All the columns are num or integer hence we can calculate the summary of the complete dataset
lapply(Cleansedenergy.ff, summary)
# Heating.load and cooling.load is reflecting a postive skewed distribution as mean is greater than median and 1st QR is considerably different from 3rd QR.
# Calculate standard deviation to measure the dispursion of the distribution
lapply(Cleansedenergy.ff, sd)
# Frequency Tables.
table(Cleansedenergy.ff$Overall.Height)
#Frequency table of two categorical variables.
table(Cleansedenergy.ff$Overall.Height, Cleansedenergy.ff$Orientation)
# Calculate coveriance between numeric attributes.
cov(Cleansedenergy.ff)
#Calculate the Pearson's correlation of between numeric attributes.
cor(Cleansedenergy.ff)# Overall.Height is strongly related to Heating.load and cooling.load
#Create barchart for Wall Area
require(ggplot2)
ggplot(Cleansedenergy.ff, aes(x=reorder(Wall.Area,Wall.Area, function(x) -length(x)))) + ## Function shorts the bars
  geom_bar() + 
  xlab('Wall Area')
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) ## Theme function for conrol
# Bar plot clearly shows you that wall area between 294 & 318.5 is most prevalent in the homes.
#Create histograms for Heating.load
ggplot(Cleansedenergy.ff, aes(Heating.Load)) + geom_histogram(binwidth = .5) + 
  ggtitle('Histogram of Heating Load')
#Create boxplot for one variable
ggplot(Cleansedenergy.ff, aes(x = factor(0), y = Cooling.Load )) + geom_boxplot()
#Create boxplot to compare two variables.
ggplot(Cleansedenergy.ff, aes(x = factor(Overall.Height), y = Cooling.Load)) + geom_boxplot() + 
  xlab('Overall Height') + ggtitle('Cooling load by Overall Height')
#One can clearly see that height is strongly correlated to cooling loads.
ggplot(Cleansedenergy.ff, aes(x = factor(Roof.Area), y = Cooling.Load)) + geom_boxplot() + 
  xlab('Roof Area') + ggtitle('Cooling load by Roof Area')
#One can see that Roof Area has a negative correlation with cooling loads.
ggplot(Cleansedenergy.ff, aes(x = factor(Orientation), y = Cooling.Load)) + geom_boxplot() + 
  xlab('Orientation') + ggtitle('Cooling load by Orientation')
#One can clearly see that Orientation has no correlation with cooling loads.
#Kernel Densite Plots.
ggplot(Cleansedenergy.ff, aes(Cooling.Load )) + geom_density()
# Violin Plot
ggplot(Cleansedenergy.ff, aes(x = factor(Overall.Height), y = Cooling.Load)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab('Overall Height')  + ggtitle('Cooling Load by Overall Height')
#Violin Plot
ggplot(Cleansedenergy.ff, aes(x = factor(Orientation), y = Cooling.Load)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab('Orientation')  + ggtitle('Cooling Load by Roof Area')

