# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
# Load the functions.
source("CleanData.R")
source("Loaddata.R")


#Call function to load the csv file.
FileName <- 'EnergyEfficiencyData.csv'
energy.eff = Loaddata(FileName)

# Call function to cleanse the dataset off special characters (?,%,#)
Cleansedenergy.ff <- Clean(energy.eff)

#Check out the structure of the cleansed object.
str((Cleansedenergy.ff))


#All the columns are num or integer hence we can calculate the summary of the complete dataset
lapply(Cleansedenergy.ff, summary)
# Heating.load and cooling.load is reflecting a postive skewed distribution as mean is greater than median and 1st QR is considerably different from 3rd QR.
# Calculate standard deviation to measure the dispursion of the distribution
lapply(Cleansedenergy.ff, sd)

#Comparing normalized Heating and Cooling loads of a home with a higher ceiling.
#z-score normalization for Heating.load of 40.4
Heating.Load_mean <- mean(Cleansedenergy.ff$Heating.Load)
Heating.Load_sd <- sd(Cleansedenergy.ff$Heating.Load)
Normalized_40.4 <- (40.4 - Heating.Load_mean) / Heating.Load_sd
cat("Normalized Heating Load for 40.4 at record # 29" , "is:",Normalized_40.4,"\n")

#z-score normalization for Cooloing.load of 39.67
Cooling.Load_mean <- mean(Cleansedenergy.ff$Cooling.Load)
Cooling.Load_sd <- sd(Cleansedenergy.ff$Cooling.Load)
Normalized_39.67 <- (39.67 - Cooling.Load_mean) / Cooling.Load_sd
cat("Normalized cooling Load for 39.67 at record # 29" , "is:",Normalized_39.67,"\n")

#z-score normalization for Heating.load of 24.03
Heating.Load_mean <- mean(Cleansedenergy.ff$Heating.Load)
Heating.Load_sd <- sd(Cleansedenergy.ff$Heating.Load)
Normalized_24.03 <- (24.03 - Heating.Load_mean) / Heating.Load_sd
cat("Normalized Heating Load for 24.03 at record # 350" , "is:",Normalized_24.03,"\n")

#z-score normalization for Cooloing.load of 24.91
Cooling.Load_mean <- mean(Cleansedenergy.ff$Cooling.Load)
Cooling.Load_sd <- sd(Cleansedenergy.ff$Cooling.Load)
Normalized_24.91 <- (24.91 - Cooling.Load_mean) / Cooling.Load_sd
cat("Normalized cooling Load for 24.91 at record # 350" , "is:",Normalized_24.91,"\n")


#Use probabililty normal distribution to gauge the percentatage of homes based on the z-score
p_Normalized_40.4 <- 1- pnorm(Normalized_40.4)
cat("Percentage of homes with a Heating Load over 40.4" , "is:",p_Normalized_40.4*100,"\n")

p_Normalized_39.67 <- 1- pnorm(Normalized_39.67)
cat("Percentage of homes with a cooling Load over 39.67" , "is:",p_Normalized_39.67*100,"\n")

p_Normalized_24.03 <- pnorm(Normalized_24.03)
cat("Percentage of homes with a Heating Load under 24.03" , "is:",p_Normalized_24.03*100,"\n")

p_Normalized_24.91 <- pnorm(Normalized_24.91)
cat("Percentage of homes with a cooling Load under 24.91" , "is:",p_Normalized_24.91*100,"\n")

# Frequency Tables.
table(Cleansedenergy.ff$Overall.Height)
table(Cleansedenergy.ff$Orientation)

#Frequency table of two categorical variables.
table(Cleansedenergy.ff$Overall.Height, Cleansedenergy.ff$Orientation)

# Calculate coveriance between numeric attributes.
cov(Cleansedenergy.ff)
#Calculate the Pearson's correlation of between numeric attributes.
cor(Cleansedenergy.ff)# Overall.Height is strongly related to Heating.load and cooling.load
#Create barchart for Wall Area
require(ggplot2)
ggplot(Cleansedenergy.ff, aes(x=Orientation)) + ## Function shorts the bars
  geom_bar() + 
  xlab('Orientation')
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Bar plot clearly shows you that overall height of 2,3,4,5 is equally divided in the dataset.

#Create histogram for Heating.load
ggplot(Cleansedenergy.ff, aes(Heating.Load)) + geom_histogram(binwidth = .5) + 
  ggtitle('Histogram of Heating Load')

#Calculate z-score for Heating.load
zscore_heating <- scale(Cleansedenergy.ff$Heating.Load,center = TRUE, scale = TRUE)

#Create histogram for z-score of Heating Load
ggplot(Cleansedenergy.ff, aes(zscore_heating)) + geom_histogram(binwidth = .5) + 
  xlab('Normalized Heating Load') +
  ggtitle('Histogram of Z-Score Normalized Heating Load')

#Create histogram for Cooling.load
ggplot(Cleansedenergy.ff, aes(Cooling.Load)) + geom_histogram(binwidth = .5) + 
  ggtitle('Histogram of Cooling Load')

#calculate z-score for cooling.load
zscore_cooling <- scale(Cleansedenergy.ff$Cooling.Load,center = TRUE, scale = TRUE)

#Create histogram for z-score of cooling Load
ggplot(Cleansedenergy.ff, aes(zscore_cooling)) + geom_histogram(binwidth = .5) + 
  xlab('Normalized Cooling Load') +
  ggtitle('Histogram of Z-Score Cooling Load')

#Create boxplot for one variable
ggplot(Cleansedenergy.ff, aes(x = factor(0), y = Heating.Load )) + geom_boxplot()
#Create boxplot to compare two variables.
ggplot(Cleansedenergy.ff, aes(x = factor(Overall.Height), y = Heating.Load)) + geom_boxplot() + 
  xlab('Overall Height') + ggtitle('Heating load by Overall Height')
#One can clearly see that height is strongly correlated to heating loads.

ggplot(Cleansedenergy.ff, aes(x = factor(Roof.Area), y = Heating.Load)) + geom_boxplot() + 
  xlab('Roof Area') + ggtitle('Heating load by Roof Area')
#One can see that Roof Area has a negative correlation with Heating loads.

ggplot(Cleansedenergy.ff, aes(x = factor(Orientation), y = Heating.Load)) + geom_boxplot() + 
  xlab('Orientation') + ggtitle('Heating load by Orientation')
#One can clearly see that Orientation has no correlation with Heating loads.

#Kernel Densite Plots.
ggplot(Cleansedenergy.ff, aes(Heating.Load )) + geom_density()

# Violin Plot
ggplot(Cleansedenergy.ff, aes(x = factor(Overall.Height), y = Heating.Load)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab('Overall Height')  + ggtitle('Heating Load by Overall Height')
#Violin Plot
ggplot(Cleansedenergy.ff, aes(x = factor(Orientation), y = Heating.Load)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab('Orientation')  + ggtitle('Heating Load by Roof Area')

#Scatter Plot
ggplot(Cleansedenergy.ff, aes(x = Overall.Height, y = Heating.Load)) + geom_point() + 
  xlab('Overall Height') + ylab('Heating Load') + 
  ggtitle('Relationship between Heating Load and Overall Height')

#2D Kernel Plot
ggplot(Cleansedenergy.ff, aes(Overall.Height, Heating.Load)) + geom_point() + 
  geom_density2d() +
  xlab('Overall Height') + ylab('Heating Load') +
  ggtitle('Relationship between Heating.Load and Overall.Height')

#Hexbin Plots
ggplot(Cleansedenergy.ff, aes(Relative.Compactness, Heating.Load)) + 
  stat_binhex(bins = 10) +
  xlab('Relative Compactness') + ylab('Heating Load') +
  ggtitle('Relationship between Relative Compactness and Heating Load')

#Line Plots
ggplot(Cleansedenergy.ff, aes(Cooling.Load, Heating.Load)) + geom_line() + ggtitle('Line plot of Cooling Load vs. Heating Load')

#scatter plot with more than two variables.
ggplot(Cleansedenergy.ff, aes(Overall.Height, Heating.Load)) + geom_point(aes(color = factor(Surface.Area))) + 
  xlab('Overall Height') + ylab('Heating Load') + 
  ggtitle('Relationship between Overall Height and Heating Load \n with surface area')

#scatter plot with more than two variables.
ggplot(Cleansedenergy.ff, aes(Relative.Compactness,Heating.Load)) + geom_point(aes(color = factor(Roof.Area))) + 
  xlab('Relative Compactness') + ylab('Heating Load') + 
  ggtitle('Relationship between Relative Compactness and Heating Load\n with Roof.Area')

#Marker aesthetic
ggplot(Cleansedenergy.ff, aes(Relative.Compactness,Heating.Load)) + geom_point(aes(color = factor(Roof.Area), size = Overall.Height), alpha = 0.4) + 
  xlab('Relative Compactness') + ylab('Heating Load') + 
  ggtitle('Relationship between Relative Compactness and Heating Load\n with Roof Area, \n with marker area indicating Overall Height')

#Scatter Plot
options(repr.plot.width=8, repr.plot.height=8)
require(car)
scatterplotMatrix(~ Relative.Compactness + Surface.Area + Wall.Area + Roof.Area + Overall.Height + Heating.Load + Cooling.Load,  data = Cleansedenergy.ff)

#Correlation Plot

options(repr.plot.width=6, repr.plot.height=6)
library(ellipse)
R = cor(Cleansedenergy.ff[, c('Relative.Compactness', 'Surface.Area', 'Wall.Area','Roof.Area', 'Overall.Height', 'Heating.Load', 'Cooling.Load')], method = 'pearson')
print(R)
plotcorr(R, col = colorRampPalette(c("firebrick3", "white", "navy"))(10))
