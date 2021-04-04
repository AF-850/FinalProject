# The program for sorting out the data sets for the training and test datasets 

# A.1 Reading the files
setwd('C:/Users/afarr/OneDrive/Documents/Professional Information/Programming/Coursera_DS/week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train')
c1 <- read.table('X_train.txt')
c2 <- read.table('y_train.txt')
c3 <- read.table('subject_train.txt')
dim(c1)
dim(c2) 
setwd('..')
setwd('./test')
d1 <- read.table('X_test.txt')
d2 <- read.table('y_test.txt')
d3 <- read.table('subject_test.txt')

#A.2
cc1 <- cbind(c3,c2,c1)
dd1 <- cbind(d3,d2,d1)
co <- rbind(cc1,dd1)

#A.3
n1 <- data.frame(matrix(NA,ncol=dim(co)[2]-2,nrow=1))
p1 <- data.frame(matrix(NA,ncol=dim(co)[2]-2,nrow=1))
for (i in 1:dim(co)[1]) {
  m1 <- as.numeric((co[i,3:dim(co)[2]]))
  n1[i] <- mean(m1)
  p1[i] <- sd(m1)
}  

#A.4
for (i in 1:dim(co)[1]){
  if ( co[i,2] == 1) {
    co[i,2] <- 'WALKING'
  }
  if ( co[i,2] == 2) {
    co[i,2] <- 'WALKING_UPSTARIS'
  }
  if ( co[i,2] == 3) {
    co[i,2] <- 'WALKING_DOWNSTAIRS'
  }
  if ( co[i,2] == 4) {
    co[i,2] <- 'SITTING'
  }
  if ( co[i,2] == 5) {
    co[i,2] <- 'STANDING'
  }
  if ( co[i,2] == 6) {
    co[i,2] <- 'LAYING'
  }
}

#A.5
setwd('..')
au <- read.table('features.txt')
co1 <- co[,3:563]
colnames(co1) <- au[,2] 
co2 <- cbind(co[,1:2],co1)

colnames(co2)[1] <- 'Subject'
colnames(co2)[2] <- 'Activity'
colnames(co2) <- gsub('fBodyBody', 'FrequencyBody', colnames(co2))
colnames(co2) <- gsub('fBody', 'FrequencyBody', colnames(co2))
colnames(co2) <- gsub('tBody', 'TimeBody', colnames(co2))
colnames(co2) <- gsub('\\Acc', 'Accelaration', colnames(co2))
colnames(co2) <- gsub('tGravity', 'TimeGravity', colnames(co2))
colnames(co2) <- gsub('\\Jerk', '', colnames(co2))
colnames(co2) <- gsub('\\Gyro', 'Gyroscope', colnames(co2))
colnames(co2) <- gsub('\\()', '', colnames(co2))
colnames(co2) <- gsub('\\-', '.', colnames(co2))
colnames(co2) <- gsub('\\,', '.', colnames(co2))
colnames(co2) <- gsub('gravityMean', 'GravityMean', colnames(co2))
colnames(co2) <- gsub('Mag', 'Magnitude', colnames(co2))
colnames(co2) <- gsub('std', 'StandaradDeviation', colnames(co2))
colnames(co2) <- gsub('mean', 'MeanValue', colnames(co2))
colnames(co2) <- gsub('mad', 'MedianAbsoluteDeviation', colnames(co2))
colnames(co2) <- gsub('max', 'LargerstValueinArray', colnames(co2))
colnames(co2) <- gsub('min', 'SmallestValueinArray', colnames(co2))
colnames(co2) <- gsub('sma', 'SignalMagnitudeArea', colnames(co2))
colnames(co2) <- gsub('energy', 'EnergyMeasure', colnames(co2))
colnames(co2) <- gsub('iqr', 'InterquartileRange ', colnames(co2))
colnames(co2) <- gsub('entropy', 'signalEntropy', colnames(co2))
colnames(co2) <- gsub('arCoeff', 'AutorregresionCoefficientsWithBurgOrderEqualto4', colnames(co2))
colnames(co2) <- gsub('correlation', 'correlationCoefficientBetweenTwoSignals', colnames(co2))
colnames(co2) <- gsub('maxInds', 'IndexOfTheFrequencyComponentWithLargestMagnitude', colnames(co2))
colnames(co2) <- gsub('meanFreq', 'WeightedAverageOfTheFrequencyComponentsToObtainAMeanFrequency', colnames(co2))
colnames(co2) <- gsub('skewness', 'SkewnessOfTheFrequencyDomainSignal', colnames(co2))
colnames(co2) <- gsub('kurtosis', 'KurtosisOfTheFrequencyDomainSignal', colnames(co2))
colnames(co2) <- gsub('bandsEnergy', 'EnergyOfAFrequencyIntervalWithiThe64BinsOfTheFFTOfEachWindow', colnames(co2))
colnames(co2) <- gsub('angle', 'AngleBetweenToVectors', colnames(co2))
colnames(co2) <- gsub('iqr', 'InterquartileRange ', colnames(co2))

#A.6
library(plyr)
library(dplyr)
nc1 <- colnames(co2)
nm1 <- paste('m',1:563)
nm2 <- gsub(' ', '', nm1)
#***********************
colnames(co2) <- nm2
fd <- co2 %>% arrange(m1,m2) %>% 
  group_by(m1,m2) %>%
  summarise_all(funs(mean))
#***********************
colnames(fd) <- nc1
View(fd)
write.table(fd, file = 'FinalData.txt', row.name=FALSE)
write.table(nc1, file = 'VariableNames.txt', row.name=FALSE)