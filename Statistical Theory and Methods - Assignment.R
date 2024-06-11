### Statistical Theory and Methods 
### Coursework 1 
rm(list=ls())
load("rehoming.Rdata")
createsample("201749908")
save(mysample,file="mysample.RData")
mysample
### The dataset I am working with is mysample
## Column Names in the dataset 
colnames(mysample)
## Data type for all the columns in the dataset 
str(mysample)
### Getting the breeds in the samples 
unique(mysample$Breed)
#---------------------------------------------------------#
#--------------------- Data Cleaning----------------------# 
# Unique values in all the columns 
unique(mysample$Visited)#None
unique(mysample$Rehomed)### Has 99999 and -1 present
unique(mysample$Health)#None
unique(mysample$Breed)### Has NA present 
unique(mysample$Age)#None
unique(mysample$Reason)### Has NA values present 
unique(mysample$Returned)### Has NA values present 
### Getting the percentage of observations removed 
## Breed 
len_breed=length(mysample$Breed)
len_breed###no. of samples in breed
len_na_breed=sum(is.na(mysample$Breed))
len_na_breed
per_rem_breed=(len_na_breed/len_breed)*100
per_rem_breed
## Rehoming 
len_rehoming=length(mysample$Rehomed)
len_rehoming
len_na_rehoming=nrow(mysample[mysample$Rehomed=='99999',])
len_na_rehoming
per_rem_rehoming=(len_na_rehoming/len_rehoming)*100
per_rem_rehoming
### Both together 
a=mysample[is.na(mysample$Breed),]
a
# no common values - so basically the sum of two 
### Droping rows based on the Rehoming column 
new_samples<-mysample[mysample$Rehomed!='99999',]
length(new_samples$Visited)
### Dropping rows based on the Breed column 
new_samp<-mysample[!is.na(mysample$Breed),]
new_samp<-new_samp[new_samp$Rehomed!='99999',]
length(new_samp$Visited)
#---------------------------------------------------------#
#-------------------Data Exploration----------------------#
new_data_grouped<-split(new_samp, new_samp$Breed)
new_data_grouped
summary(new_data_grouped$`Border Collie`)
summary(new_data_grouped$Greyhound)
summary(new_data_grouped$`Labrador Retriever`)
### Get box plot based on Health
boxplot(new_data_grouped$`Border Collie`$Health)
boxplot(new_data_grouped$Greyhound$Health)
boxplot(new_data_grouped$`Labrador Retriever`$Health)
### Bar graph based on age 
par(mfrow=c(3,1))
A<-new_data_grouped$`Border Collie`$Age
a<-table(A)
barplot(a, main="Age Distribution of Border Collies", xlab="Age",ylab="Total Number", horiz=TRUE)
B<-new_data_grouped$Greyhound$Age
b<-table(B)
barplot(b,main="Age Distribution of Greyhound", xlab="Age",ylab="Total Number", horiz=TRUE)
C<-new_data_grouped$`Labrador Retriever`$Age
c<-table(C)
barplot(c,main="Age Distribution of Labrador Retrievers", xlab="Age",ylab="Total Number", horiz=TRUE)
D<-new_data_grouped$`Labrador Retriever`$Visited
d<-table(D)
barplot(d,main="Age Distribution of Greyhound", xlab="Age",ylab="Total Number")
# Got to remove the -1 values because visiting cant be negative 
colnames(new_data_grouped$`Border Collie`)
#---------------------------------------------------------#
#-----------------Modelling and estimation----------------#
# Rehoming plot 
par(mfrow=c(3,1))
new_data_grouped<-split(new_samples, new_samples$Breed)
A<-new_data_grouped$`Border Collie`
B<-new_data_grouped$Greyhound
C<-new_data_grouped$`Labrador Retriever`
BC_R<-A$Rehomed
B$Rehomed### Removing the -1 as well 
B<-B[B$Rehomed!='-1',]
G_R<-B$Rehomed
LR_R<-C$Rehome
range(BC_R)
range(G_R)
range(LR_R)
hist(BC_R, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60),right=FALSE, freq=TRUE, main="Histogram of Border Collies rehomed", xlab="Week Ranges")
hist(G_R,breaks=c(0,15,30,45,60,75,90),right=FALSE, freq=TRUE, main="Histogram of Greyhounds rehomed", xlab="Week Ranges")
hist(LR_R, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55),right=FALSE,freq=TRUE, main="Histogram of Labrador Retrievers rehomed", xlab="Week Ranges")
## They are discrete Variables - as it is in terms of whole weeks and not with decimal places 
####################
# Binomial Distribuiton 
f = function(x, n, p){
  return( (p^x)*((1-p)^(n-x))*factorial(n)/(factorial(x)*factorial(n-x)))
}
####################
# Poissons Distribution 
f1 = function (l, x){
  return (((l^x)*exp(-1*l))/factorial(x))
}
####################
#Geometric Distribution 
f2=function(p,x){
  return (p*((1-p)^x))
}
#######################################
#BORDER COLLIE
hist(BC_R, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60),right=FALSE, freq=TRUE)
BC_R<-c(BC_R)
BC_R
x=BC_R
curve(dnorm(x, mean=mean(x), sd=sd(x)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
plotNormalHistogram( x, prob = FALSE, 
                     main = "Normal Distribution overlay on Histogram", 
                     length = 60 ) 
pmf <- dpois(x, lambda=mean(x))
plot(
  x,
  pmf,
  type = "h",
  lwd = 3,
  main = "Poisson PMF",
  xlab = "Number of events",
  ylab = "Probability"
)

### Best fit is Binomial Distribution
########################################
#GREYHOUND
hist(G_R,breaks=c(0,15,30,45,60,75),right=FALSE, freq=FALSE)
q<-mean(G_R)
p<-1/q
x<-c(G_R)
curve(dexp(x, rate=1/mean(x)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
curve(dgeom(x, p=1/mean(x)), 
      col="red", lwd=2, add=TRUE, yaxt="n")
### best fit is Geometric Distribution 
########################################
#LAB RETRIEVER
hist(LR_R, breaks=c(0,10,20,30,40,50,60),right=FALSE,freq=TRUE)
LR_R<-c(LR_R)
LR_R
x=LR_R
curve(dnorm(x, mean=mean(x), sd=sd(x)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")
plotNormalHistogram( x, prob = FALSE, 
                     main = "Normal Distribution overlay on Histogram", 
                     length = 60 ) 
pmf <- dpois(x, lambda=mean(x))
plot(
  x,
  pmf,
  type = "h",
  lwd = 3,
  main = "Poisson PMF",
  xlab = "Number of events",
  ylab = "Probability"
)
l<-mean(BC_R)
l
n<-60
p<-l/n
x<-seq(from=0,to=60,by=1)
plot(x,f1(l,x),col="blue", type='l')




l<-mean(LR_R)
n<-50
p<-l/n
x<-seq(from=0,to=50,by=1)
plot(x,f1(l,x),col="blue", type='l')
plot(x,f(x,n,p),col="red",type='l')
### best fit is Poissons Distribution 
########################################
#Checking the Suitability of the proposed model 
# Checking for QQ plots 
# Border Collie 
par(mfrow=c(1,1))
BC_R
BC_R<-BC_R[order(BC_R)]
BC_R
mu<-mean(BC_R)
sigma<-sd(BC_R)
sort(BC_R)
z<-(sort(BC_R)-mu)/sigma
n<-length(BC_R)
r<-(1:n)
q<-qnorm(p=r/(n+1),mean=0,sd=1)
plot(q,z)
abline(a=0, b=1, col="red")
qqnorm(BC_R)
abline(a=0, b=1, col="red")
#--------------------------------------------------#
#----------------Inference------------------------#
# What to check the mean rehoming time for the three breeds is 27 weeks 
# We use confidence interval to do this 
# Its like guessing ranges, how likely it is that the mean rehoming time 
# is within a certain range. That range is called as confidence interval 
# 2 types - z intervals and T intervals 
# 1.z intervals - when the population sd is known 
# 2. t intervals - when the population sd is not known 
################################################
# Using z intervals 
# 1. Border Collie 
mu<-mean(BC_R)
sigma<-sd(BC_R)
mu
sigma 
z<-1.96
CI_1 <-(mu-(z*sigma))
CI_2 <-(mu+(z*sigma))
CI_1
CI_2
# 2. Greyhound 
mu<-mean(G_R)
sigma<-sd(G_R)
mu
sigma 
z<-1.96
CI_1 <-(mu-(z*sigma))
CI_2 <-(mu+(z*sigma))
CI_1
CI_2
# 3. Lab Retriever 
mu<-mean(LR_R)
sigma<-sd(LR_R)
mu
sigma 
z<-1.96
CI_1 <-(mu-(z*sigma))
CI_2 <-(mu+(z*sigma))
CI_1
CI_2
### uSING T - TEST 
# 1. Border Collie 
mu<-mean(BC_R)
sigma<-sd(BC_R)
n<-length(BC_R)
standard_error = sigma / sqrt(n)
degrees_of_freedom = n - 1
t_value = qt(0.975, degrees_of_freedom)  # t-value for 95% confidence interval
margin_of_error = t_value * (standard_error)
lower_ci = mu - margin_of_error
upper_ci = mu + margin_of_error
print( c("border collie",mu, lower_ci, upper_ci))
# 2. Grey hound 
mu<-mean(G_R)
sigma<-sd(G_R)
n<-length(G_R)
standard_error = sigma / sqrt(n)
degrees_of_freedom = n - 1
t_value = qt(0.975, degrees_of_freedom)  # t-value for 95% confidence interval
margin_of_error = t_value * (standard_error)
lower_ci = mu - margin_of_error
upper_ci = mu + margin_of_error
print( c("Grey hound",mu, lower_ci, upper_ci))
# 3. Lab Ret 
mu<-mean(LR_R)
sigma<-sd(LR_R)
n<-length(LR_R)
standard_error = sigma / sqrt(n)
degrees_of_freedom = n - 1
t_value = qt(0.975, degrees_of_freedom)  # t-value for 95% confidence interval
margin_of_error = t_value * (standard_error)
lower_ci = mu - margin_of_error
upper_ci = mu + margin_of_error
print( c("Lab ret",mu, lower_ci, upper_ci))
#-------------------------------------------------------------#
#-------------------------Discussions-------------------------#
#Will directly write in Word 

#-------------------------------------------------------------#
#------------------------Comparison---------------------------#
# A, B combo 
breed1 <- A
breed2 <- B
breed1
breed2
breed1_R<-breed1$Rehomed
breed2_R<-breed2$Rehomed
mean_breed1 <- mean(breed1_R)
mean_breed2 <- mean(breed2_R)
sd_breed1 <- sd(breed1_R)
sd_breed2 <- sd(breed2_R)
n_breed1 <- length(breed1_R)
n_breed2 <- length(breed2_R)
standard_error_breed1 <- sd_breed1 / sqrt(n_breed1)
standard_error_breed2 <- sd_breed2 / sqrt(n_breed2)
difference_means <- mean_breed1 - mean_breed2
margin_of_error <- 1.96 * sqrt(standard_error_breed1^2 + standard_error_breed2^2)
lower_ci <- difference_means - margin_of_error
upper_ci <- difference_means + margin_of_error
print( c("Border Collie','Greyhound','Breed 1 mean", 
         mean_breed1, 'Breed 2 mean',mean_breed2, 
         'lower _ci',lower_ci,"Upper_ci", upper_ci))
# A, C Combo 
breed1 <- A
breed2 <- C
breed1
breed2
breed1_R<-breed1$Rehomed
breed2_R<-breed2$Rehomed
mean_breed1 <- mean(breed1_R)
mean_breed2 <- mean(breed2_R)
sd_breed1 <- sd(breed1_R)
sd_breed2 <- sd(breed2_R)
n_breed1 <- length(breed1_R)
n_breed2 <- length(breed2_R)
standard_error_breed1 <- sd_breed1 / sqrt(n_breed1)
standard_error_breed2 <- sd_breed2 / sqrt(n_breed2)
difference_means <- mean_breed1 - mean_breed2
margin_of_error <- 1.96 * sqrt(standard_error_breed1^2 + standard_error_breed2^2)
lower_ci <- difference_means - margin_of_error
upper_ci <- difference_means + margin_of_error
print( c("Border Collie','Lab','Breed 1 mean", 
         mean_breed1, 'Breed 2 mean',mean_breed2, 
         'lower _ci',lower_ci,"Upper_ci", upper_ci))
# B,C combo 
breed1 <- B
breed2 <- C
breed1
breed2
breed1_R<-breed1$Rehomed
breed2_R<-breed2$Rehomed
mean_breed1 <- mean(breed1_R)
mean_breed2 <- mean(breed2_R)
sd_breed1 <- sd(breed1_R)
sd_breed2 <- sd(breed2_R)
n_breed1 <- length(breed1_R)
n_breed2 <- length(breed2_R)
standard_error_breed1 <- sd_breed1 / sqrt(n_breed1)
standard_error_breed2 <- sd_breed2 / sqrt(n_breed2)
difference_means <- mean_breed1 - mean_breed2
margin_of_error <- 1.96 * sqrt(standard_error_breed1^2 + standard_error_breed2^2)
lower_ci <- difference_means - margin_of_error
upper_ci <- difference_means + margin_of_error
print( c('Greyhound','Lab ret','Breed 1 mean', 
         mean_breed1, 'Breed 2 mean',mean_breed2, 
         'lower _ci',lower_ci,"Upper_ci", upper_ci))
###############################################################

