library(ggplot2)
library(caret)
library(GGally)

# Load train/test datasets
train <- read.csv('train.csv')
test <- read.csv('test.csv')

test.donation <- data.frame(test[,],Made.Donation.in.March.2007 = rep('None',nrow(test)))

data.combined <- rbind(train,test.donation)
str(data.combined)
data.combined$Made.Donation.in.March.2007 <- as.factor(data.combined$Made.Donation.in.March.2007)
summary(data.combined)
table(train$Made.Donation.in.March.2007)


ggpairs(data.combined[1:576,-1],mapping = aes(color = as.factor(Made.Donation.in.March.2007),alpha = 0.5))
# Total.Volume.Donated..c.c.. and Number.of.Donations are highly correlated.
# some variables have highly skewed(positive) distribution.(Months.since.Last.Donation, Number.of.Donations, Total.Volume.Donated)
# Need to transform the variables in order to remove skewness.

# Checking forr any missing values in the dataset
data.combined[!complete.cases(data.combined),]


#=================================Exploratory Data Analysis===================================#

# Hypothesis: Actice donors are more likely to donate.
# Months.since.Last.Donation
ggplot(data.combined[1:576,],aes(x = Months.since.Last.Donation, fill = Made.Donation.in.March.2007))+
  geom_histogram(color = 'black')
# recent donors seems to be more likely to donate


# Bucketing since.last.donation to make it a factor for better visualization
data.combined$since.last.bucket <- cut(x = data.combined$Months.since.Last.Donation,breaks = c(-1,2,7,14,74))  
# Months.since.Last.Donation = 0 means donated recently

# Visualizing the since.last.bucket w.r.t march.2007
ggplot(data.combined[1:576,],aes(x = since.last.bucket,fill = Made.Donation.in.March.2007))+
  geom_bar(color = 'black')
# Donors with small since.last are more likely to donate
# Recent donors are more likely to donate again.


# Number.of.Donations
ggplot(data.combined[1:576,],aes(y = Number.of.Donations, x = Made.Donation.in.March.2007,fill = Made.Donation.in.March.2007))+
  geom_boxplot()+
  geom_jitter(alpha = 0.5)
# Seems Donors with more donations are more likely to donate

  
# Bucketing Number.of.Donations to make it a factor for better visualization

  data.combined$donations.bucket <- cut(x = data.combined$Number.of.Donations, 
                                        breaks = c(0,1,2,4,6,10,50)) 
  levels(data.combined$donations.bucket)
  table(data.combined$donations.bucket)
 
# Visualizing donations.bucket wrt March 2007 donation 
ggplot(data.combined[1:576,],aes(x = donations.bucket,fill = Made.Donation.in.March.2007))+
  geom_bar()
# Donors with more number of donations are more likely to donate


#  Total.Volume.Donated..c.c..  
# Since Total.Volume.Donated..c.c.. is highly correlated(1) to Number.of.Donations, I won't be analysing this variable


# Months.since.First.Donation
 ggplot(data.combined[1:576,],aes(x = Made.Donation.in.March.2007,y = Months.since.First.Donation, fill = Made.Donation.in.March.2007))+
   geom_boxplot()+geom_jitter(alpha = 0.5)
 #  data.combined$first.donation.bucket <- cut(x = data.combined$Months.since.First.Donation,breaks = c(0,15,28,50,100))
 #  table(data.combined$first.donation.bucket)
 #  levels(data.combined$first.donation.bucket)
 #  ggplot(data.combined[1:576,],aes(x = first.donation.bucket,fill = Made.Donation.in.March.2007))+
 #   geom_bar(color = 'black')
 # # Unable to find a pattern/predictive power
 
 #=====================================================================================================#
 
 # Creating some new variables using the existing ones
 
 #Active donor : smaller the value means greater the number of donations,including the recent ones.
 data.combined$active.donor <- data.combined$Months.since.Last.Donation / data.combined$Number.of.Donations
 
 ggplot(data.combined[1:576,],aes(x = Made.Donation.in.March.2007,y = active.donor,fill = Made.Donation.in.March.2007))+
   geom_boxplot()

 
 # Ratio: smaller the ratio the better, ratio close to 1 indicates donor probabaly stopped donating blood. 
 # for eg. if last donation = 14 and first donation = 14 then donor has not donated for the last 14 months
 tail(data.combined[which(data.combined$Months.since.Last.Donation == data.combined$Months.since.First.Donation),c(2,5)])
 data.combined$ratio <- data.combined$Months.since.Last.Donation / data.combined$Months.since.First.Donation
 
 # Visualizing w.r.t March.2007
 ggplot(data.combined[1:576,],aes(y = ratio,x = Made.Donation.in.March.2007))+
   geom_boxplot()+geom_jitter(alpha = 0.5, aes(color = Made.Donation.in.March.2007))
 
 
 # Calculate the period for which the Donor has been actively donating.
data.combined$active.period <- data.combined$Months.since.First.Donation - data.combined$Months.since.Last.Donation
# where active.period = 0 means donated for the first time this current period.

# Visualizing the active.period wrt March donation
ggplot(data.combined[1:576,],aes(x = active.period,fill = Made.Donation.in.March.2007))+
   geom_density()
summary(data.combined$active.period)
cor(x = data.combined$Months.since.First.Donation,data.combined$active.period)
# 0.9460455 highly correlated. So won't be using this feature in model building


# Hypothesis : One has to wait for 3-4 months to donate again. 
# Calculate donations per month = Number.of.Donations / Months.since.First.Donation
 data.combined$donations.per.month <- ifelse(data.combined$Months.since.First.Donation == 0,0,
                                            data.combined$Number.of.Donations/data.combined$Months.since.First.Donation)

 # Visualizing donations.per.month wrt march.2007 donations
 ggplot(data.combined[1:576,],aes(y = donations.per.month,x = Made.Donation.in.March.2007))+
   geom_boxplot()+geom_jitter(alpha = 0.5, aes(color = Made.Donation.in.March.2007))
# Donors who donate once per quater are likely to donate. 
# As the frequency of donation increases to once in 2 months or every month, Donors are not likely to donate.
 

 
 # Visualizing all new variables wrt march 2007 donations
ggpairs(data.combined[1:576,c(6,9,10,12)],mapping = aes(color = Made.Donation.in.March.2007,alpha = 0.5))
# active.donor and donations.per.month have highly skewed distribution


#=====================================================================================================#
#                 Transform skewed variables and normalize the data

box_cox <- function(x){
  if(sum(x==0)!=0){x<-x+1e-6} # to avoid "Lambda could not be estimated; no transformation is applied"
  bc <- BoxCoxTrans(x)
  L <- bc$lambda
  if(L == 0){x <- log(x)}
  if(L != 0){x <- (x^L -1)/L}
  return(x)
}

normalized <- function(x){
  (x-mean(x))/sd(x)
}
 
# Rearranging data.combined and removing correlated variables

data.combined.final <- data.combined[,c(2,3,5,9,10,12,6)]

# Removing skewness for Months.since.Last.Donation, Number.of.Donations, active.donor and donations.per.month
data.combined.final[,c(1,2,4,6)] <- lapply(data.combined.final[,c(1,2,4,6)], box_cox)
data.combined.final[,-7] <- lapply(data.combined.final[,-7],normalized)

#=====================================================================================================#
 

# LogLoss <- function(actual, predicted, eps=0.00001) {
#   predicted <- pmin(pmax(predicted, eps), 1-eps)
#   -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
# }


#==================================================================================================

# CROSS VALIDATION using CARET

#==================================================================================================

library(caret)

# Retrieving modified train and test from data.combined.final
train.1 <- data.combined.final[which(data.combined.final$Made.Donation.in.March.2007!='None'),]
str(train.1)
train.1$Made.Donation.in.March.2007 <- droplevels(train.1$Made.Donation.in.March.2007)

test.1 <- data.combined.final[which(data.combined.final$Made.Donation.in.March.2007=='None'),]
test.1 <- test.1[,-7]
str(test.1)

# partioning the train.1 using stratification
set.seed(1234)
index <- createDataPartition(train.1$Made.Donation.in.March.2007,times = 1, p = 0.7,list = FALSE)
train.2 <- train.1[index,]
validation <- train.1[-index,]


tc <- trainControl(method = 'repeatedcv',number = 10,repeats = 10,classProbs = TRUE,summaryFunction = mnLogLoss)

rf.model <- train(Made.Donation.in.March.2007~.,
                  data = train.2,
                  trControl = tc,
                  method = 'rf',
                  metric = 'logLoss')

# Error: At least one of the class levels is not a valid R variable name; 
# This will cause errors when class probabilities are generated because the variables names will be converted to  X0, X1 . 
# Please use factor levels that can be used as valid R variable names  (see ?make.names for help).

# convert 0/1 to X0/X1
levels(train.1$Made.Donation.in.March.2007) <- make.names(levels(factor(train.1$Made.Donation.in.March.2007)))

rf.model <- train(Made.Donation.in.March.2007~.,
                  data = train.2,
                  trControl = tc,
                  method = 'rf',
                  metric = 'logLoss')
rf.model
# mtry  logLoss  
# 2     0.9032593
# 4     0.9673455
# 6     0.9789294

gbm.model <- train(Made.Donation.in.March.2007~.,
                  data = train.2,
                  trControl = tc,
                  method = 'gbm',
                  metric = 'logLoss')
gbm.model
# logLoss  
# 0.4923098


# Validation
gbm.pred <- predict(gbm.model,newdata = validation,'prob')
gbm.pred$obs <- validation$Made.Donation.in.March.2007
mnLogLoss(gbm.pred, lev = levels(gbm.pred$obs))
# logLoss 
# 0.4561137 


# Prediction on test data & Submission
pred <- predict(gbm.model,newdata = test.1,'prob')
submit.df <- data.frame(test$X,pred[,2])
colnames(submit.df) <- c('','Made Donation in March 2007')
write.csv(submit.df,file = 'GBM_SUB_20180904_1.csv', row.names = FALSE)
