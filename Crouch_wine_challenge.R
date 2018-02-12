
# SAP.iO Technical Challenge
# Adam Crouch
rm(list = ls())
# install.packages("_______")
library(ggplot2)
library(ggthemes)
library(lattice)
library(gmodels)
library(tidyverse)



#### READ IN DATA AND CHECK STRUCTURE ####
# reading in data - set working directory to location of csv
setwd("C:/Users/Adam Crouch/Google Drive/Github/")
chall <- data.frame(read_csv("SAPio_DataScience_Challenge.csv"))
# there is a problem reading in some variables - parsing failure 
# need to read in numeric instead of integer
chall <- data.frame(read_csv("SAPio_DataScience_Challenge.csv", 
                             col_types = cols('total sulfur dioxide' = col_double())))

# checking the structure of datatable and summary stats 
str(chall)
summary(chall)
# are there any duplicates? 109 duplicates - potential problem? will revisit if time permits
duplicates <- which(duplicated(chall))
chall.u <- unique(chall)
# for now, use unique datatable as dataframe for analysis
df <- chall.u



#### DEFINE A 'GOOD' WINE ####
# checking out distribution of quality variable 
ggplot(df, aes(x=quality)) + 
  geom_histogram(binwidth = 1, colour="black", fill="white") + theme_fivethirtyeight()
# for future classifacation, want to assign 'good' and 'bad' binary variable 
# since chall$quality median is 6, anything over 6 will be considered 'good'
df$good.wine <- ifelse(df$quality>6,1,0)

# curious how many of total are now considered 'good' 
ggplot(df, aes(x=good.wine,fill=factor(good.wine))) + 
  geom_bar(stat = "count") + theme_fivethirtyeight()

CrossTable(df$good.wine) # ~20% are good
CrossTable(df$good.wine, df$type, prop.c=F,prop.chisq=F)



#### CHECKING CORRELATIONS AND DISTRIBUTIONS ####
library(corrplot)
corrplot(cor(df[,2:16], use = "pairwise.complete.obs"))
# there are missing or NA obs for several variables - will likely need imputation
# POSITIVE CORR: alcohol is the only predictor that positively correlates with quality
# NEGATIVE CORR: volatile.acidity, chlorides, and density have negative correlations
# given these correlation, want to look into distribution between good and bad

# boxplot of alcohol between good and bad
ggplot(df,aes(x = good.wine, y = alcohol,group=good.wine)) +
  geom_boxplot(fill = 'red') + 
  theme_fivethirtyeight()
# BETTER PLOT: density plot of alcohol between good and bad
ggplot(df,aes(x=alcohol,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=median(alcohol[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=median(alcohol[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  theme_fivethirtyeight()

# with more time, would have looked at more predictor distributions 



#### EXPLORING MISSING DATA AND IMPUTATION ####
# exploring missing data across all variables  
library(Amelia)
library(missForest)
missmap(df, main = "Missing Values vs observed")
# residual sugar, astringency.rating, volatile.acidity, vintage, and pH missing obs

# looking at more advanced ways to impute, but going with mean and mode to save time
# df.mis <- prodNA(df, noNA=0.1)
# summary(df.mis)
# impute using all parameters as default values 
# df.imp <- missForest(df.mis)

# make imputations based on average values and mode (vintage)
df.imp <- df
df.imp$astringency.rating[is.na(df.imp$astringency.rating)] <- mean(df$astringency.rating, na.rm = T)
df.imp$volatile.acidity[is.na(df.imp$volatile.acidity)] <- mean(df$volatile.acidity, na.rm = T)
df.imp$vintage[is.na(df.imp$vintage)] <- mode(df$vintage)
df.imp$pH[is.na(df.imp$pH)] <- mean(df$pH, na.rm = T)

# residual sugar has too many missing values, so will drop for now
df.imp$residual.sugar = NULL

# checking to make sure imputation worked
missmap(df.imp, main = "Missing Values after imputation")

# want to classify missing variables (given more time)
df.imp$quality = NULL


#### PARTITIONING ####
set.seed(919)
partition <- sample(1:2, size = nrow(df.imp), prob = c(.7,.30), replace = T)

# Create a training and validation set from the original data frame 
train <- df.imp[partition == 1, ]    # subset the grade data frame to training indices only
test <- df.imp[partition == 2, ]  # subset the grade data frame to validation indices only

test <- test %>%
  filter(vintage>2001) # removing one obs with vintage year 2001



#### FITTING LOGISTIC REGRESSION #####

good.model <- glm(good.wine~., family = binomial(link='logit'), # want to see odds ratios
              data = train)
summary(good.model)
# we see the following variables are significant:
# volatile.acidity
# chlorides
# free.sulfur.dioxide
# total.sulfur.dioxide
# density
# sulphates
# alcohol
# vintage(2005 and 2008) 



#### ASSESSING LOGISTIC REGRESSION ####
fit.model <- predict(good.model, newdata = test, type = "response")
range(fit.model)

# selecting arbitrary cut-off of 30%
fit.df <- ifelse(fit.model > 0.3,1,0)
fit.df

# looking at missclassification and confusion matrix 
misclass_error <- mean(fit.df != test$good.wine)
print(paste('Accuracy',1-misclass_error)) # ~ 0.80
table(test$good.wine, fit.df)

# looking at ROC curve and Area Under Curve
library(ROCR)
pr <- prediction(fit.df, test$good.wine)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # AUC ~ 0.71

# The logistic regression provides decent predictive power
# would consider checking interactions and multi-collinearity given more time
# there are likely a number of interactions between sulfur variables, density, and alcohol level


#### TREE BASED CLASSIFICATION MODELING ####
# library(randomForest)
# chall.RF <- randomForest(factor(good.wine)~., df.imp, ntree=150)

library(rpart)
tree.fit <- rpart(good.wine ~ ., train, method = "class")
summary(tree.fit)
plotcp(tree.fit)
plot(tree.fit)

#### FUTURE NEXT STEPS ####
# would like to incorporate decision tree with logistric regression in order to get 
# a scorecard of good and bad predictors
# same idea as in credit scoring
# but in this case with wine attributes and physicochemical characteristics


