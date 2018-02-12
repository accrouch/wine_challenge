
# SAP.iO Technical Challenge
# Adam Crouch
rm(list = ls())
# install.packages("_______")
library(ggplot2)
library(corrplot)
library(randomForest)
library(rpart)
library(ggthemes)
library(lattice)
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
# are there any duplicates? 109 duplicates - potential problem? will revisit
dups <- which(duplicated(chall))
chall.u <- unique(chall)
# for now, use unique datatable as dataframe for analysis
df <- chall.u

# curious how many of each type of wine - mostly white 
table(df$type)

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
tbl <- table(df$good.wine)
prop <- round(prop.table(tbl)*100,2)
good.tbl <- cbind(tbl,prop) # combine counts and proportions
good.tbl # ~20% are good

#### CHECKING CORRELATIONS ####
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

#### EXPLORING MISSING DATA AND IMPUTATION ####
# exploring missing data across all variables  
library(Amelia)
library(missForest)
missmap(df, main = "Missing Values vs observed")
# residual sugar, astringency.rating, volatile.acidity, vintage, and pH missing obs

# df.mis <- prodNA(df, noNA=0.1)
# summary(df.mis)
# impute using all parameters as default values 
# df.imp <- missForest(df.mis)

# make imputations based on average values
df.imp <- df
df.imp$astringency.rating[is.na(df.imp$astringency.rating)] <- mean(df$astringency.rating, na.rm = T)
df.imp$volatile.acidity[is.na(df.imp$volatile.acidity)] <- mean(df$volatile.acidity, na.rm = T)
df.imp$vintage[is.na(df.imp$vintage)] <- mean(df$vintage, na.rm = T)
df.imp$pH[is.na(df.imp$pH)] <- mean(df$pH, na.rm = T)

# checking to make sure imputation worked
missmap(df.imp, main = "Missing Values after imputation")

#### PARTITIONING THE DATA ####
set.seed(919)
partition <- sample(1:2, size = nrow(df.imp), prob = c(.7,.30), replace = T)

# Create a training and validation set from the original data frame 
df_train <- df.imp[partition == 1, ]    # subset the grade data frame to training indices only
df_valid <- df.imp[partition == 2, ]  # subset the grade data frame to validation indices only
df_valid_og <- df_valid[,1:15]

#### TREE BASED CLASSIFICATION MODELING ####
chall.RF <- randomForest(factor(good.wine)~.-quality-residual.sugar, df.imp, ntree=150)


tree.fit <- rpart(good.wine ~ .-quality, df_train, method = "class")
summary(tree.fit)
plotcp(tree.fit)
plot(tree.fit)

#### FITTING LOGISTIC REGRESSION #####

good.model <- glm(good.wine~.-quality-residual.sugar, family = binomial(link='logit'), 
              data = df_train)
summary(good.model)

df_train$good_prob <- predict(df_train$good_pred,type = "response")

#### ASSESSING LOGISTIC REGRESSION ####
fit.df <- predict(logres, newdata = df_valid_og, type = "response")
fit.df <- ifelse(fit.df > 0.5,1,0)
fit.df

misClasificError <- mean(fit.df != df_valid$good.wine)
print(paste('Accuracy',1-misClasificError))

library(ROCR)
pr <- prediction(fit.df, df_valid$good.wine)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
