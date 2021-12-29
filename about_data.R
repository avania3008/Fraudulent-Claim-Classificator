library(dplyr)
library(tidyr)
library(corrplot)
library(stringr)
library(ggplot2)
library(caret)
library(outliers)
library(reshape2)
library(lattice)
library(psych)

raw.train <- read.csv("data/training.csv", sep = ",", header = T)
raw.test <- read.csv("data/test_2021.csv", sep = ",", header = T)

str(raw.train)
str(raw.test)

#missing values (train & test) : marital_status, witness_present_ind, claim_est_payout, age_of_vehicle
summary(is.na(raw.train))
summary(is.na(raw.test))

#get mode function
getmode <- function(x){
  u <- unique(x)
  m <- u[which.max(tabulate(match(x, u)))]
  return(m)
}

impute_convert <- function(dataset){
  for(i in 1:ncol(dataset)){
    len <- length(levels(factor(dataset[[i]])))
    if(class(dataset[[i]]) == "character" || len <= 5){
      #convert char ke factor
      dataset[[i]] <- as.factor(dataset[[i]])
      #kalau ada missing value
      if(sum(is.na(dataset[[i]])) > 0){
        na.cond <- which(is.na(dataset[[i]]))
        na.rows <- as.numeric(rownames(dataset[na.cond,]))
        dataset[na.rows,i] <- getmode(dataset[[i]])
      } 
    } else { #data numeric
      if(sum(is.na(dataset[[i]])) > 0){
        na.cond <- which(is.na(dataset[[i]]))
        na.rows <- as.numeric(rownames(dataset[na.cond,]))
        dataset[na.rows,i] <- median(dataset[[i]], na.rm = T)
      }
    }
  }
  return(dataset)
}

train <- impute_convert(raw.train)
summary(is.na(train))
str(train)

test <- impute_convert(raw.test)
summary(is.na(test))
str(test)

#ubah zip code ke factor
train$zip_code <- as.factor(train$zip_code)
test$zip_code <- as.factor(test$zip_code)

#cek keseimbangan target values di data train
summary(train$fraud)
prop.table(table(train$fraud))

#tabel summary statistics
#1. Data Train
#a. Numeric
tr.num <- unlist(lapply(train, is.numeric))
sum.num.train <- do.call(cbind, lapply(train[,tr.num], summary))
sum.num.train.df <- as.data.frame(round(t(sum.num.train),2))

#b. Categoric
categorical <- function(feature){
  return(list("Number of Levels" = nlevels(feature),
              "Levels" = levels(feature),
              "Levels' Count" = summary(feature)))
}

concat <- function(vector){
  result <- vector[1]
  for(i in 2:length(vector)){
    result <- paste(result,vector[i],sep = ",\n")
  }
  return(result)
}

tidy_levels <- function(df){
  for(i in 1:nrow(df)){
    for(j in 2:3){
      df[i,j] <- concat(unlist(df[i,j]))
    }
  }
  return(df)
}

tr.cat <- unlist(lapply(train, is.factor))
sum.cat.train <- do.call(rbind, lapply(train[,tr.cat], categorical))
sum.cat.train.df <- tidy_levels(as.data.frame(sum.cat.train))

tr.zip <- train$zip_code
tr.zip.df <- data.frame("Levels" = names(summary(tr.zip)),
                        "Levels' Count" = summary(tr.zip))
rownames(tr.zip.df) <- 1:nrow(tr.zip.df)

tr.date <- train$claim_date
tr.date.df <- data.frame("Levels" = names(summary(tr.date)),
                        "Levels' Count" = summary(tr.date))
rownames(tr.date.df) <- 1:nrow(tr.date.df)

#2. Data Test
#a. Numeric
ts.num <- unlist(lapply(test, is.numeric))
sum.num.test <- do.call(cbind, lapply(test[,ts.num], summary))
sum.num.test.df <- as.data.frame(round(t(sum.num.test),2))

#b. Categoric
ts.cat <- unlist(lapply(test, is.factor))
sum.cat.test <- do.call(rbind, lapply(test[,ts.cat], categorical))
sum.cat.test.df <- tidy_levels(as.data.frame(sum.cat.test))

ts.zip <- test$zip_code
ts.zip.df <- data.frame("Levels" = names(summary(ts.zip)),
                        "Levels' Count" = summary(ts.zip))
rownames(ts.zip.df) <- 1:nrow(ts.zip.df)

ts.date <- test$claim_date
ts.date.df <- data.frame("Levels" = names(summary(ts.date)),
                         "Levels' Count" = summary(ts.date))
rownames(ts.date.df) <- 1:nrow(ts.date.df)

#plots for numeric variables
#A. Correlation Plot
corrplot(cor(train[,tr.num]), method = "circle", type = "lower", tl.col = "black", bg = "grey", tl.srt = 0)
heatmap(cor(train[,tr.num]), Rowv = NA, Colv = NA)
pairs.panels(train[,tr.num])

#B. Boxplot
#1. Data Train
num.train <- train[,tr.num]
tr.long <- melt(num.train)
ggplot(tr.long, aes(x = value, y = variable)) + geom_boxplot(colour = "blue", notch = T, fill = "red")

#2. Data Test
num.test <- test[,ts.num]
ts.long <- melt(num.test)
ggplot(ts.long, aes(x = variable, y = value)) + geom_boxplot(colour = "blue", notch = T, fill = "red")

#C. Histogram
#1. Data Train
par(mfrow = c(2,5))
for(i in names(num.train)){
  hist(num.train[[i]], xlab = i, main = paste("Histogram of",i))
}

names(num.train[[2]])

#strip plot (isi kosong), dotplot (isi warna)
#method strip plot -> overplot, jitter, stack
dotchart(num.train$age_of_driver,
           main = "Dot Plot of Age of Driver", 
           col = "blue",
           pch = 2,
           pt.cex = 3)

ggplot(tr.long, aes(x = value, y = variable)) + geom_dotplot()

#plots for categorical variables
#color set : rainbow(), heat.colors(), terrain.colors(), topo.colors(), cm.colors()

#A. Bar Chart
#1. Data Train
cat.train <- train[,tr.cat]
l <- nlevels(cat.train$claim_day_of_week)
library(lattice)
t <- as.data.frame(group_by(train, train[["fraud"]], train[["gender"]]) %>% summarise("Freq" = n()) %>% rename("fraud" = 'train[["fraud"]]',
                                                                                                               "gender" = 'train[["gender"]]'))
tbl <- as.data.frame(train %>% group_by(fraud, gender) %>% summarise("Freq" = n()))

barchart(Freq ~ gender, data = as.data.frame(tbl), 
         groups = fraud,
         main = "Bar Chart of Gender based on Fraud",
         xlab = "Gender",
         stack = F,
         auto.key = T,
         box.ratio = 3)

as.
#B. Pie Chart
pie(summary(cat.train$claim_day_of_week), 
    labels = names(summary(cat.train$claim_day_of_week)),
    main = "Pie Chart of Gender",
    col = rainbow(nlevels(cat.train$claim_day_of_week)))


#cek outliers dengan cook's distance
model <- glm(fraud ~ ., data = c.train, family = "binomial")
cooksd <- cooks.distance(model)
plot(cooksd, pch = "*", col = "black", cex = 2,
     main = "Influential Obs by Cooks Distance")
#garis pembatas
abline(h = 4*mean(cooksd, na.rm = T), col = "red")
#text(x = 1:length(cooksd)+1, y = cooksd, labels = ifelse(cooksd>4*mean(cooksd,na.rm=T), names(cooksd),""), col="red")

#row data yg outlier
outliers <- as.numeric(rownames(c.train[cooksd > 4*mean(cooksd, na.rm = T), ]))
print(outliers)
2074 / nrow(c.train) #8.8% outlier

#DATA PARTITION
#drop columns yg tidak diperlukan : zip_code, claim_date, claim_day_of_week
c.train <- subset(train, select = -c(zip_code, claim_date, claim_day_of_week))
c.test <- subset(test, select = -c(zip_code, claim_date, claim_day_of_week))

#c.train dipisah jadi train model & test model
#proporsi buat target 0 & 1 dibuat sama di train & test
tr.index <- createDataPartition(c.train$fraud, p = 0.7, list = F, times = 1)
tr.mdl <- c.train[tr.index,]
ts.mdl <- c.train[-tr.index,]

prop.table(summary(tr.mdl$fraud))
summary(ts.mdl$fraud)

#MACHINE LEARNING
set.seed(123)
#Logistic Regression
lr <- glm(fraud ~ ., data = tr.mdl, family = "binomial")
summary(lr)
varImp(lr, scale = F)

lr2 <- glm(fraud ~ age_of_driver + gender + marital_status + safty_rating + annual_income
           + high_education_ind + address_change_ind + living_status +accident_site + past_num_of_claims
           + witness_present_ind + age_of_vehicle, data = tr.mdl, family = "binomial")
summary(lr2)

ts.mdl.2 <- subset(ts.mdl, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
                                      high_education_ind,address_change_ind,living_status,accident_site,
                                      past_num_of_claims,witness_present_ind,age_of_vehicle))

pred.prob <- lr2 %>% predict(ts.mdl.2, type = 'response')
pred.class <- ifelse(pred.prob > 0.5, "1", "0")
confusionMatrix(as.factor(pred.class), ts.mdl$fraud)
#accuracy = 84.16% -> median
#accuracy = 84.2% -> mean

#cara lain hitung akurasi model
mean(pred.class == ts.mdl$fraud)

#hasil prediksi dengan data test
c.test.2 <- subset(c.test, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
                                      high_education_ind,address_change_ind,living_status,accident_site,
                                      past_num_of_claims,witness_present_ind,age_of_vehicle))
pred.prob.ts <- lr2 %>% predict(c.test.2, type = "response")
pred.class.ts <- ifelse(pred.prob.ts > 0.5, "1", "0")
res.lr <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.class.ts))
summary(res.lr$fraud)

#cara lain buat pemodelan banyak metode secara langsung

#tr.mdl.2 <- subset(tr.mdl, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
#                                       high_education_ind,address_change_ind,living_status,accident_site,
#                                       past_num_of_claims,witness_present_ind,age_of_vehicle,fraud))
#
#library(caretEnsemble)
#set.seed(123)
#my_ctrl <- trainControl(method = "cv", 
#                        number = 5,
#                        classProbs = TRUE,
#                        savePredictions = "final",
#                        index = createResample(tr.mdl.2$fraud, 3),
#                        sampling = "up",
#                        allowParallel = TRUE)
#
##model list with method Logistic Regression (glm) & Naive Bayes (nb)
##NB error
#model_list <- caretList(make.names(fraud) ~ .,
#                        data = tr.mdl.2,
#                        methodList = c("glm","nb"),
#                        metric = "Kappa",
#                        tuneList = NULL,
#                        continue_on_fail = FALSE,  
#                        preProcess = c("center", "scale"),
#                        trControl = my_ctrl)
#summary(model_list$glm)

#Decision Tree (immune to multicollinearity)
library(party)
set.seed(123)
dt <- ctree(fraud ~ ., data = tr.mdl)
plot(dt)
pred.dt <- predict(dt, ts.mdl)
pred.dt.all <- predict(dt, c.train)

my.input <- data.frame("claim_number" = as.integer(14850),
                 "age_of_driver" = as.integer(120),
                 "gender" = factor("F", levels = levels(c.test$gender)),
                 "marital_status" = factor("0", levels = levels(c.test$marital_status)),
                 "safty_rating" = as.integer(75),
                 "annual_income" = as.integer(37500),
                 "high_education_ind" = factor("0", levels = levels(c.test$high_education_ind)),
                 "address_change_ind"  = factor("0", levels = levels(c.test$address_change_ind)),
                 "living_status"  = factor("Own", levels = levels(c.test$living_status)),
                 "accident_site"  = factor("Highway", levels = levels(c.test$accident_site)),
                 "past_num_of_claims" = as.integer(1),
                 "witness_present_ind" = factor("0", levels = levels(c.test$witness_present_ind)),
                 "liab_prct" = as.integer(50),
                 "channel" = factor("Broker", levels = levels(c.test$channel)),
                 "policy_report_filed_ind" = factor("0", levels = levels(c.test$policy_report_filed_ind)),
                 "claim_est_payout" = as.numeric(5000),
                 "age_of_vehicle" = as.numeric(5),
                 "vehicle_category" = factor("Compact", levels = levels(c.test$vehicle_category)),
                 "vehicle_price" = as.numeric(25000),
                 "vehicle_color" = factor("black", levels = levels(c.test$vehicle_color)),
                 "vehicle_weight" = as.numeric(22000),
                 "fraud" = as.factor("0"))

output <- data.frame(Prediction=predict(dt,my.input), round(predict(dt,my.input, type="prob"), 3), Result = ifelse(as.factor(predict(dt,my.input)) == "0","Not Fraud","Fraud"))
print(output)

act <- ts.mdl$fraud
act.all <- c.train$fraud

confusionMatrix(as.factor(pred.dt), act)
confusionMatrix(as.factor(pred.dt.all), act.all)

dt.pred.df <- data.frame(ts.mdl[,-length(ts.mdl)], actual = act, predicted = pred.dt)
View(dt.pred.df)
#84.29%
dt.pred.df[dt.pred.df$predicted == 1,c(22,23)]
pred.ts.dt <- predict(dt, c.test)
res.dt <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.dt))
summary(res.dt$fraud)


#Random Forest
library(MASS)
library(randomForest)
set.seed(123)
rf <- randomForest(fraud ~ ., data = tr.mdl)
plot(rf)
pred.rf <- predict(rf, ts.mdl)
confusionMatrix(as.factor(pred.rf), act)
#84.42%

#hasil prediksi dengan data test
pred.ts.rf <- predict(rf, c.test)
res.rf <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.rf))
summary(res.rf$fraud)

#Naive Bayes
library(naivebayes)
nb <- naive_bayes(fraud ~ ., data = tr.mdl)
                  #  age_of_driver + gender + marital_status + safty_rating + annual_income
                  #+ high_education_ind + address_change_ind + living_status +accident_site 
                  #+ past_num_of_claims + witness_present_ind + age_of_vehicle, data = tr.mdl)
plot(nb)
pred.nb <- predict(nb, ts.mdl)
confusionMatrix(as.factor(pred.nb), act)
#83.75

pred.ts.nb <- predict(nb, c.test)
res.nb <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.nb))
summary(res.nb$fraud)

#K-NN
library(class)
library(gmodels)
tr.mdl.knn <- subset(tr.mdl, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
                                        high_education_ind,address_change_ind,living_status,accident_site,
                                        past_num_of_claims,witness_present_ind,age_of_vehicle,fraud))
ts.mdl.knn <- subset(ts.mdl, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
                                        high_education_ind,address_change_ind,living_status,accident_site,
                                        past_num_of_claims,witness_present_ind,age_of_vehicle,fraud))
#karena KNN algoritmanya based on distance, semua variabel diubah jadi numerik
to.num <- function(df){
  for(i in 1:ncol(df)){
    if(class(df[[i]]) == "factor"){
      df[[i]] <- as.numeric(df[[i]])
    }
  }
  return(df)
}

tr.mdl.knn <- to.num(tr.mdl)
ts.mdl.knn <- to.num(ts.mdl)

tr.mdl.knn2 <- to.num(tr.mdl.knn)
ts.mdl.knn2 <- to.num(ts.mdl.knn)

str(tr.mdl.knn)
str(ts.mdl.knn)

tr.mdl.label <- tr.mdl.knn$fraud
ts.mdl.label <- ts.mdl.knn$fraud

t <- as.factor(as.numeric(act))
set.seed(123)
pred.knn3 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.knn$fraud, k=3)
confusionMatrix(pred.knn3, t)
#accuracy = 81.47% -> median
#accuracy = 81.38% -> mean

c.train.num <- to.num(c.train)
c.test.num <- to.num(data.frame(c.test,"fraud" = as.factor("0")))

pred.ts.knn <- knn(train = c.train.num, test = c.test.num, cl = c.train.num$fraud, k=20)
pred.ts.knn <- ifelse(pred.ts.knn == 1,0,1)

res.knn <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.knn))
res.knn

pred.knn5 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.label, k=5)
confusionMatrix(pred.knn5, t)
#accuracy = 83.03%
#82.94%

pred.knn10 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.label, k=10)
confusionMatrix(pred.knn10, t)
#accuracy = 83.99%
#84.22%

pred.knn20 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.label, k=20)
confusionMatrix(pred.knn20, t)
#accuracy = 84.35%
#84.35%

pred.knn50 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.label, k=50)
confusionMatrix(pred.knn50, t)
#accuracy = 84.36%
#84.36% 
pred.knn100 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.label, k=100)
confusionMatrix(pred.knn100, t)
#accuracy = 84.36%
#84.36%

mean(pred.knn3 == as.factor(as.numeric(ts.mdl$fraud))) #0.8049278
mean(pred.knn5 == as.factor(as.numeric(ts.mdl$fraud))) #0.8260467
mean(pred.knn10 == as.factor(as.numeric(ts.mdl$fraud))) #0.8388292
mean(pred.knn20 == as.factor(as.numeric(ts.mdl$fraud))) #0.8436458
mean(pred.knn50 == as.factor(as.numeric(ts.mdl$fraud))) #0.8436458
mean(pred.knn100 == as.factor(as.numeric(ts.mdl$fraud))) #0.8436458

pred.knn <- ifelse(pred.knn20 == 1, 0, 1)

knn.pred.df <- data.frame(ts.mdl[,-length(ts.mdl)], actual = act, predicted = as.factor(pred.knn))
res.knn <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.nb))
res.knn
summary(res.nb$fraud)

my.input.num <- to.num(my.input)
knn.pred.new <- knn(train = c.train.num, test = my.input.num, cl = c.train.num$fraud, k=20)
knn.pred.new <- ifelse(knn.pred.new == 1,0,1)

output <- data.frame(Prediction=knn.pred.new, Result = ifelse(as.factor(knn.pred.new) == "0","Not Fraud","Fraud"))
print(output)

