server <- function(input, output){
  #functions
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
  
  to.num <- function(df){
    for(i in 1:ncol(df)){
      if(class(df[[i]]) == "factor"){
        df[[i]] <- as.numeric(df[[i]])
      }
    }
    return(df)
  }
  
  #import & manipulate data
  raw.train <- read.csv("data/training.csv", sep = ",", header = T)
  raw.test <- read.csv("data/test_2021.csv", sep = ",", header = T)
  
  train <- impute_convert(raw.train)
  test <- impute_convert(raw.test)
  
  train$zip_code <- as.factor(train$zip_code)
  test$zip_code <- as.factor(test$zip_code)
  
  c.train <- subset(train, select = -c(claim_number, zip_code, claim_date, claim_day_of_week))
  c.test <- subset(test, select = -c(claim_number, zip_code, claim_date, claim_day_of_week))
  
  tr.index <- createDataPartition(c.train$fraud, p = 0.7, list = F, times = 1)
  tr.mdl <- c.train[tr.index,]
  ts.mdl <- c.train[-tr.index,]
  
  #tab_data
  output$print.train <- renderDT(
    raw.train, options = list(scrollX = TRUE)
  )
  
  output$print.test <- renderDT(
    raw.test, options = list(scrollX = TRUE)
  )
  
  #tab_summary
  tr.num <- unlist(lapply(train, is.numeric))
  sum.num.train <- do.call(cbind, lapply(train[,tr.num], summary))
  sum.num.train.df <- as.data.frame(round(t(sum.num.train),2))
  
  output$print.sum.num.train <- renderDT(
    sum.num.train.df, options = list(scrollX = TRUE)
  )
  
  tr.cat <- unlist(lapply(train, is.factor))
  tr.no.zip.date <- train[,tr.cat]
  tr.no.zip.date <- subset(tr.no.zip.date, select = -c(zip_code, claim_date))
  sum.cat.train <- do.call(rbind, lapply(tr.no.zip.date, categorical))
  sum.cat.train.df <- tidy_levels(as.data.frame(sum.cat.train))
  
  output$print.sum.cat.train <- renderDT(
    sum.cat.train.df, options = list(scrollX = TRUE)
  )
  
  tr.zip <- train$zip_code
  tr.zip.df <- data.frame("Levels" = names(summary(tr.zip)),
                          "Levels' Count" = summary(tr.zip))
  rownames(tr.zip.df) <- 1:nrow(tr.zip.df)
  
  output$print.tr.zip <- renderDT(
    tr.zip.df, options = list(scrollX = TRUE)
  )
  
  tr.date <- train$claim_date
  tr.date.df <- data.frame("Levels" = names(summary(tr.date)),
                           "Levels' Count" = summary(tr.date))
  rownames(tr.date.df) <- 1:nrow(tr.date.df)
  
  output$print.tr.date <- renderDT(
    tr.date.df, options = list(scrollX = TRUE)
  )
  
  ts.num <- unlist(lapply(test, is.numeric))
  sum.num.test <- do.call(cbind, lapply(test[,ts.num], summary))
  sum.num.test.df <- as.data.frame(round(t(sum.num.test),2))
  
  output$print.sum.num.test <- renderDT(
    sum.num.test.df, options = list(scrollX = TRUE)
  )
  
  ts.cat <- unlist(lapply(test, is.factor))
  ts.no.zip.date <- test[,ts.cat]
  ts.no.zip.date <- subset(ts.no.zip.date, select = -c(zip_code, claim_date))
  sum.cat.test <- do.call(rbind, lapply(ts.no.zip.date, categorical))
  sum.cat.test.df <- tidy_levels(as.data.frame(sum.cat.test))
  
  output$print.sum.cat.test <- renderDT(
    sum.cat.test.df, options = list(scrollX = TRUE)
  )
  
  ts.zip <- test$zip_code
  ts.zip.df <- data.frame("Levels" = names(summary(ts.zip)),
                          "Levels' Count" = summary(ts.zip))
  rownames(ts.zip.df) <- 1:nrow(ts.zip.df)
  
  output$print.ts.zip <- renderDT(
    ts.zip.df, options = list(scrollX = TRUE)
  )
  
  ts.date <- test$claim_date
  ts.date.df <- data.frame("Levels" = names(summary(ts.date)),
                           "Levels' Count" = summary(ts.date))
  rownames(ts.date.df) <- 1:nrow(ts.date.df)
  
  output$print.ts.date <- renderDT(
    ts.date.df, options = list(scrollX = TRUE)
  )
  
  #tab_plot
  output$corr.train <- renderPlot({
    corrplot(cor(train[,tr.num]), method = input$corr.tr.method, type = input$corr.tr.type,
             tl.col = as.character(input$corr.tr.lb), 
             bg = as.character(input$corr.tr.bg), 
             tl.srt = as.numeric(input$corr.tr.lb.alt))
  })
  
  output$corr.test <- renderPlot({
    corrplot(cor(test[,ts.num]), method = input$corr.ts.method, type = input$corr.ts.type,
             tl.col = as.character(input$corr.ts.lb), 
             bg = as.character(input$corr.ts.bg), 
             tl.srt = as.numeric(input$corr.ts.lb.alt))
  })
  
  output$box.train <- renderPlot({
    var <- input$box.tr.var
    if(var == "all"){
      num.train <- train[,tr.num]
      tr.long <- melt(num.train)
      if(as.logical(input$box.tr.orient) == TRUE){
        ggplot(tr.long, aes(x = value, y = variable)) + geom_boxplot(colour = as.character(input$box.tr.border), 
                       notch = as.logical(input$box.tr.notch), 
                       fill = as.character(input$box.tr.color))
      } else {
        ggplot(tr.long, aes(x = variable, y = value)) + geom_boxplot(colour = as.character(input$box.tr.border), 
                       notch = as.logical(input$box.tr.notch), 
                       fill = as.character(input$box.tr.color))
      }
    } else {
      x <- train[[var]]
      x <- na.omit(x)
      
      boxplot(x, main = paste("Boxplot of",var),notch = as.logical(input$box.tr.notch),
              horizontal = as.logical(input$box.tr.orient),
              col = as.character(input$box.tr.color),
              border = as.character(input$box.tr.border))
    }
  })
  
  output$box.test <- renderPlot({
    var <- input$box.ts.var
    if(var == "all"){
      num.test <- test[,ts.num]
      ts.long <- melt(num.test)
      if(as.logical(input$box.ts.orient) == TRUE){
        ggplot(ts.long, aes(x = value, y = variable)) + geom_boxplot(colour = as.character(input$box.ts.border), 
                                                                     notch = as.logical(input$box.ts.notch), 
                                                                     fill = as.character(input$box.ts.color))
      } else {
        ggplot(ts.long, aes(x = variable, y = value)) + geom_boxplot(colour = as.character(input$box.ts.border), 
                                                                     notch = as.logical(input$box.ts.notch), 
                                                                     fill = as.character(input$box.ts.color))
      }
    } else {
      x <- test[[var]]
      x <- na.omit(x)
      
      boxplot(x, main = paste("Boxplot of",var),notch = as.logical(input$box.ts.notch),
              horizontal = as.logical(input$box.ts.orient),
              col = as.character(input$box.ts.color),
              border = as.character(input$box.ts.border))
    }
  })
  
  output$hist.train <- renderPlot({
    var <- input$hist.tr.var
    if(var == "all"){
      par(mfrow = c(2,5))
      num.train <- train[,tr.num]
      for(i in names(num.train)){
        x <- num.train[[i]]
        x <- na.omit(x) 
        bins <- seq(min(x), max(x), length.out = input$tr.bins + 1)
        hist(x, breaks = bins,
             xlab = i, main = paste("Histogram of",i),
             col = as.character(input$hist.tr.color), 
             border = as.character(input$hist.tr.border))
      }
    } else {
      x <- train[[var]]
      x <- na.omit(x)
      bins <- seq(min(x), max(x), length.out = input$tr.bins + 1)
      
      hist(x, breaks = bins, col = as.character(input$hist.tr.color), 
           border = as.character(input$hist.tr.border), xlab = var,
           main = paste("Histogram of",var))
    }
  })
  
  output$hist.test <- renderPlot({
    var <- input$hist.ts.var
    if(var == "all"){
      par(mfrow = c(2,5))
      num.test <- test[,ts.num]
      for(i in names(num.test)){
        x <- num.test[[i]]
        x <- na.omit(x) 
        bins <- seq(min(x), max(x), length.out = input$ts.bins + 1)
        hist(x, breaks = bins,
             xlab = i, main = paste("Histogram of",i),
             col = as.character(input$hist.ts.color), 
             border = as.character(input$hist.ts.border))
      }
    } else {
      x <- test[[var]]
      x <- na.omit(x)
      bins <- seq(min(x), max(x), length.out = input$ts.bins + 1)
      
      hist(x, breaks = bins, col = as.character(input$hist.ts.color), 
           border = as.character(input$hist.ts.border), xlab = var,
           main = paste("Histogram of",var))
    }
  })
  
  output$bar.train <- renderPlot({
    var <- input$bar.tr.var
    x <- train[[var]]
    x <- na.omit(x)
    
    if(input$bar.tr.groupby == FALSE){
      barchart(x, main = paste("Bar Chart of",var),
               horizontal = as.logical(input$bar.tr.orient), 
               col = input$bar.tr.color, 
               border = input$bar.tr.border, 
               box.ratio = input$bar.tr.ratio)
    } else {
      tbl <- as.data.frame(group_by(train, train[["fraud"]], x) %>% summarise("Freq" = n()) %>% rename("fraud" = 'train[["fraud"]]', var = x))
      barchart(Freq ~ var, data = tbl, 
               groups = fraud,
               main = paste("Bar Chart of",var,"based on Fraud"),
               horizontal = as.logical(input$bar.tr.orient),
               box.ratio = input$bar.tr.ratio,
               auto.key = T)
    }
  })
  
  output$bar.test <- renderPlot({
    var <- input$bar.ts.var
    x <- test[[var]]
    x <- na.omit(x)
    
    barchart(x, main = paste("Bar Chart of",var),
             horizontal = as.logical(input$bar.ts.orient), 
             col = input$bar.ts.color, 
             border = input$bar.ts.border, 
             box.ratio = input$bar.ts.ratio)
  })
  
  output$pie.train <- renderPlot({
    var <- input$pie.tr.var
    x <- train[[var]]
    x <- na.omit(x)
    n <- nlevels(x)
    col.set <- switch(input$pie.tr.colorset,
                      "rainbow" = rainbow(n),
                      "heat" = heat.colors(n),
                      "terrain" = terrain.colors(n),
                      "topo" = topo.colors(n),
                      "cm" = cm.colors(n))
    pie(summary(x), 
        labels = names(summary(x)),
        main = paste("Pie Chart of",var),
        col = col.set)
  })
  
  output$pie.test <- renderPlot({
    var <- input$pie.ts.var
    x <- test[[var]]
    x <- na.omit(x)
    n <- nlevels(x)
    col.set <- switch(input$pie.ts.colorset,
                      "rainbow" = rainbow(n),
                      "heat" = heat.colors(n),
                      "terrain" = terrain.colors(n),
                      "topo" = topo.colors(n),
                      "cm" = cm.colors(n))
    pie(summary(x), 
        labels = names(summary(x)),
        main = paste("Pie Chart of",var),
        col = col.set)
  })
  
  #tab_preprocessing
  output$str.train <- renderPrint(str(raw.train))
  output$str.test <- renderPrint(str(raw.test))
  
  output$miss.train <- renderPrint(summary(is.na(raw.train)))
  output$miss.test <- renderPrint(summary(is.na(raw.test)))
  
  output$new.str.train <- renderPrint(str(train))
  output$new.str.test <- renderPrint(str(test))
  
  output$new.miss.train <- renderPrint(summary(is.na(train)))
  output$new.miss.test <- renderPrint(summary(is.na(test)))
  
  model <- glm(fraud ~ ., data = c.train, family = "binomial")
  cooksd <- cooks.distance(model)
  
  output$outliers.plot <- renderPlot({
    plot(cooksd, pch = "*", col = "black", cex = 2,
         main = "Influential Obs by Cooks Distance")
    abline(h = 4*mean(cooksd, na.rm = T), col = "red")
  })
  
  output$outliers <- renderPrint(as.numeric(rownames(c.train[cooksd > 4*mean(cooksd, na.rm = T), ])))
  
  output$tr.fraud <- renderPrint(prop.table(summary(tr.mdl$fraud)))
  output$ts.fraud <- renderPrint(prop.table(summary(ts.mdl$fraud)))
  
  #tab_prediction_res
  #Decision Tree
  set.seed(123)
  dt <- ctree(fraud ~ ., data = tr.mdl)
  
  output$dt.plot <- renderPlot({plot(dt)})
  
  pred.dt <- predict(dt, ts.mdl)
  act <- ts.mdl$fraud
  
  output$cm.dt <- renderPrint(confusionMatrix(as.factor(pred.dt), act))
  
  dt.pred.df <- data.frame(ts.mdl[,-length(ts.mdl)], actual = act, predicted = pred.dt)
  
  output$dt.pred <- renderDT(
    dt.pred.df, options = list(scrollX = TRUE)
  )
  
  dt.all <- ctree(fraud ~ ., data = c.train)
  pred.ts.dt <- predict(dt.all, c.test)
  res.dt <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.dt))
  
  output$dt.df <- renderDT(
    res.dt, options = list(scrollX = TRUE)
  )
  
  #KNN
  tr.mdl.knn <- to.num(tr.mdl)
  ts.mdl.knn <- to.num(ts.mdl)
  
  tr.mdl.label <- tr.mdl.knn$fraud
  ts.mdl.label <- ts.mdl.knn$fraud
  
  set.seed(123)
  
  pred.knn20 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.label, k=20)
  pred.knn <- ifelse(pred.knn20 == 1, 0, 1)
  knn.pred.df <- data.frame(ts.mdl[,-length(ts.mdl)], actual = act, predicted = as.factor(pred.knn))
  
  output$knn.pred <- renderDT(
    knn.pred.df, options = list(scrollX = TRUE)
  )
  
  c.train.num <- to.num(c.train)
  c.test.num <- to.num(data.frame(c.test,"fraud" = as.factor("0")))
  
  pred.ts.knn <- knn(train = c.train.num, test = c.test.num, cl = c.train.num$fraud, k=20)
  pred.ts.knn <- ifelse(pred.ts.knn == 1,0,1)
  
  res.knn <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.knn))
  res.knn
  
  output$knn.df <- renderDT(
    res.knn, options = list(scrollX = TRUE)
  )
  
  #Logistic Regression
  set.seed(123)
  
  lr <- glm(fraud ~ ., data = tr.mdl, family = "binomial")
  
  output$lr.sum.1 <- renderPrint(summary(lr))
  output$var.imp.1 <- renderPrint(varImp(lr, scale = F))
  
  lr2 <- glm(fraud ~ age_of_driver + gender + marital_status + safty_rating + annual_income
             + high_education_ind + address_change_ind + living_status +accident_site + past_num_of_claims
             + witness_present_ind + age_of_vehicle, data = tr.mdl, family = "binomial")
  
  output$lr.sum.2 <- renderPrint(summary(lr2))
  output$var.imp.2 <- renderPrint(varImp(lr2, scale = F))
  
  ts.mdl.2 <- subset(ts.mdl, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
                                        high_education_ind,address_change_ind,living_status,accident_site,
                                        past_num_of_claims,witness_present_ind,age_of_vehicle))
  
  pred.prob <- lr2 %>% predict(ts.mdl.2, type = 'response')
  pred.class <- ifelse(pred.prob > 0.5, "1", "0")
  
  output$cm.lr <- renderPrint(confusionMatrix(as.factor(pred.class), act))
  
  lr.pred.df <- data.frame(ts.mdl[,-length(ts.mdl)], actual = act, predicted = as.factor(pred.class))
  
  output$lr.pred <- renderDT(
    lr.pred.df, options = list(scrollX = TRUE)
  )
  
  c.test.2 <- subset(c.test, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
                                        high_education_ind,address_change_ind,living_status,accident_site,
                                        past_num_of_claims,witness_present_ind,age_of_vehicle))
  
  lr2.new <- glm(fraud ~ age_of_driver + gender + marital_status + safty_rating + annual_income
                 + high_education_ind + address_change_ind + living_status +accident_site + past_num_of_claims
                 + witness_present_ind + age_of_vehicle, data = c.train, family = "binomial")
  pred.prob.ts <- lr2.new %>% predict(c.test.2, type = "response")
  pred.class.ts <- ifelse(pred.prob.ts > 0.5, "1", "0")
  res.lr <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.class.ts))
  
  output$lr.df <- renderDT(
    res.lr, options = list(scrollX = TRUE)
  )
  
  #Naive Bayes
  set.seed(123)
  nb <- naive_bayes(fraud ~ ., data = tr.mdl)
  
  output$nb.plot <- renderPlot({plot(nb)})
  
  pred.nb <- predict(nb, ts.mdl)
  act <- ts.mdl$fraud
  
  output$cm.nb <- renderPrint(confusionMatrix(as.factor(pred.nb), act))
  
  nb.pred.df <- data.frame(ts.mdl[,-length(ts.mdl)], actual = act, predicted = pred.nb)
  
  output$nb.pred <- renderDT(
    nb.pred.df, options = list(scrollX = TRUE)
  )
  
  nb.all <- naive_bayes(fraud ~ ., data = c.train)
  pred.ts.nb <- predict(nb.all, c.test)
  res.nb <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.nb))
  
  output$nb.df <- renderDT(
    res.nb, options = list(scrollX = TRUE)
  )
  
  #Random Forest
  set.seed(123)
  rf <- randomForest(fraud ~ ., data = tr.mdl)
  
  output$rf.plot <- renderPlot({plot(rf)})
  
  pred.rf <- predict(rf, ts.mdl)
  act <- ts.mdl$fraud
  
  output$cm.rf <- renderPrint(confusionMatrix(as.factor(pred.rf), act))
  
  rf.pred.df <- data.frame(ts.mdl[,-length(ts.mdl)], actual = act, predicted = pred.rf)
  
  output$rf.pred <- renderDT(
    rf.pred.df, options = list(scrollX = TRUE)
  )
  
  rf.all <- randomForest(fraud ~ ., data = c.train)
  pred.ts.rf <- predict(rf.all, c.test)
  res.rf <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.rf))
  
  output$rf.df <- renderDT(
    res.rf, options = list(scrollX = TRUE)
  )
  
  #tab_predictor
  #Decision Tree
  dt.prediction <- reactive({
    my.input <- data.frame("age_of_driver" = as.integer(input$dt.age_of_driver),
                           "gender" = factor(input$dt.gender, levels = levels(c.test$gender)),
                           "marital_status" = factor(input$dt.marital_status, levels = levels(c.test$marital_status)),
                           "safty_rating" = as.integer(input$dt.safty_rating),
                           "annual_income" = as.integer(input$dt.annual_income),
                           "high_education_ind" = factor(input$dt.high_education_ind, levels = levels(c.test$high_education_ind)),
                           "address_change_ind"  = factor(input$dt.address_change_ind, levels = levels(c.test$address_change_ind)),
                           "living_status"  = factor(input$dt.living_status, levels = levels(c.test$living_status)),
                           "accident_site"  = factor(input$dt.accident_site, levels = levels(c.test$accident_site)),
                           "past_num_of_claims" = as.integer(input$dt.past_num_of_claims),
                           "witness_present_ind" = factor(input$dt.witness_present_ind, levels = levels(c.test$witness_present_ind)),
                           "liab_prct" = as.integer(input$dt.liab_prct),
                           "channel" = factor(input$dt.channel, levels = levels(c.test$channel)),
                           "policy_report_filed_ind" = factor(input$dt.policy_report_filed_ind, levels = levels(c.test$policy_report_filed_ind)),
                           "claim_est_payout" = as.numeric(input$dt.claim_est_payout),
                           "age_of_vehicle" = as.numeric(input$dt.age_of_vehicle),
                           "vehicle_category" = factor(input$dt.vehicle_category, levels = levels(c.test$vehicle_category)),
                           "vehicle_price" = as.numeric(input$dt.vehicle_price),
                           "vehicle_color" = factor(input$dt.vehicle_color, levels = levels(c.test$vehicle_color)),
                           "vehicle_weight" = as.numeric(input$dt.vehicle_weight))
    
    output <- data.frame(Prediction=predict(dt.all,my.input), Result = ifelse(as.factor(predict(dt.all,my.input)) == "0","Not Fraud","Fraud"))
    print(output)
  })
  
  output$dt.status <- renderText({
    if (input$dt.predict.btn>0) { 
      isolate("Decision Tree Prediction Results :") 
    } else {
      return("Predictor is ready to use! Change the features values and press \"Predict\" button")
    }
  })
  
  output$dt.output <- renderTable({
    if (input$dt.predict.btn>0) { 
      isolate(dt.prediction()) 
    } 
  })
  
  #KNN
  knn.prediction <- reactive({
    my.input <- data.frame("age_of_driver" = as.integer(input$knn.age_of_driver),
                           "gender" = factor(input$knn.gender, levels = levels(c.test$gender)),
                           "marital_status" = factor(input$knn.marital_status, levels = levels(c.test$marital_status)),
                           "safty_rating" = as.integer(input$knn.safty_rating),
                           "annual_income" = as.integer(input$knn.annual_income),
                           "high_education_ind" = factor(input$knn.high_education_ind, levels = levels(c.test$high_education_ind)),
                           "address_change_ind"  = factor(input$knn.address_change_ind, levels = levels(c.test$address_change_ind)),
                           "living_status"  = factor(input$knn.living_status, levels = levels(c.test$living_status)),
                           "accident_site"  = factor(input$knn.accident_site, levels = levels(c.test$accident_site)),
                           "past_num_of_claims" = as.integer(input$knn.past_num_of_claims),
                           "witness_present_ind" = factor(input$knn.witness_present_ind, levels = levels(c.test$witness_present_ind)),
                           "liab_prct" = as.integer(input$knn.liab_prct),
                           "channel" = factor(input$knn.channel, levels = levels(c.test$channel)),
                           "policy_report_filed_ind" = factor(input$knn.policy_report_filed_ind, levels = levels(c.test$policy_report_filed_ind)),
                           "claim_est_payout" = as.numeric(input$knn.claim_est_payout),
                           "age_of_vehicle" = as.numeric(input$knn.age_of_vehicle),
                           "vehicle_category" = factor(input$knn.vehicle_category, levels = levels(c.test$vehicle_category)),
                           "vehicle_price" = as.numeric(input$knn.vehicle_price),
                           "vehicle_color" = factor(input$knn.vehicle_color, levels = levels(c.test$vehicle_color)),
                           "vehicle_weight" = as.numeric(input$knn.vehicle_weight),
                           "fraud" = as.factor("0"))
    my.input.num <- to.num(my.input)
    knn.pred.new <- knn(train = c.train.num, test = my.input.num, cl = c.train.num$fraud, k=20)
    knn.pred.new <- ifelse(knn.pred.new == 1,0,1)
    
    output <- data.frame(Prediction=knn.pred.new, Result = ifelse(as.factor(knn.pred.new) == "0","Not Fraud","Fraud"))
    print(output)
  })
  
  output$knn.status <- renderText({
    if (input$knn.predict.btn>0) { 
      isolate("K-Nearest Neighbor Prediction Results :") 
    } else {
      return("Predictor is ready to use! Change the features values and press \"Predict\" button")
    }
  })
  
  output$knn.output <- renderTable({
    if (input$knn.predict.btn>0) { 
      isolate(knn.prediction()) 
    } 
  })
  
  #Logistic Regression
  lr.prediction <- reactive({
    my.input <- data.frame("age_of_driver" = as.integer(input$lr.age_of_driver),
                           "gender" = factor(input$lr.gender, levels = levels(c.test$gender)),
                           "marital_status" = factor(input$lr.marital_status, levels = levels(c.test$marital_status)),
                           "safty_rating" = as.integer(input$lr.safty_rating),
                           "annual_income" = as.integer(input$lr.annual_income),
                           "high_education_ind" = factor(input$lr.high_education_ind, levels = levels(c.test$high_education_ind)),
                           "address_change_ind"  = factor(input$lr.address_change_ind, levels = levels(c.test$address_change_ind)),
                           "living_status"  = factor(input$lr.living_status, levels = levels(c.test$living_status)),
                           "accident_site"  = factor(input$lr.accident_site, levels = levels(c.test$accident_site)),
                           "past_num_of_claims" = as.integer(input$lr.past_num_of_claims),
                           "witness_present_ind" = factor(input$lr.witness_present_ind, levels = levels(c.test$witness_present_ind)),
                           "liab_prct" = as.integer(input$lr.liab_prct),
                           "channel" = factor(input$lr.channel, levels = levels(c.test$channel)),
                           "policy_report_filed_ind" = factor(input$lr.policy_report_filed_ind, levels = levels(c.test$policy_report_filed_ind)),
                           "claim_est_payout" = as.numeric(input$lr.claim_est_payout),
                           "age_of_vehicle" = as.numeric(input$lr.age_of_vehicle),
                           "vehicle_category" = factor(input$lr.vehicle_category, levels = levels(c.test$vehicle_category)),
                           "vehicle_price" = as.numeric(input$lr.vehicle_price),
                           "vehicle_color" = factor(input$lr.vehicle_color, levels = levels(c.test$vehicle_color)),
                           "vehicle_weight" = as.numeric(input$lr.vehicle_weight))
    
    lr.all <- glm(fraud ~ ., data = c.train, family = "binomial")
    pred.prob <- lr.all %>% predict(my.input, type = 'response')
    pred.class <- ifelse(pred.prob > 0.5, "1", "0")
    
    output <- data.frame(Prediction=as.factor(pred.class), Result = ifelse(as.factor(pred.class) == "0","Not Fraud","Fraud"))
    print(output)
  })
  
  output$lr.status <- renderText({
    if (input$lr.predict.btn>0) { 
      isolate("First Model of Logistic RegressionPrediction Results :") 
    } else {
      return("Predictor is ready to use! Change the features values and press \"Predict\" button")
    }
  })
  
  output$lr.output <- renderTable({
    if (input$lr.predict.btn>0) { 
      isolate(lr.prediction()) 
    } 
  })
  
  lr2.prediction <- reactive({
    my.input <- data.frame("age_of_driver" = as.integer(input$lr2.age_of_driver),
                           "gender" = factor(input$lr2.gender, levels = levels(c.test$gender)),
                           "marital_status" = factor(input$lr2.marital_status, levels = levels(c.test$marital_status)),
                           "safty_rating" = as.integer(input$lr2.safty_rating),
                           "annual_income" = as.integer(input$lr2.annual_income),
                           "high_education_ind" = factor(input$lr2.high_education_ind, levels = levels(c.test$high_education_ind)),
                           "address_change_ind"  = factor(input$lr2.address_change_ind, levels = levels(c.test$address_change_ind)),
                           "living_status"  = factor(input$lr2.living_status, levels = levels(c.test$living_status)),
                           "accident_site"  = factor(input$lr2.accident_site, levels = levels(c.test$accident_site)),
                           "past_num_of_claims" = as.integer(input$lr2.past_num_of_claims),
                           "witness_present_ind" = factor(input$lr2.witness_present_ind, levels = levels(c.test$witness_present_ind)),
                           "age_of_vehicle" = as.numeric(input$lr2.age_of_vehicle))
    lr2.all <- glm(fraud ~ age_of_driver + gender + marital_status + safty_rating + annual_income
                   + high_education_ind + address_change_ind + living_status +accident_site + past_num_of_claims
                   + witness_present_ind + age_of_vehicle, data = c.train, family = "binomial")
    pred.prob <- lr2.all %>% predict(my.input, type = 'response')
    pred.class <- ifelse(pred.prob > 0.5, "1", "0")
    
    output <- data.frame(Prediction=as.factor(pred.class), Result = ifelse(as.factor(pred.class) == "0","Not Fraud","Fraud"))
    print(output)
  })
  
  output$lr2.status <- renderText({
    if (input$lr2.predict.btn>0) { 
      isolate("Second Model of Logistic Regression Prediction Results :") 
    } else {
      return("Predictor is ready to use! Change the features values and press \"Predict\" button")
    }
  })
  
  output$lr2.output <- renderTable({
    if (input$lr2.predict.btn>0) { 
      isolate(lr2.prediction()) 
    } 
  })
  
  #Naive Bayes
  
  nb.prediction <- reactive({
    my.input <- data.frame("age_of_driver" = as.integer(input$nb.age_of_driver),
                           "gender" = factor(input$nb.gender, levels = levels(c.test$gender)),
                           "marital_status" = factor(input$nb.marital_status, levels = levels(c.test$marital_status)),
                           "safty_rating" = as.integer(input$nb.safty_rating),
                           "annual_income" = as.integer(input$nb.annual_income),
                           "high_education_ind" = factor(input$nb.high_education_ind, levels = levels(c.test$high_education_ind)),
                           "address_change_ind"  = factor(input$nb.address_change_ind, levels = levels(c.test$address_change_ind)),
                           "living_status"  = factor(input$nb.living_status, levels = levels(c.test$living_status)),
                           "accident_site"  = factor(input$nb.accident_site, levels = levels(c.test$accident_site)),
                           "past_num_of_claims" = as.integer(input$nb.past_num_of_claims),
                           "witness_present_ind" = factor(input$nb.witness_present_ind, levels = levels(c.test$witness_present_ind)),
                           "liab_prct" = as.integer(input$nb.liab_prct),
                           "channel" = factor(input$nb.channel, levels = levels(c.test$channel)),
                           "policy_report_filed_ind" = factor(input$nb.policy_report_filed_ind, levels = levels(c.test$policy_report_filed_ind)),
                           "claim_est_payout" = as.numeric(input$nb.claim_est_payout),
                           "age_of_vehicle" = as.numeric(input$nb.age_of_vehicle),
                           "vehicle_category" = factor(input$nb.vehicle_category, levels = levels(c.test$vehicle_category)),
                           "vehicle_price" = as.numeric(input$nb.vehicle_price),
                           "vehicle_color" = factor(input$nb.vehicle_color, levels = levels(c.test$vehicle_color)),
                           "vehicle_weight" = as.numeric(input$nb.vehicle_weight))
    
    nb.all <- naive_bayes(fraud ~ ., data = c.train)
    pred.nb <- predict(nb.all, my.input)
    
    output <- data.frame(Prediction=as.factor(pred.nb), Result = ifelse(as.factor(pred.nb) == "0","Not Fraud","Fraud"))
    print(output)
  })
  
  output$nb.status <- renderText({
    if (input$nb.predict.btn>0) { 
      isolate("Naive Bayes Prediction Results :") 
    } else {
      return("Predictor is ready to use! Change the features values and press \"Predict\" button")
    }
  })
  
  output$nb.output <- renderTable({
    if (input$nb.predict.btn>0) { 
      isolate(nb.prediction()) 
    } 
  })
  
  
  #Random Forest
  rf.prediction <- reactive({
    my.input <- data.frame("age_of_driver" = as.integer(input$rf.age_of_driver),
                           "gender" = factor(input$rf.gender, levels = levels(c.test$gender)),
                           "marital_status" = factor(input$rf.marital_status, levels = levels(c.test$marital_status)),
                           "safty_rating" = as.integer(input$rf.safty_rating),
                           "annual_income" = as.integer(input$rf.annual_income),
                           "high_education_ind" = factor(input$rf.high_education_ind, levels = levels(c.test$high_education_ind)),
                           "address_change_ind"  = factor(input$rf.address_change_ind, levels = levels(c.test$address_change_ind)),
                           "living_status"  = factor(input$rf.living_status, levels = levels(c.test$living_status)),
                           "accident_site"  = factor(input$rf.accident_site, levels = levels(c.test$accident_site)),
                           "past_num_of_claims" = as.integer(input$rf.past_num_of_claims),
                           "witness_present_ind" = factor(input$rf.witness_present_ind, levels = levels(c.test$witness_present_ind)),
                           "liab_prct" = as.integer(input$rf.liab_prct),
                           "channel" = factor(input$rf.channel, levels = levels(c.test$channel)),
                           "policy_report_filed_ind" = factor(input$rf.policy_report_filed_ind, levels = levels(c.test$policy_report_filed_ind)),
                           "claim_est_payout" = as.numeric(input$rf.claim_est_payout),
                           "age_of_vehicle" = as.numeric(input$rf.age_of_vehicle),
                           "vehicle_category" = factor(input$rf.vehicle_category, levels = levels(c.test$vehicle_category)),
                           "vehicle_price" = as.numeric(input$rf.vehicle_price),
                           "vehicle_color" = factor(input$rf.vehicle_color, levels = levels(c.test$vehicle_color)),
                           "vehicle_weight" = as.numeric(input$rf.vehicle_weight))
    
    rf.all <- randomForest(fraud ~ ., data = c.train)
    pred.rf <- predict(rf.all, my.input)
    
    output <- data.frame(Prediction=as.factor(pred.rf), Result = ifelse(as.factor(pred.rf) == "0","Not Fraud","Fraud"))
    print(output)
  })
  
  output$rf.status <- renderText({
    if (input$rf.predict.btn>0) { 
      isolate("Random Forest Prediction Results :") 
    } else {
      return("Predictor is ready to use! Change the features values and press \"Predict\" button")
    }
  })
  
  output$rf.output <- renderTable({
    if (input$rf.predict.btn>0) { 
      isolate(rf.prediction()) 
    } 
  })
}
