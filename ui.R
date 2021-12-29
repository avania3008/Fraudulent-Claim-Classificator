library(shiny)
library(shinythemes)
library(shinydashboard)
library(colourpicker)
library(DT)
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
library(party)
library(naivebayes)
library(MASS)
library(randomForest)
library(class)
library(gmodels)

dashboardPage(
  dashboardHeader(
    title = htmltools::img(src = knitr::image_uri("icon/logo2-rm.png"), style = "height:40px; width: 170px;")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "tab_data", icon = icon("table")),
      menuItem("Statistics", tabName = "tab_summary", icon = icon("clipboard-list")),
      menuItem("Plots", tabName = "tab_plot", icon = icon("chart-bar")),
      menuItem("Preprocess", tabName = "tab_preprocess", icon = icon("cogs")),
      menuItem("Prediction Result", tabName = "tab_prediction_res", icon = icon("brain")),
      menuItem("Predictor", tabName = "tab_predictor", icon = icon("atom")),
      menuItem("About", tabName = "tab_about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "tab_data",
        h1("Fraudulent Claim Dataset", style = "font-family: 'cursive';
                                       text-align: center"),
        h3(HTML("<a href='https://www.kaggle.com/surekharamireddy/fraudulent-claim-on-cars-physical-damage'>View Data Source</a>"), 
          style = "font-family: 'cursive';
                   text-align: center"),
        br(),
        HTML("
              <div>
               <blockquote>
                 <p><strong>Context</strong></p>
                 <p style=\"text-align: justify;\">Team is concerned about the fraud detection accuracy 
                 as well as the key drivers that cause fraudulence. Tasked with identifying first-party 
                 physical damage fraudulence and explaining the indicators of fraudulent claims.</p>
                 <br>
                 <p><strong>Acknowledgements</strong></p>
                 <p style=\"text-align: justify;\">We wouldn't be here without the help of others. 
                 If you owe any attributions or thanks, include them here along with any citations 
                 of past research.</p>
                 <br>
                 <p><strong>Inspiration</strong></p>
                 <p style=\"text-align: justify;\">Your data will be in front of the world's largest 
                 data science community. What questions do you want to see answered?</p>
               </blockquote>
              </div>
        "),
        br(),
        tabsetPanel(
          tabPanel("Train Data",
            h2("Train Data", style = "font-family: 'cursive';
                          text-align: center"),
            DTOutput("print.train")
          ),
          
          tabPanel("Test Data",
            h2("Test Data", style = "font-family: 'cursive';
                          text-align: center"),
            DTOutput("print.test"),      
          )
        ),
 
        br(),br(),br()
      ),
      
      tabItem(
        "tab_summary",
        h1("Summary Statistics", style = "font-family: 'cursive';
                                       text-align: center"),
        h3("Summary about data's information", style = "font-family: 'cursive';
                                                        font-style: italic;
                                                        text-align: center"),
        br(),
        tabsetPanel(
          tabPanel("Train Data",
            h2("Summary of Train Data", style = "font-family: 'cursive';
                                text-align: center"),
            h3("Numeric Features", style = "font-family: 'cursive';
                                text-align: center"),
            DTOutput("print.sum.num.train"),
            br(),
            h3("Categorical Features", style = "font-family: 'cursive';
                                text-align: center"),
            DTOutput("print.sum.cat.train"),
            br(),
            h3("Other Categorical Features", style = "font-family: 'cursive'; text-align: center"),
            h4("Zip Code", style = "font-family: 'cursive'; text-align: center"),
            DTOutput("print.tr.zip"),
            br(),
            h4("Claim Date", style = "font-family: 'cursive'; text-align: center"),
            DTOutput("print.tr.date")
          ),
                   
          tabPanel("Test Data",
            h2("Summary of Test Data", style = "font-family: 'cursive';
                                text-align: center"),
            h3("Numeric Features", style = "font-family: 'cursive';
                                text-align: center"),
            DTOutput("print.sum.num.test"),
            br(),
            h3("Categorical Features", style = "font-family: 'cursive';
                                text-align: center"),
            DTOutput("print.sum.cat.test"),
            br(),
            h3("Other Categorical Features", style = "font-family: 'cursive'; text-align: center"),
            h4("Zip Code", style = "font-family: 'cursive'; text-align: center"),
            DTOutput("print.ts.zip"),
            br(),
            h4("Claim Date", style = "font-family: 'cursive'; text-align: center"),
            DTOutput("print.ts.date")
          )
                   
        ),
        
        br(),br(),br()
      ),
      
      tabItem(
        "tab_plot",
        h1("Plots", style = "font-family: 'cursive';
                                       text-align: center"),
        h3("Show the variables' distributions in a plot", style = "font-family: 'cursive';
                                                                  font-style: italic;
                                                                  text-align: center"),
        
        br(),
        tabsetPanel(
          tabPanel("Numerical",
            br(),
            selectInput("plot","Plot Type",
                        c("Correlation Plot" = "corr", "Boxplot" = "box", "Histogram" = "hist"),
                        selected = "corr"
            ),
            hr(),
                   
            conditionalPanel(condition = "input.plot == 'corr'",
              tabsetPanel(
                tabPanel("Train Data",
                  h2("Correlation Plot of Train Data", style = "font-family: 'cursive';
                          text-align: center"),
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      HTML("<h3>Parameters</h3>"),
                      
                      selectInput("corr.tr.method","Method",
                                  c("Circle" = "circle",
                                    "Square" = "square",
                                    "Ellipse" = "ellipse",
                                    "Number" = "number",
                                    "Shade" = "shade",
                                    "Color" = "color",
                                    "Pie" = "pie"
                                  ),
                                  selected = "circle"
                      ),
                      
                      radioButtons("corr.tr.type","Type",
                                   choiceNames = c("Full","Lower","Upper"),
                                   choiceValues = c("full","lower","upper")
                      ),
                      
                      colourInput("corr.tr.lb","Label Color",value = "black"),
                      colourInput("corr.tr.bg","Background Color",value = "grey"),
                      sliderInput("corr.tr.lb.alt","Label Rotation", 
                                  min = 0, max = 90, value = 45),
                      
                      width = 2
                    ),
                    
                    mainPanel(
                      plotOutput("corr.train"),
                      width = 10
                    ),
                    
                    position = "left",
                    fluid = T  
                    
                  )
                         
                ),
                
                tabPanel("Test Data",
                  h2("Correlation Plot of Test Data", style = "font-family: 'cursive';
                          text-align: center"),
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      HTML("<h3>Parameters</h3>"),
                      
                      selectInput("corr.ts.method","Method",
                                  c("Circle" = "circle",
                                    "Square" = "square",
                                    "Ellipse" = "ellipse",
                                    "Number" = "number",
                                    "Shade" = "shade",
                                    "Color" = "color",
                                    "Pie" = "pie"
                                  ),
                                  selected = "circle"
                      ),
                      
                      radioButtons("corr.ts.type","Type",
                                   choiceNames = c("Full","Lower","Upper"),
                                   choiceValues = c("full","lower","upper")
                      ),
                      
                      colourInput("corr.ts.lb","Label Color",value = "black"),
                      colourInput("corr.ts.bg","Background Color",value = "grey"),
                      sliderInput("corr.ts.lb.alt","Label Rotation", 
                                  min = 0, max = 90, value = 45),
                      
                      width = 2
                    ),
                    
                    mainPanel(
                      plotOutput("corr.test"),
                      width = 10
                    ),
                    
                    position = "left",
                    fluid = T  
                    
                  )
                )
                
              ),              
            ),
            
            conditionalPanel(condition = "input.plot == 'box'",
              tabsetPanel(
                tabPanel("Train Data",
                  h2("Boxplot of Train Data", style = "font-family: 'cursive';
                                                       text-align: center"),
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      HTML("<h3>Parameters</h3>"),
                      
                      selectInput("box.tr.var","Numeric Variable",
                                  c("All Numeric Variable" = "all",
                                    "Age of Driver" = "age_of_driver",
                                    "Safty Rating" = "safty_rating",
                                    "Annual Income" = "annual_income",
                                    "Past Num. of Claims" = "past_num_of_claims",
                                    "Liability Percentage" = "liab_prct",
                                    "Claim Est. Payout" = "claim_est_payout",
                                    "Age of Vehicle" = "age_of_vehicle",
                                    "Vehicle Price" = "vehicle_price",
                                    "Vehicle Weight" = "vehicle_weight"
                                  ),
                                  selected = "all"
                      ),
                      
                      selectInput("box.tr.orient","Orientation",
                                  c("Horizontal" = TRUE, "Vertical" = FALSE),
                                  selected = TRUE),
                      radioButtons("box.tr.notch","Notch",
                                   choiceNames = c("Yes","No"),
                                   choiceValues = c(TRUE, FALSE)),
                      colourInput("box.tr.color","Fill", value = "white"),
                      colourInput("box.tr.border","Border", value = "black"),
                      
                      
                      width = 2
                    ),
                    
                    mainPanel(
                      plotOutput("box.train"),
                      width = 10
                    ),
                    
                    position = "left",
                    fluid = T  
                    
                  )
                         
                ),
                
                tabPanel("Test Data",
                  h2("Boxplot of Test Data", style = "font-family: 'cursive';
                                                     text-align: center"),
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      HTML("<h3>Parameters</h3>"),
                      
                      selectInput("box.ts.var","Numeric Variable",
                                  c("All Numeric Variable" = "all",
                                    "Age of Driver" = "age_of_driver",
                                    "Safty Rating" = "safty_rating",
                                    "Annual Income" = "annual_income",
                                    "Past Num. of Claims" = "past_num_of_claims",
                                    "Liability Percentage" = "liab_prct",
                                    "Claim Est. Payout" = "claim_est_payout",
                                    "Age of Vehicle" = "age_of_vehicle",
                                    "Vehicle Price" = "vehicle_price",
                                    "Vehicle Weight" = "vehicle_weight"
                                  ),
                                  selected = "all"
                      ),
                      
                      selectInput("box.ts.orient","Orientation",
                                  c("Horizontal" = TRUE, "Vertical" = FALSE),
                                  selected = TRUE),
                      radioButtons("box.ts.notch","Notch",
                                   choiceNames = c("Yes","No"),
                                   choiceValues = c(TRUE, FALSE)),
                      colourInput("box.ts.color","Fill", value = "white"),
                      colourInput("box.ts.border","Border", value = "black"),
                      
                      
                      width = 2
                    ),
                    
                    mainPanel(
                      plotOutput("box.test"),
                      width = 10
                    ),
                    
                    position = "left",
                    fluid = T  
                    
                  )
                )
                
              )         
            ),
            
            conditionalPanel(condition = "input.plot == 'hist'",
              tabsetPanel(
                tabPanel("Train Data",
                  h2("Histogram of Train Data", style = "font-family: 'cursive';
                                                         text-align: center"),
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      HTML("<h3>Parameters</h3>"),
                      
                      selectInput("hist.tr.var","Numeric Variable",
                                  c("All Numeric Variable" = "all",
                                    "Age of Driver" = "age_of_driver",
                                    "Safty Rating" = "safty_rating",
                                    "Annual Income" = "annual_income",
                                    "Past Num. of Claims" = "past_num_of_claims",
                                    "Liability Percentage" = "liab_prct",
                                    "Claim Est. Payout" = "claim_est_payout",
                                    "Age of Vehicle" = "age_of_vehicle",
                                    "Vehicle Price" = "vehicle_price",
                                    "Vehicle Weight" = "vehicle_weight"
                                  ),
                                  selected = "all"
                      ),
                      
                      sliderInput("tr.bins","Number of Bins",
                                  min = 1,
                                  max = 50,
                                  value = 20
                      ),
                      
                      colourInput("hist.tr.color","Color", value = "#75AADB"),
                      colourInput("hist.tr.border","Border", value = "black"),
                      width = 2
                    ),
                    
                    mainPanel(
                      plotOutput("hist.train"),
                      width = 10
                    ),
                    
                    position = "left",
                    fluid = T  
                    
                  )
                         
                ),
                
                tabPanel("Test Data",
                  h2("Histogram of Test Data", style = "font-family: 'cursive';
                                                       text-align: center"),
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      HTML("<h3>Parameters</h3>"),
                      
                      selectInput("hist.ts.var","Numeric Variable",
                                  c("All Numeric Variable" = "all",
                                    "Age of Driver" = "age_of_driver",
                                    "Safty Rating" = "safty_rating",
                                    "Annual Income" = "annual_income",
                                    "Past Num. of Claims" = "past_num_of_claims",
                                    "Liability Percentage" = "liab_prct",
                                    "Claim Est. Payout" = "claim_est_payout",
                                    "Age of Vehicle" = "age_of_vehicle",
                                    "Vehicle Price" = "vehicle_price",
                                    "Vehicle Weight" = "vehicle_weight"
                                  ),
                                  selected = "all"
                      ),
                      
                      sliderInput("ts.bins","Number of Bins",
                                  min = 1,
                                  max = 50,
                                  value = 20
                      ),
                      
                      colourInput("hist.ts.color","Color", value = "#75AADB"),
                      colourInput("hist.ts.border","Border", value = "black"),
                      width = 2
                    ),
                    
                    mainPanel(
                      plotOutput("hist.test"),
                      width = 10
                    ),
                    
                    position = "left",
                    fluid = T  
                    
                  )
                )
              )               
            )       
          ),
          tabPanel("Categorical",
            br(),
            selectInput("plotcat","Plot Type",
                        c("Bar Chart" = "bar", "Pie Chart" = "pie"),
                        selected = "bar"
            ),
            hr(),
            conditionalPanel(condition = "input.plotcat == 'bar'",
               tabsetPanel(
                 tabPanel("Train Data",
                    h2("Bar Plot of Train Data", style = "font-family: 'cursive';
                                                   text-align: center"),
                    br(),
                    sidebarLayout(
                      sidebarPanel(
                        HTML("<h3>Parameters</h3>"),
                        
                        selectInput("bar.tr.var","Categorical Variable",
                                    c("Gender" = "gender",
                                      "Marital Status" = "marital_status",
                                      "High Education Indication" = "high_education_ind",
                                      "Address Change Indication" = "address_change_ind",
                                      "Living Status" = "living_status",
                                      "Claim Day of Week" = "claim_day_of_week",
                                      "Accident Site" = "accident_site",
                                      "Witness Present Indication" = "witness_present_ind",
                                      "Channel" = "channel",
                                      "Policy Report Filed Indication" = "policy_report_filed_ind",
                                      "Vehicle Category" = "vehicle_category",
                                      "Vehicle Color" = "vehicle_color",
                                      "Fraud" = "fraud"
                                    ),
                                    selected = "gender"
                        ),
                        
                        selectInput("bar.tr.orient","Orientation",
                                    c("Horizontal" = FALSE, "Vertical" = TRUE),
                                    selected = FALSE),
                        
                        sliderInput("bar.tr.ratio","Box Ratio",
                                    min = 1,
                                    max = 20,
                                    value = 3
                        ),
                        
                        checkboxInput("bar.tr.groupby","Group by Target Class", FALSE),
                        colourInput("bar.tr.color","Color", value = "#75AADB"),
                        colourInput("bar.tr.border","Border", value = "black"),               
                        
                        
                        width = 2
                      ),
                      
                      mainPanel(
                        plotOutput("bar.train"),
                        width = 10
                      ),
                      
                      position = "left",
                      fluid = T 
                    )
                    
                 ),
                 tabPanel("Test Data",
                   h2("Bar Plot of Test Data", style = "font-family: 'cursive';
                                            text-align: center"),
                   br(),
                   sidebarLayout(
                     sidebarPanel(
                       HTML("<h3>Parameters</h3>"),
                       
                       selectInput("bar.ts.var","Categorical Variable",
                                   c("Gender" = "gender",
                                     "Marital Status" = "marital_status",
                                     "High Education Indication" = "high_education_ind",
                                     "Address Change Indication" = "address_change_ind",
                                     "Living Status" = "living_status",
                                     "Claim Day of Week" = "claim_day_of_week",
                                     "Accident Site" = "accident_site",
                                     "Witness Present Indication" = "witness_present_ind",
                                     "Channel" = "channel",
                                     "Policy Report Filed Indication" = "policy_report_filed_ind",
                                     "Vehicle Category" = "vehicle_category",
                                     "Vehicle Color" = "vehicle_color",
                                     "Fraud" = "fraud"
                                   ),
                                   selected = "gender"
                       ),
                       
                       selectInput("bar.ts.orient","Orientation",
                                   c("Horizontal" = FALSE, "Vertical" = TRUE),
                                   selected = FALSE),
                       
                       sliderInput("bar.ts.ratio","Box Ratio",
                                   min = 1,
                                   max = 20,
                                   value = 3
                       ),
                    
                       colourInput("bar.ts.color","Color", value = "#75AADB"),
                       colourInput("bar.ts.border","Border", value = "black"),
                       
                       width = 2
                     ),
                     
                     mainPanel(
                       plotOutput("bar.test"),
                       width = 10
                     ),
                     
                     position = "left",
                     fluid = T 
                   )      
                 )
               )              
                             
            ),
            conditionalPanel(condition = "input.plotcat == 'pie'",
              tabsetPanel(
                tabPanel("Train Data",
                  h2("Pie Chart of Train Data", style = "font-family: 'cursive';
                                   text-align: center"),
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      HTML("<h3>Parameters</h3>"),
                      
                      selectInput("pie.tr.var","Categorical Variable",
                                  c("Gender" = "gender",
                                    "Marital Status" = "marital_status",
                                    "High Education Indication" = "high_education_ind",
                                    "Address Change Indication" = "address_change_ind",
                                    "Living Status" = "living_status",
                                    "Claim Day of Week" = "claim_day_of_week",
                                    "Accident Site" = "accident_site",
                                    "Witness Present Indication" = "witness_present_ind",
                                    "Channel" = "channel",
                                    "Policy Report Filed Indication" = "policy_report_filed_ind",
                                    "Vehicle Category" = "vehicle_category",
                                    "Vehicle Color" = "vehicle_color",
                                    "Fraud" = "fraud"
                                  ),
                                  selected = "gender"
                      ),
                      
                      selectInput("pie.tr.colorset","Color Set",
                                  c("Rainbow" = "rainbow",
                                    "Heat Colors" = "heat",
                                    "Terrain Colors" = "terrain",
                                    "Topo Colors" = "topo",
                                    "Pink and Blue" = "cm"),
                                  selected = "rainbow"
                      ),
                      width = 2
                    ),
                    
                    mainPanel(
                      plotOutput("pie.train"),
                      width = 10
                    ),
                    
                    position = "left",
                    fluid = T  
                    
                  )
                         
                ),
                
                tabPanel("Test Data",
                  h2("Pie Chart of Test Data", style = "font-family: 'cursive';
                                   text-align: center"),
                  br(),
                  sidebarLayout(
                    sidebarPanel(
                      HTML("<h3>Parameters</h3>"),
                      
                      selectInput("pie.ts.var","Categorical Variable",
                                  c("Gender" = "gender",
                                    "Marital Status" = "marital_status",
                                    "High Education Indication" = "high_education_ind",
                                    "Address Change Indication" = "address_change_ind",
                                    "Living Status" = "living_status",
                                    "Claim Day of Week" = "claim_day_of_week",
                                    "Accident Site" = "accident_site",
                                    "Witness Present Indication" = "witness_present_ind",
                                    "Channel" = "channel",
                                    "Policy Report Filed Indication" = "policy_report_filed_ind",
                                    "Vehicle Category" = "vehicle_category",
                                    "Vehicle Color" = "vehicle_color",
                                    "Fraud" = "fraud"
                                  ),
                                  selected = "gender"
                      ),
                      
                      selectInput("pie.ts.colorset","Color Set",
                                  c("Rainbow" = "rainbow",
                                    "Heat Colors" = "heat",
                                    "Terrain Colors" = "terrain",
                                    "Topo Colors" = "topo",
                                    "Pink and Blue" = "cm"),
                                  selected = "rainbow"
                      ),
                      width = 2
                    ),
                    
                    mainPanel(
                      plotOutput("pie.test"),
                      width = 10
                    ),
                    
                    position = "left",
                    fluid = T  
                    
                  )
                )
              )
            )
          )
        )
      ),
      
      tabItem(
        "tab_preprocess",
        h1("Preprocessing", style = "font-family: 'cursive';
                                    font-style: italic;
                                    text-align: center"),
        h3("Implemented steps for data preprocessing", 
           style = "font-family: 'cursive';
                   text-align: center"),
        br(), br(),
        div(
          style = "text-align: justify; width: 100%; display: block;",
          h3("1. Import Data"),
          p("Import raw train data and raw test data that have been downloaded from", HTML('<a href="https://www.kaggle.com/surekharamireddy/fraudulent-claim-on-cars-physical-damage">Kaggle (Fraudulent Claim on Cars Physical Damage)</a>.')),
          HTML('
            <blockquote>
              <p>raw.train <- read.csv("data/training.csv", sep = ",", header = T)<br>
              raw.test <- read.csv("data/test_2021.csv", sep = ",", header = T)</p>
            </blockquote>   
          ')
        ),
        div(
          style = "text-align: justify; width: 100%; display: block;",
          h3("2. Data Structure"),
          p("Take a sneak peek for the structures of train data and test data."),
          br(),
          tabsetPanel(
            tabPanel("Train",
              br(),
              HTML('<blockquote>
				              <p>str(raw.train)</p>
 		 	              </blockquote>
              '),
              verbatimTextOutput("str.train")       
            ),
            tabPanel("Test",
              br(),
              HTML('<blockquote>
				              <p>str(raw.test)</p>
 		 	              </blockquote>
              '),
              verbatimTextOutput("str.test")
            )
          )
        ),
        div(
          style = "text-align: justify; width: 100%; display: block;",
          h3("3. Check for Missing Values"),
          p("Check whether there are any missing values in both datasets."),
          br(),
          tabsetPanel(
            tabPanel("Train",
              br(),
              HTML('<blockquote>
				              <p>summary(is.na(raw.train))</p>
 		 	              </blockquote>
              '),
              verbatimTextOutput("miss.train")   
            ),
            tabPanel("Test",
              br(),
              HTML('<blockquote>
				              <p>summary(is.na(raw.test))</p>
 		 	              </blockquote>
              '),
              verbatimTextOutput("miss.test")       
            )
          ),
          p("*TRUE indicates number of missing values, FALSE indicates number of non-missing values"),
          HTML("<p>From the above results, we can say that the features which have missing values from both train data
            and test data are <strong>marital_status</strong>, <strong>witness_present_ind</strong>, <strong>claim_est_payout</strong>, 
               and <strong>age_of_vehicle</strong>.<p>")
        ),
        div(
          style = "text-align: justify; width: 100%; display: block;",
          h3("4. Impute missing values and convert data type"),
          p("Imputing missing values for categorical data with mode and 
            numerical data with median (robust to outliers). 
            Also convert the character data type into factor type. These processes were done using these functions"),
          HTML('
            <blockquote>
              <pre>
  getmode <- function(x){                                         
    u <- unique(x)
    m <- u[which.max(tabulate(match(x, u)))]
    return(m)
  }
  
  impute_convert <- function(dataset){
    for(i in 1:ncol(dataset)){
      len <- length(levels(factor(dataset[[i]])))
      if(class(dataset[[i]]) == "character" || len <= 5){
        dataset[[i]] <- as.factor(dataset[[i]])
        if(sum(is.na(dataset[[i]])) > 0){
          na.cond <- which(is.na(dataset[[i]]))
          na.rows <- as.numeric(rownames(dataset[na.cond,]))
          dataset[na.rows,i] <- getmode(dataset[[i]])<
        } 
      } else {
        if(sum(is.na(dataset[[i]])) > 0){
          na.cond <- which(is.na(dataset[[i]]))
          na.rows <- as.numeric(rownames(dataset[na.cond,]))
          dataset[na.rows,i] <- median(dataset[[i]], na.rm = T)
        }
      }
    }
    return(dataset)
  }
              </pre>
            </blockquote>
          '),
          p("And the imputation and data type conversion for the train data and test data are using these codes"),
          HTML('
            <blockquote>
              <p>train <- impute_convert(raw.train)<br>test <- impute_convert(raw.test)</p>
            </blockquote>
          '),
          p("Here are the results after we do the imputation for missing values and data type conversion :"),
          tabsetPanel(
            tabPanel("Train",
              br(),
              p("Train data's structure after conversion :"),
              HTML('<blockquote>str(train)</blockquote>'),
              verbatimTextOutput("new.str.train"),
              br(),
              p("Train data's missing values after imputation :"),
              HTML('<blockquote>summary(is.na(train))</blockquote>'),
              verbatimTextOutput("new.miss.train")
            ),
            tabPanel("Test",
              br(),
              p("Test data's structure after conversion :"),
              HTML('<blockquote>str(test)</blockquote>'),
              verbatimTextOutput("new.str.test"),
              br(),
              p("Test data's missing values after imputation :"),
              HTML('<blockquote>summary(is.na(test))</blockquote>'),
              verbatimTextOutput("new.miss.test")
            )
          )
        ),
        div(
          style = "text-align: justify; width: 100%; display: block;",
          h3("5. Drop unnecessary features"),
          HTML('<p>The next step is dropping unnecessary features from the dataset. The dropped features are
               <strong>claim_number</strong>, <strong>zip_code</strong>, <strong>claim_date</strong>, and <strong>claim_day_of_week</strong>.
               Drop those features from train data and test data.<p>'),
          HTML('
            <blockquote>
              <p>
                c.train <- subset(train, select = -c(claim_number, zip_code, claim_date, claim_day_of_week))<br>
                c.test <- subset(test, select = -c(claim_number, zip_code, claim_date, claim_day_of_week))
              </p>
            </blockquote>
          ')
        ),
        div(
          style = "text-align: justify; width: 100%; display: block;",
          h3("6. Checking outliers"),
          p("The dataset contains more than one features to classified the fraud indication (multivariate)
            and one of the methods to see outliers from multivariates dataset is using Cook's Distance.
            Here are the results for missing values plot for train data :"),
          HTML('
            <blockquote>
              <p>
              model <- glm(fraud ~ ., data = c.train, family = "binomial") <br>
              cooksd <- cooks.distance(model) <br>
              plot(cooksd, pch = "*", col = "black", cex = 2, main = "Influential Obs by Cooks Distance") <br>
              abline(h = 4*mean(cooksd, na.rm = T), col = "red")
              </p>
            </blockquote>
          '),
          plotOutput("outliers.plot"),
          br(),
          p("Here are the outliers' indexes :"),
          HTML('
            <blockquote>
              <p>
              outliers <- as.numeric(rownames(c.train[cooksd > 4*mean(cooksd, na.rm = T), ])) <br>
              print(outliers)
              </p>
            </blockquote>
          '),
          verbatimTextOutput("outliers")
        ),
        div(
          style = "text-align: justify; width: 100%; display: block;",
          h3("7. Create new train data and test data from subsetted train data"),
          p("Now we have subsetted clean train data and test data. Before we build the Machine Learning models, 
            we need to split the subsetted train data (c.train) into new train data and test data. The new train data used 
            to train the models and the new test data used to score the models. The actual test data (c.test) can be treated 
            as a new data that we want to predict it's class (fraud indication). Here are the codes to split the train data
            into new train data and test data with the same proportion of target values :"),
          HTML('
            <blockquote>
              <p>
              tr.index <- createDataPartition(c.train$fraud, p = 0.7, list = F, times = 1)<br>
              tr.mdl <- c.train[tr.index,]<br>
              ts.mdl <- c.train[-tr.index,]<br>
              </p>
            </blockquote>
          '),
          p("The proportion of new train data is 70% of the actual train data and the new test 
            data has 30% amount of the actual train data. As for the target values, both new train 
            data and test data have the similar proportion of target \"0\" and target \"1\"."),
          HTML('
            <blockquote>
              <p>summary(tr.mdl$fraud)</p>
            </blockquote>
          '),
          verbatimTextOutput("tr.fraud"),
          HTML('
            <blockquote>
              <p>summary(ts.mdl$fraud)</p>
            </blockquote>
          '),
          verbatimTextOutput("ts.fraud"),
        ),
        div(
          style = "text-align: justify; width: 100%; display: block;",
          h3("8. Start to build the models"),
          HTML("<p>Finally we can start to build some models from various Machine Learning methods for classification. 
            The methods that can be used for classification are <strong>Logistic Regression</strong>, <strong>Decision Tree</strong>, 
               <strong>Random Forest</strong>, <strong>Naive Bayes</strong>, and <strong>K-Nearest Neighbor</strong>.</p>")
        ),
        br(),br(),br()
      ),
      
      tabItem(
        "tab_prediction_res",
        h1("Prediction Result", style = "font-family: 'cursive';
                                font-style: italic;
                                text-align: center"),
        h3("Prediction result using several Machine Learning methods", 
           style = "font-family: 'cursive';
                   text-align: center"),
        br(),
        tabsetPanel(
          tabPanel("Decision Tree",
            h2("Prediction using Decision Tree", style = "font-family: 'cursive';
                                                          text-align: center"),
            br(),
            p("Here are the built decision tree model using all features from the new train data (tr.mdl) using seed number '123' :"),
            HTML('
            <blockquote>
              <p>
              set.seed(123)<br>
              dt <- ctree(fraud ~ ., data = tr.mdl) <br>
              plot(dt)
              </p>
            </blockquote>
            '),
            plotOutput("dt.plot"),
            br(),
            p("Then we predict the new test data (ts.mdl) using the Decision Tree model and evaluate the model using Confusion Matrix : "),
            HTML('
            <blockquote>
              <p>
              pred.dt <- predict(dt, ts.mdl) <br>
              act <- ts.mdl$fraud <br>
              confusionMatrix(as.factor(pred.dt), act)
              </p>
            </blockquote>    
            '),
            verbatimTextOutput("cm.dt"),
            br(),
            p("The accuracy of Decision Tree model using all features from the train data is 84.25%*. Here are the prediction result using the new test data (ts.mdl) :"),
            p("*Can be slightly differ from output", style = "color: red"),
            h2("Model's Test Data with Actual and Predicted Output", style = "font-family: 'cursive';
                                                   text-align: center"),
            DTOutput("dt.pred"),
            br(),
            p("Next we can predict the target of actual test data (c.test) using new model that includes all train data (c.train) :"),
            HTML('
              <blockquote>
                <p>
                  dt.all <- ctree(fraud ~ ., data = c.train) <br>
                  pred.ts.dt <- predict(dt.all, c.test) <br>
                  res.dt <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.dt)) <br>
                  res.dt
                 </p>
              </blockquote> 
            '),
            h2("Test Data with Predicted Output", style = "font-family: 'cursive';
                                                   text-align: center"),
            DTOutput("dt.df"),
            br(),br(),br()
          ),
          
          tabPanel("K-Nearest Neighbor",
            h2("Prediction using K-Nearest Neighbor", style = "font-family: 'cursive';
                                                   text-align: center"),
            br(),
            p("KNN is a distance-based algorithm, so we need to convert every single categorical features into numeric values. Thus, we made a function to convert the whole dataset as we can see in below code :"),
            HTML('
            <blockquote>
            <pre>
  to.num <- function(df){
    for(i in 1:ncol(df)){
      if(class(df[[i]]) == "factor"){
        df[[i]] <- as.numeric(df[[i]])
      }
    }
    return(df)
  }
            </pre>
            </blockquote>
            '),
            p("Because we don't want to affect the actual train and test data for the model (tr.mdl & ts.mdl), we assign the function's results to new variables."),
            HTML('
            <blockquote>
              <p>tr.mdl.knn <- to.num(tr.mdl)<br>ts.mdl.knn <- to.num(ts.mdl)</p>
            </blockquote>
            '),
            p("Then we can build the KNN models and predict the results using seed number '123'. There are several k-parameter values that were used to build the model, such as 3, 5, 10, 20, 50, adn 100 :"),
            HTML('
            <blockquote>
              <p>
                set.seed(123) <br>
                pred.knn3 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.knn$fraud, k=3) <br>
                pred.knn5 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.knn$fraud, k=5) <br>
                pred.knn10 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.knn$fraud, k=10) <br>
                pred.knn20 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.knn$fraud, k=20) <br>
                pred.knn50 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.knn$fraud, k=50) <br>
                pred.knn100 <- knn(train = tr.mdl.knn, test = ts.mdl.knn, cl = tr.mdl.knn$fraud, k=100)
              </p>
            </blockquote>
            '),
            p("As for the accuracy of each models :"),
            HTML('
            <blockquote>
              <p>
                #ACCURACY <br>
                mean(pred.knn3 == as.factor(as.numeric(ts.mdl$fraud))) #0.8049278 <br>
                mean(pred.knn5 == as.factor(as.numeric(ts.mdl$fraud))) #0.8260467 <br>
                mean(pred.knn10 == as.factor(as.numeric(ts.mdl$fraud))) #0.8388292 <br>
                mean(pred.knn20 == as.factor(as.numeric(ts.mdl$fraud))) #0.8436458 <br>
                mean(pred.knn50 == as.factor(as.numeric(ts.mdl$fraud))) #0.8436458 <br>
                mean(pred.knn100 == as.factor(as.numeric(ts.mdl$fraud))) #0.8436458
              </p>
            </blockquote>
            '),
            p("From the above result, we can see that the optimum number from the list of 3, 5, 10, 20, 50, and 100 is k = 20, 
              because if we increase the value of k to above 20, the accuracy number doesn't change. So using KNN model with k = 20, 
              we predict the model test data (ts.mdl). Before that, we need to convert the values of actual target and predicted target 
              because when we convert the target from factor to numeric, the values automatically change from 0 to 1 and 1 to 2. 
              This conversion makes the target values into their initial values, which are 1 to 0 and 2 to 1."),
            HTML('
              <blockquote>
                <p>
                  pred.knn <- ifelse(pred.knn20 == 1, 0, 1)
                </p>
              </blockquote>      
            '),
            h2("Model's Test Data with Actual and Predicted Output", style = "font-family: 'cursive';
                                                   text-align: center"),
            DTOutput("knn.pred"),
            br(),
            p("Next we can predict the target of actual test data (c.test) using new model that includes all train data (c.train) :"),
            HTML('
              <blockquote>
                <p>
                  c.train.num <- to.num(c.train) <br>
                  c.test.num <- to.num(data.frame(c.test,"fraud" = as.factor("0"))) <br>
                   <br>
                  pred.ts.knn <- knn(train = c.train.num, test = c.test.num, cl = c.train.num$fraud, k=20) <br>
                  pred.ts.knn <- ifelse(pred.ts.knn == 1,0,1)  <br>
                   <br>
                  res.knn <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.knn))  <br>
                  res.knn
                 </p>
              </blockquote> 
            '),
            h2("Test Data with Predicted Output", style = "font-family: 'cursive';
                                                   text-align: center"),
            DTOutput("knn.df"),
            br(),br(),br()
          ),
          
          tabPanel("Logistic Regression",
            h2("Prediction using Logistic Regression", style = "font-family: 'cursive';
                                                   text-align: center"),
            br(),
            p("The first Logistic Regression model was built using all features from the new train data (tr.mdl) 
               that created from subsetted train data (c.train). Using seed number '123', below is the first model's 
               summary :"),
            HTML('
            <blockquote>
              <p>
              set.seed(123)<br>
              lr <- glm(fraud ~ ., data = tr.mdl, family = "binomial")<br>
              summary(lr)
              </p>
            </blockquote>
            '),
            verbatimTextOutput("lr.sum.1"),
            br(),
            p("From the above summary we can see that some of the features are not significantly affect the target variable 
              (fraud). Using the first model, we can see each features' importance :"),
            verbatimTextOutput("var.imp.1"),
            br(),
            p("Next, the insignificant features that we can see from the first model's summary were dropped and it creates a new model :"),
            HTML('
            <blockquote>
              <p>
              set.seed(123)<br>
              lr2 <- glm(fraud ~ age_of_driver + gender + marital_status + safty_rating + annual_income
                        + high_education_ind + address_change_ind + living_status +accident_site + past_num_of_claims
                        + witness_present_ind + age_of_vehicle, data = tr.mdl, family = "binomial")<br>
              summary(lr2)
              </p>
            </blockquote>
            '),
            verbatimTextOutput("lr.sum.2"),
            br(),
            p("And for the second model features' importance :"),
            verbatimTextOutput("var.imp.2"),
            br(),
            p("From both models' summaries, the AIC value of the second model is slightly lower than the first model. 
              In that case, we use the second model to evalute the model's score and to predict the test data. 
              Before evaluating model, the new test data (ts.mdl) must only contains all the features used to build the 
              second model (lr2). So, we need to subset the new test data (ts.mdl) into subsetted new test data (ts.mdl.2) 
              such as below code :"),
            HTML('
              <blockquote>
                <p>
                ts.mdl.2 <- subset(ts.mdl, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
                                      high_education_ind,address_change_ind,living_status,accident_site,
                                      past_num_of_claims,witness_present_ind,age_of_vehicle))
                </p>
              </blockquote>   
            '),
            p("Next predict the subsetted new test data using the second model."),
            HTML('
              <blockquote>
                <p>
                pred.prob <- lr2 %>% predict(ts.mdl.2, type = "response")<br>
                pred.class <- ifelse(pred.prob > 0.5, "1", "0")
                </p>
              </blockquote> 
            '),
            p("And compare the actual values of target with the predicted values so we can calculate model's metrics using Confusion Matrix."),
            HTML('
              <blockquote>
                <p>
                 confusionMatrix(as.factor(pred.class), ts.mdl$fraud)
                 </p>
              </blockquote> 
            '),
            verbatimTextOutput("cm.lr"),
            br(),
            p("The result shows that the accuracy of the model using Logistic Regression method is 84.14%*. Here are the prediction result using the new test data (ts.mdl) :"),
            p("*Can be slightly differ from output", style = "color: red"),
            h2("Model's Test Data with Actual and Predicted Output", style = "font-family: 'cursive'; text-align: center"),
            DTOutput("lr.pred"),
            br(),
            p("Next we can predict the target of actual test data, but we should subset the test data (c.test) so it will only includes the used features in the second model :"),
            HTML('
              <blockquote>
                <p>
                 c.test.2 <- subset(c.test, select = c(age_of_driver,gender,marital_status,safty_rating,annual_income,
                                      high_education_ind,address_change_ind,living_status,accident_site,
                                      past_num_of_claims,witness_present_ind,age_of_vehicle))
                 </p>
              </blockquote> 
            '),
            p("Here are the prediction result of subsetted test data using new model that includes all train data (c.train) :"),
            HTML('
              <blockquote>
                <p>
                  lr2.new <- glm(fraud ~ age_of_driver + gender + marital_status + safty_rating + annual_income
                 + high_education_ind + address_change_ind + living_status +accident_site + past_num_of_claims
                 + witness_present_ind + age_of_vehicle, data = c.train, family = "binomial") <br><br>
                  pred.prob.ts <- lr2.new %>% predict(c.test.2, type = "response") <br>
                  pred.class.ts <- ifelse(pred.prob.ts > 0.5, "1", "0") <br>
                  res.lr <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.class.ts)) <br>
                  res.lr
                 </p>
              </blockquote> 
            '),
            h2("Test Data with Predicted Output", style = "font-family: 'cursive';
                                                   text-align: center"),
            DTOutput("lr.df"),
            br(),br(),br()
          ),
          
          tabPanel("Naive Bayes",
            h2("Prediction using Naive Bayes", style = "font-family: 'cursive';
                                                  text-align: center"),
            br(),
            p("Here are the built Naive Bayes model using all features from the new train data (tr.mdl) using seed number '123' :"),
            HTML('
            <blockquote>
              <p>
              set.seed(123)<br>
              nb <- naive_bayes(fraud ~ ., data = tr.mdl)<br>
              plot(nb)
              </p>
            </blockquote>
            '),
            plotOutput("nb.plot"),
            br(),
            p("Then we predict the new test data (ts.mdl) using the Naive Bayes model and evaluate the model using Confusion Matrix : "),
            HTML('
            <blockquote>
              <p>
              pred.nb <- predict(nb, ts.mdl) <br>
              act <- ts.mdl$fraud <br>
              confusionMatrix(as.factor(pred.nb), act)
              </p>
            </blockquote>    
            '),
            verbatimTextOutput("cm.nb"),
            br(),
            p("The accuracy of Naive Bayes model using all features from the train data is 82.38%*. Here are the prediction result using the new test data (ts.mdl) :"),
            p("*Can be slightly differ from output", style = "color: red"),
            h2("Model's Test Data with Actual and Predicted Output", style = "font-family: 'cursive'; text-align: center"),
            DTOutput("nb.pred"),
            br(),
            p("Next we can predict the target of actual test data (c.test) using new model that includes all train data (c.train) :"),
            HTML('
              <blockquote>
                <p>
                  nb.all <- naive_bayes(fraud ~ ., data = c.train) <br>
                  pred.ts.nb <- predict(nb.all, c.test) <br>
                  res.nb <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.nb)) <br>
                  res.nb
                 </p>
              </blockquote> 
            '),
            h2("Test Data with Predicted Output", style = "font-family: 'cursive';
                                                   text-align: center"),
            DTOutput("nb.df"),
            br(),br(),br()
          ),
          
          tabPanel("Random Forest",
            h2("Prediction using Random Forest", style = "font-family: 'cursive';
                                                   text-align: center"),
            br(),
            p("Here are the built Random Forest model using all features from the new train data (tr.mdl) using seed number '123' :"),
            HTML('
            <blockquote>
              <p>
              set.seed(123)<br>
              rf <- randomForest(fraud ~ ., data = tr.mdl)<br>
              plot(rf)
              </p>
            </blockquote>
            '),
            plotOutput("rf.plot"),
            br(),
            p("Then we predict the new test data (ts.mdl) using the Random Forest model and evaluate the model using Confusion Matrix : "),
            HTML('
            <blockquote>
              <p>
              pred.rf <- predict(rf, ts.mdl) <br>
              act <- ts.mdl$fraud <br>
              confusionMatrix(as.factor(pred.rf), act)
              </p>
            </blockquote>    
            '),
            verbatimTextOutput("cm.rf"),
            br(),
            p("The accuracy of Random Forest model using all features from the train data is 84.46%*. Here are the prediction result using the new test data (ts.mdl) :"),
            p("*Can be slightly differ from output", style = "color: red"),
            h2("Model's Test Data with Actual and Predicted Output", style = "font-family: 'cursive'; text-align: center"),
            DTOutput("rf.pred"),
            br(),
            p("Next we can predict the target of actual test data (c.test) using new model that includes all train data (c.train) :"),
            HTML('
              <blockquote>
                <p>
                  rf.all <- randomForest(fraud ~ ., data = c.train) <br>
                  pred.ts.rf <- predict(rf.all, c.test) <br>
                  res.rf <- data.frame(c.test[,-length(c.test)], fraud = as.factor(pred.ts.rf)) <br>
                  res.rf
                 </p>
              </blockquote> 
            '),
            h2("Test Data with Predicted Output", style = "font-family: 'cursive';
                                                   text-align: center"),
            DTOutput("rf.df"),
            br(),br(),br()
          )
        )
      ),
      
      tabItem(
        "tab_predictor",
        h1("Predictor", style = "font-family: 'cursive';
                                       text-align: center"),
        h3("Predict target value based on users' inputs", 
           style = "font-family: 'cursive';
                    font-style: italic;
                    text-align: center"),
        br(),
        tabsetPanel(
          tabPanel("Decision Tree",
            h2("Decision Tree Predictor", style = "font-family: 'cursive';
                                            text-align: center"),
            br(),
            fluidRow(
              column(2,
                numericInput("dt.age_of_driver","Age of Driver", min = 17, max = 120, value = 45),
                selectInput("dt.gender","Gender", c("Female" = "F","Male" = "M")),
                selectInput("dt.marital_status","Marital Status", c("Not Married" = "0","Married" = "1")),
                numericInput("dt.safty_rating","Safty Rating", min = 1, max = 100, value = 75)
              ),
              
              column(2, offset = 0.5,
                numericInput("dt.annual_income","Annual Income", min = 1, max = 50000, value = 37500),
                selectInput("dt.high_education_ind","High Education", c("No" = "0","Yes" = "1")),
                selectInput("dt.address_change_ind","Address Change", c("No" = "0","Yes" = "1")),
                selectInput("dt.living_status","Living Status", c("Own" = "Own","Rent" = "Rent"))
              ),
              
              column(2,
                selectInput("dt.accident_site","Accident Site", c("Highway" = "Highway","Local" = "Local", "Parking Lot" = "Parking Lot")),
                numericInput("dt.past_num_of_claims","Past Num. of Claims", min = 0, max = 10, value = 1),
                selectInput("dt.witness_present_ind","Witness Present", c("No" = "0","Yes" = "1"))
              ),
              
              column(2, offset = 0.5,
                numericInput("dt.liab_prct","Liability Percentage", min = 0, max = 100, value = 50),
                selectInput("dt.channel","Channel", c("Broker" = "Broker","Online" = "Online", "Phone" = "Phone")),
                selectInput("dt.policy_report_filed_ind","Policy Report Filed", c("No" = "0","Yes" = "1"))
              ),
              
              column(2,
                numericInput("dt.claim_est_payout","Claim Estimation Payout", min = 250, max = 17500, value = 5000),
                numericInput("dt.age_of_vehicle","Age of Vehicle in Years", min = 0, max = 20, value = 5),
                selectInput("dt.vehicle_category","Vehicle Category", c("Compact" = "Compact","Large" = "Large", "Medium" = "Medium"))
              ),
              
              column(2, offset = 0.5,
                numericInput("dt.vehicle_price","Vehicle Price", min = 2300, max = 130000, value = 25000),
                selectInput("dt.vehicle_color","Vehicle Color", c("Black" = "black","Blue" = "blue", 
                                                                  "Gray" = "gray", "Red" = "red",
                                                                  "Silver" = "silver", "White" = "white", "Other" = "other")),
                numericInput("dt.vehicle_weight","Vehicle Weight", min = 2500, max = 110000, value = 22000),
                actionButton("dt.predict.btn", "Predict", class = "btn btn-secondary")     
              )
            ),
            tags$label(h3('Status & Output')),
            verbatimTextOutput('dt.status'),
            tableOutput('dt.output')
          ),
          
          tabPanel("K-Nearest Neighbor",
            h2("K-Nearest Neighbor Predictor", style = "font-family: 'cursive';
                                     text-align: center"),
            br(),
            fluidRow(
              column(2,
                numericInput("knn.age_of_driver","Age of Driver", min = 17, max = 120, value = 45),
                selectInput("knn.gender","Gender", c("Female" = "F","Male" = "M")),
                selectInput("knn.marital_status","Marital Status", c("Not Married" = "0","Married" = "1")),
                numericInput("knn.safty_rating","Safty Rating", min = 1, max = 100, value = 75)
              ),
              
              column(2, offset = 0.5,
                numericInput("knn.annual_income","Annual Income", min = 1, max = 50000, value = 37500),
                selectInput("knn.high_education_ind","High Education", c("No" = "0","Yes" = "1")),
                selectInput("knn.address_change_ind","Address Change", c("No" = "0","Yes" = "1")),
                selectInput("knn.living_status","Living Status", c("Own" = "Own","Rent" = "Rent"))
              ),
              
              column(2,
                selectInput("knn.accident_site","Accident Site", c("Highway" = "Highway","Local" = "Local", "Parking Lot" = "Parking Lot")),
                numericInput("knn.past_num_of_claims","Past Num. of Claims", min = 0, max = 10, value = 1),
                selectInput("knn.witness_present_ind","Witness Present", c("No" = "0","Yes" = "1"))
              ),
              
              column(2, offset = 0.5,
                numericInput("knn.liab_prct","Liability Percentage", min = 0, max = 100, value = 50),
                selectInput("knn.channel","Channel", c("Broker" = "Broker","Online" = "Online", "Phone" = "Phone")),
                selectInput("knn.policy_report_filed_ind","Policy Report Filed", c("No" = "0","Yes" = "1"))
              ),
              
              column(2,
                numericInput("knn.claim_est_payout","Claim Estimation Payout", min = 250, max = 17500, value = 5000),
                numericInput("knn.age_of_vehicle","Age of Vehicle in Years", min = 0, max = 20, value = 5),
                selectInput("knn.vehicle_category","Vehicle Category", c("Compact" = "Compact","Large" = "Large", "Medium" = "Medium"))
              ),
              
              column(2, offset = 0.5,
                numericInput("knn.vehicle_price","Vehicle Price", min = 2300, max = 130000, value = 25000),
                selectInput("knn.vehicle_color","Vehicle Color", c("Black" = "black","Blue" = "blue", 
                                                                  "Gray" = "gray", "Red" = "red",
                                                                  "Silver" = "silver", "White" = "white", "Other" = "other")),
                numericInput("knn.vehicle_weight","Vehicle Weight", min = 2500, max = 110000, value = 22000),
                actionButton("knn.predict.btn", "Predict", class = "btn btn-secondary")     
              )
            ),
            tags$label(h3('Status & Output')),
            verbatimTextOutput('knn.status'),
            tableOutput('knn.output')
          ),
          
          tabPanel("Logistic Regression",
            h2("Logistic Regression Predictor", style = "font-family: 'cursive';
                              text-align: center"),
            br(),
            selectInput("lrmodel","Model Selection",c("First Model" = 0, "Second Model" = 1), selected = 0),
            br(),
            conditionalPanel(condition = "input.lrmodel == 0",
              fluidRow(
                column(2,
                       numericInput("lr.age_of_driver","Age of Driver", min = 17, max = 120, value = 45),
                       selectInput("lr.gender","Gender", c("Female" = "F","Male" = "M")),
                       selectInput("lr.marital_status","Marital Status", c("Not Married" = "0","Married" = "1")),
                       numericInput("lr.safty_rating","Safty Rating", min = 1, max = 100, value = 75)
                ),
                
                column(2, offset = 0.5,
                       numericInput("lr.annual_income","Annual Income", min = 1, max = 50000, value = 37500),
                       selectInput("lr.high_education_ind","High Education", c("No" = "0","Yes" = "1")),
                       selectInput("lr.address_change_ind","Address Change", c("No" = "0","Yes" = "1")),
                       selectInput("lr.living_status","Living Status", c("Own" = "Own","Rent" = "Rent"))
                ),
                
                column(2,
                       selectInput("lr.accident_site","Accident Site", c("Highway" = "Highway","Local" = "Local", "Parking Lot" = "Parking Lot")),
                       numericInput("lr.past_num_of_claims","Past Num. of Claims", min = 0, max = 10, value = 1),
                       selectInput("lr.witness_present_ind","Witness Present", c("No" = "0","Yes" = "1"))
                ),
                
                column(2, offset = 0.5,
                       numericInput("lr.liab_prct","Liability Percentage", min = 0, max = 100, value = 50),
                       selectInput("lr.channel","Channel", c("Broker" = "Broker","Online" = "Online", "Phone" = "Phone")),
                       selectInput("lr.policy_report_filed_ind","Policy Report Filed", c("No" = "0","Yes" = "1"))
                ),
                
                column(2,
                       numericInput("lr.claim_est_payout","Claim Estimation Payout", min = 250, max = 17500, value = 5000),
                       numericInput("lr.age_of_vehicle","Age of Vehicle in Years", min = 0, max = 20, value = 5),
                       selectInput("lr.vehicle_category","Vehicle Category", c("Compact" = "Compact","Large" = "Large", "Medium" = "Medium"))
                ),
                
                column(2, offset = 0.5,
                       numericInput("lr.vehicle_price","Vehicle Price", min = 2300, max = 130000, value = 25000),
                       selectInput("lr.vehicle_color","Vehicle Color", c("Black" = "black","Blue" = "blue", 
                                                                         "Gray" = "gray", "Red" = "red",
                                                                         "Silver" = "silver", "White" = "white", "Other" = "other")),
                       numericInput("lr.vehicle_weight","Vehicle Weight", min = 2500, max = 110000, value = 22000),
                       actionButton("lr.predict.btn", "Predict", class = "btn btn-secondary")     
                )
              ),
              tags$label(h3('Status & Output')),
              verbatimTextOutput('lr.status'),
              tableOutput('lr.output')              
            ),
            
            conditionalPanel(condition = "input.lrmodel == 1",
              fluidRow(
                column(2,
                  numericInput("lr2.age_of_driver","Age of Driver", min = 17, max = 120, value = 45),
                  selectInput("lr2.gender","Gender", c("Female" = "F","Male" = "M"))
                ),
                
                column(2, offset = 0.5,
                  selectInput("lr2.marital_status","Marital Status", c("Not Married" = "0","Married" = "1")),
                  numericInput("lr2.safty_rating","Safty Rating", min = 1, max = 100, value = 75)  
                ),
                
                column(2, 
                  numericInput("lr2.annual_income","Annual Income", min = 1, max = 50000, value = 37500),
                  selectInput("lr2.high_education_ind","High Education", c("No" = "0","Yes" = "1"))
                       
                ),
                
                column(2, offset = 0.5,
                  selectInput("lr2.address_change_ind","Address Change", c("No" = "0","Yes" = "1")),
                  selectInput("lr2.living_status","Living Status", c("Own" = "Own","Rent" = "Rent"))        
                ),
                
                column(2,
                  selectInput("lr2.accident_site","Accident Site", c("Highway" = "Highway","Local" = "Local", "Parking Lot" = "Parking Lot")),
                  numericInput("lr2.past_num_of_claims","Past Num. of Claims", min = 0, max = 10, value = 1)
                ),
                
                column(2,offset = 0.5,
                  selectInput("lr2.witness_present_ind","Witness Present", c("No" = "0","Yes" = "1")),
                  numericInput("lr2.age_of_vehicle","Age of Vehicle in Years", min = 0, max = 20, value = 5),
                  actionButton("lr2.predict.btn", "Predict", class = "btn btn-secondary")      
                ),
                
              ),
              tags$label(h3('Status & Output')),
              verbatimTextOutput('lr2.status'),
              tableOutput('lr2.output') 
            )
            
          ),
          
          tabPanel("Naive Bayes",
            h2("Naive Predictor", style = "font-family: 'cursive';
                              text-align: center"),
            br(),
            fluidRow(
              column(2,
                numericInput("nb.age_of_driver","Age of Driver", min = 17, max = 120, value = 45),
                selectInput("nb.gender","Gender", c("Female" = "F","Male" = "M")),
                selectInput("nb.marital_status","Marital Status", c("Not Married" = "0","Married" = "1")),
                numericInput("nb.safty_rating","Safty Rating", min = 1, max = 100, value = 75)
              ),
              
              column(2, offset = 0.5,
                numericInput("nb.annual_income","Annual Income", min = 1, max = 50000, value = 37500),
                selectInput("nb.high_education_ind","High Education", c("No" = "0","Yes" = "1")),
                selectInput("nb.address_change_ind","Address Change", c("No" = "0","Yes" = "1")),
                selectInput("nb.living_status","Living Status", c("Own" = "Own","Rent" = "Rent"))
              ),
              
              column(2,
                selectInput("nb.accident_site","Accident Site", c("Highway" = "Highway","Local" = "Local", "Parking Lot" = "Parking Lot")),
                numericInput("nb.past_num_of_claims","Past Num. of Claims", min = 0, max = 10, value = 1),
                selectInput("nb.witness_present_ind","Witness Present", c("No" = "0","Yes" = "1"))
              ),
              
              column(2, offset = 0.5,
                numericInput("nb.liab_prct","Liability Percentage", min = 0, max = 100, value = 50),
                selectInput("nb.channel","Channel", c("Broker" = "Broker","Online" = "Online", "Phone" = "Phone")),
                selectInput("nb.policy_report_filed_ind","Policy Report Filed", c("No" = "0","Yes" = "1"))
              ),
              
              column(2,
                numericInput("nb.claim_est_payout","Claim Estimation Payout", min = 250, max = 17500, value = 5000),
                numericInput("nb.age_of_vehicle","Age of Vehicle in Years", min = 0, max = 20, value = 5),
                selectInput("nb.vehicle_category","Vehicle Category", c("Compact" = "Compact","Large" = "Large", "Medium" = "Medium"))
              ),
              
              column(2, offset = 0.5,
                numericInput("nb.vehicle_price","Vehicle Price", min = 2300, max = 130000, value = 25000),
                selectInput("nb.vehicle_color","Vehicle Color", c("Black" = "black","Blue" = "blue", 
                                                                  "Gray" = "gray", "Red" = "red",
                                                                  "Silver" = "silver", "White" = "white", "Other" = "other")),
                numericInput("nb.vehicle_weight","Vehicle Weight", min = 2500, max = 110000, value = 22000),
                actionButton("nb.predict.btn", "Predict", class = "btn btn-secondary")     
              )
            ),
            tags$label(h3('Status & Output')),
            verbatimTextOutput('nb.status'),
            tableOutput('nb.output')
          ),
          
          tabPanel("Random Forest",
            h2("Random Forest Predictor", style = "font-family: 'cursive';
                              text-align: center"),
            br(),
            fluidRow(
              column(2,
                numericInput("rf.age_of_driver","Age of Driver", min = 17, max = 120, value = 45),
                selectInput("rf.gender","Gender", c("Female" = "F","Male" = "M")),
                selectInput("rf.marital_status","Marital Status", c("Not Married" = "0","Married" = "1")),
                numericInput("rf.safty_rating","Safty Rating", min = 1, max = 100, value = 75)
              ),
              
              column(2, offset = 0.5,
                numericInput("rf.annual_income","Annual Income", min = 1, max = 50000, value = 37500),
                selectInput("rf.high_education_ind","High Education", c("No" = "0","Yes" = "1")),
                selectInput("rf.address_change_ind","Address Change", c("No" = "0","Yes" = "1")),
                selectInput("rf.living_status","Living Status", c("Own" = "Own","Rent" = "Rent"))
              ),
              
              column(2,
                selectInput("rf.accident_site","Accident Site", c("Highway" = "Highway","Local" = "Local", "Parking Lot" = "Parking Lot")),
                numericInput("rf.past_num_of_claims","Past Num. of Claims", min = 0, max = 10, value = 1),
                selectInput("rf.witness_present_ind","Witness Present", c("No" = "0","Yes" = "1"))
              ),
              
              column(2, offset = 0.5,
                numericInput("rf.liab_prct","Liability Percentage", min = 0, max = 100, value = 50),
                selectInput("rf.channel","Channel", c("Broker" = "Broker","Online" = "Online", "Phone" = "Phone")),
                selectInput("rf.policy_report_filed_ind","Policy Report Filed", c("No" = "0","Yes" = "1"))
              ),
              
              column(2,
                numericInput("rf.claim_est_payout","Claim Estimation Payout", min = 250, max = 17500, value = 5000),
                numericInput("rf.age_of_vehicle","Age of Vehicle in Years", min = 0, max = 20, value = 5),
                selectInput("rf.vehicle_category","Vehicle Category", c("Compact" = "Compact","Large" = "Large", "Medium" = "Medium"))
              ),
              
              column(2, offset = 0.5,
                numericInput("rf.vehicle_price","Vehicle Price", min = 2300, max = 130000, value = 25000),
                selectInput("rf.vehicle_color","Vehicle Color", c("Black" = "black","Blue" = "blue", 
                                                                  "Gray" = "gray", "Red" = "red",
                                                                  "Silver" = "silver", "White" = "white", "Other" = "other")),
                numericInput("rf.vehicle_weight","Vehicle Weight", min = 2500, max = 110000, value = 22000),
                actionButton("rf.predict.btn", "Predict", class = "btn btn-secondary")     
              )
            ),
            tags$label(h3('Status & Output')),
            verbatimTextOutput('rf.status'),
            tableOutput('rf.output')
          )
        )
      ),
      
      tabItem(
        "tab_about",
        h1("About", style = "font-family: 'cursive';
                                       text-align: center"),
        h3("About this Shiny web apps", 
           style = "font-family: 'cursive';
                    font-style: italic;
                    text-align: center"),
        br(),
        HTML(" 
          <div style = 'text-align: justify'>
            <h4><strong>Creator</strong></h4>
            <p><em>Aurellia Vania Yosephine Budiman</em></p>
            <p>(Undergraduate Student at Bina Nusantara University, majors in Computer Science and Statistics)</p>
            
            <br>
            <h4><strong>About Project</strong></h4>
            <p>This web application project was created in December 2021 and developed using R Shiny and RStudio.</p>
            <br>
            <h4><strong>References :</strong></h4>
              <ul>
                <li><a href='https://www.kaggle.com/surekharamireddy/fraudulent-claim-on-cars-physical-damage'>Fraudulent Claim Dataset from Kaggle</a></li>
                <li><a href='https://shiny.rstudio.com/gallery/'>R Shiny Gallery</a></li>
                <li><a href='https://towardsdatascience.com/classification-on-a-large-and-noisy-dataset-with-r-c10cf14cbae6'>Classification on a Large and Noisy Dataset with R</a></li>
                <li><a href='https://www.v7labs.com/blog/data-preprocessing-guide'>A Simple Guide to Data Preprocessing in Machine Learning</a></li>
              </ul>
          </div>
        ")
      )
    )
  ),
  title = "Fraudulent Claim Predictor",
  skin  = "blue"
)



