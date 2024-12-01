# To empty environment
rm(list=ls())

library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(doParallel)
registerDoParallel(cores=4)

# Data Quality reports =========================================================
# Function to clean contents of each cell in dataframe
clean.cell <- function(x) {
  x <- trimws(x)
  return(gsub(" ", "", x))
}

# Function to generate numeric variables data quality report
gen.data.report.num <- function(df) {
  # Function to generate summary of a numeric column
  gen.num.summary <- function(x){
    Q1<-function(x,na.rm=TRUE) { quantile(x,na.rm=na.rm)[2] }
    Q3<-function(x,na.rm=TRUE) { quantile(x,na.rm=na.rm)[4] }
    
    c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
      min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
      max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
  }
  
  # Extracting the Numeric columns and generate the Numeric summary
  num.df <- df %>% 
    dplyr::select(where(is.numeric)) %>% 
    dplyr::as_tibble()
  
  if(length(num.df) > 0) {
    cols <- colnames(num.df)
    
    # Numeric related histogram plots
    plot(num.df %>%
           pivot_longer(cols = cols) %>%
           as.data.frame() %>%
           ggplot(aes(x=value)) +
           geom_histogram(bins = sqrt(length(df))) +
           facet_wrap(~ name, scales = "free"))
    
    return(num.df %>%
             dplyr::summarise(dplyr::across(.fns = gen.num.summary)) %>%
             cbind(
               stat=c("n","unique","missing","mean","min","Q1",
                      "median","Q3","max","sd")) %>%
             pivot_longer(cols[1]:cols[length(cols)], names_to = "variable", 
                          values_to = "value") %>%
             pivot_wider(names_from = stat, values_from = value) %>%
             mutate(missing_pct = 100*missing/n,
                    unique_pct = 100*unique/n) %>%
             select(variable, n, missing, missing_pct, unique, 
                    unique_pct, everything()))
  }
  
  # If the dataset doesn't have any numeric variables then message is shown
  return(data.frame(Message="No Numeric values in data"))
}

#Function for non numerical data quality report
gen.data.report.nnum <- function(df) {
  # Function to generate summary of a non-numeric column
  gen.n.num.summary <- function(x)
  {
    c(length(x), n_distinct(x), sum(is.na(x)))
  }
  
  # Extracting the Non-Numeric Columns
  nnum.df <- df %>% 
    dplyr::transmute(across(!where(is.numeric), as.factor)) %>% 
    as_tibble() 
  
  if(length(nnum.df) > 0) {
    cols <- colnames(nnum.df)
    
    return(nnum.df %>%
             dplyr::summarise(dplyr::across(.fns = gen.n.num.summary)) %>%
             cbind(stat=c("n","unique","missing")) %>%
             pivot_longer(cols[1]:cols[length(cols)], names_to = "variable", 
                          values_to = "value") %>%
             pivot_wider(names_from = stat, values_from = value) %>%
             dplyr::mutate(dplyr::across(c(missing, unique, n),as.numeric)) %>%
             dplyr::mutate(missing_pct = 100*missing/n, 
                           unique_pct = 100*unique/n) %>%
             select(variable, n, missing, missing_pct, unique, 
                    unique_pct, everything()))
  }
  
  return(data.frame(Message="No Non Numeric values in data"))
}

#Importing the raw data and doing basic imputation -----------------------------
raw.data <- read.csv("data/dataset.csv", na.strings = c("NA", ""))
# glimpse(raw.data)

# Replacing NA values with -1 and trimming whitespaces in each cell of the dataframe
raw.data <- lapply(raw.data[,-1], clean.cell) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  cbind(Disease=raw.data[,1])
raw.data[is.na(raw.data)] = -1
# glimpse(raw.data)

## Getting all the Symptoms
Symptoms <- cbind(raw.data$Symptom_1, raw.data$Symptom_2, raw.data$Symptom_3, 
                  raw.data$Symptom_4, raw.data$Symptom_5, raw.data$Symptom_6, 
                  raw.data$Symptom_7, raw.data$Symptom_8, raw.data$Symptom_9,
                  raw.data$Symptom_10, raw.data$Symptom_11, raw.data$Symptom_12, 
                  raw.data$Symptom_13, raw.data$Symptom_14, raw.data$Symptom_15,
                  raw.data$Symptom_16, raw.data$Symptom_17)

# Converting all the vectors into one vector
Symptoms <- as.vector(Symptoms)
Symptoms <- Symptoms[Symptoms!=-1] # Removing NA's or NA represenation (-1)
Symptoms <- unique(Symptoms)

## Wrangling the Raw data from semi-long form to wide form =====================
clean.data <- raw.data %>%
  rowwise() %>% #To make the operation row wise
  mutate(Symptoms=paste(Symptom_1,Symptom_2,Symptom_3,Symptom_4,
                        Symptom_5,Symptom_6,Symptom_7,Symptom_8,
                        Symptom_9,Symptom_10,Symptom_11,Symptom_12,
                        Symptom_13,Symptom_14,Symptom_15,Symptom_16,
                        Symptom_17, sep = ", ")) %>% #Attach all symptoms as one
  ungroup() %>% # To remove row wise operation
  select(Disease, Symptoms) #Selecting only disease and all its symptoms

# Pulling unique symptoms for each Disease
clean.data$Symptoms <- sapply(strsplit(clean.data$Symptoms, ", "), 
                              function(x) paste(unique(x), collapse=", ")) 

Symptoms <- sort(Symptoms)
for(i in 1:length(Symptoms)) {
  clean.data[, Symptoms[i]] <- NA
}

# Looping through each row and assigning true to the Symptom column if the
# Symptom is present in `Symptoms` column string (comma-seperated data)
for(i in 1:nrow(clean.data)) {
  temp.symptoms <- strsplit(as.character(clean.data[i, "Symptoms"]), ", ") %>%
    as.data.frame()
  temp.symptoms <- as.vector(temp.symptoms[,1])
  
  for(j in 1:length(temp.symptoms)) {
    clean.data[i, temp.symptoms[j]] <- TRUE
  }
}

# Later, assigning False to all the cells in dataframe which are not assigned
# True and removing `Symptoms` and `-1` column (-1 is used to represent no 
# recorded symptom)
clean.data[is.na(clean.data)] <- FALSE
clean.data$Symptoms <- NULL
clean.data$`-1` <- NULL

clean.data$Disease <- as.factor(clean.data$Disease)
View(clean.data)

gen.data.report.num(clean.data) %>% 
  write.csv("Cleaned Data Numeric Data Report.csv",row.names = F)

# Clean Data Report Generation
gen.data.report.nnum(clean.data) %>%
  write.csv("Cleaned Data Non Numeric Data Report.csv",row.names = F)


# Visualizations ---------------------------------------------------------------

# Visualization 1
## Visualize the top 10 Symptoms recorded in the initial RAW dataset ===========
raw.data <- read.csv("data/dataset.csv", na.strings = c("NA", ""))

# Appending all the Symptoms
Symptoms <- cbind(raw.data$Symptom_1, raw.data$Symptom_2, raw.data$Symptom_3 , raw.data$Symptom_4,
                  raw.data$Symptom_5,raw.data$Symptom_6, raw.data$Symptom_7, raw.data$Symptom_8,
                  raw.data$Symptom_9, raw.data$Symptom_10,raw.data$Symptom_11, raw.data$Symptom_12, 
                  raw.data$Symptom_13, raw.data$Symptom_14, raw.data$Symptom_15,
                  raw.data$Symptom_16, raw.data$Symptom_17)

# Converting all the vectors into one vector
Symptoms <- as.vector(Symptoms)
Symptoms <- Symptoms[!is.na(Symptoms)] # Removing NA's
Symptoms <- as.data.frame(Symptoms)    #Converting it to Dataframe

agg_symptoms <- Symptoms %>% group_by(Symptoms) %>%   # Grouping by the symptoms and adding total count
  summarise(total_count=n()) %>%
  arrange(desc(total_count)) %>%
  mutate(Symptoms=as.factor(trimws(Symptoms)))

#Selecting top 10 symptoms and arranging them in descending order. Using ggplot to display.
agg_symptoms %>%
  top_n(10) %>%
  mutate(Symptoms = fct_reorder(Symptoms, total_count, .desc = TRUE)) %>%
  ggplot(Top10_symp, mapping = aes(x = Symptoms, y = total_count)) +
  geom_col(fill = "#0099f9") +
  geom_text(aes(label = total_count ), vjust = 2, size = 5, color = "#ffffff") +
  labs(title="Top 10 Symptoms", x="Symptoms", y="Count")


# Visualization 2
## Finding Highly co-related variables in the Cleaned Data =====================
custom.corr <- function(data=df,sig=0.5){
  # Convert characters to factor then to numeric (Intermediate factor conversion
  # is to handle NA value in the data)
  df_cor <- data %>% mutate(across(.cols = where(is.character), 
                                   .fns = as.factor))
  df_cor <- df_cor %>% mutate(across(.cols = where(is.factor), 
                                     .fns = as.numeric))
  
  corr <- cor(df_cor)
  
  # Drop perfect correlations
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  corr[corr == 1] <- NA 
  
  # Converting to dataframe to generate cor-table
  corr <- as.data.frame(as.table(corr))
  corr <- na.omit(corr)
  
  # Select significant values and sort by highest correlation
  corr <- subset(corr, abs(Freq) > sig) 
  corr <- corr[order(-abs(corr$Freq)),] 
  
  # Convert corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  # Plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
  
  return(corr)
}

custom.corr(data=clean.data[, -1], sig=0.7) %>% 
  View(title = "Correlation between Variables (>0.7)")

# Top 5 symptoms seen in observations ================================
x <- data.frame(count=colSums(clean.data[,-1]), 
                symptom=names(colSums(clean.data[,-1]))) %>%
  arrange(desc(count))

x[1:5, ] %>%
  mutate(symptom = fct_reorder(symptom, count, .desc = TRUE)) %>%
  ggplot(aes(y=count, x=symptom)) +
  geom_col(fill = "#0099f9") +
  geom_text(aes(label = count), vjust = 2, size = 5, color = "#ffffff") +
  labs(title="Top 5 Symptoms", x="Symptoms", y="Count")


## Modelling ===============================================
# Removing duplicated rows in clean.data
nrow(clean.data) - sum(duplicated(clean.data)) # 304
clean.data <- unique(clean.data)

write.csv(clean.data, file = "clean.data.csv", row.names = F)

# Removing the Near Zero Variance
nearZeroVarCols <- nearZeroVar(clean.data)
reduced.data <- clean.data[, -nearZeroVarCols]
View(reduced.data)

#Model 1 - RF
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5, repeats = 3) # 5-fold cross validation (with 3 repetitions)

rf.fit <- train(Disease ~ ., data = clean.data,
                method = "rf", trControl=fitControl)

rf.fit

plot(rf.fit)

# Model 1.1 - RF with Reduced data
rf.fit1.1 <- train(Disease ~ ., data = reduced.data,
                   method = "rf", trControl=fitControl)

rf.fit1.1

plot(rf.fit1.1)

#Model 1 - RF
#with tuning parameters
rf.tune <- expand.grid(mtry=seq(10, 22, 1))

rf.fit1 <- train(Disease ~ ., data = clean.data,
                 method = "rf", trControl=fitControl, tuneGrid=rf.tune)

rf.fit1

plot(rf.fit1)

p1 = predict(rf.fit1,clean.data)

confusionMatrix(p1,clean.data$Disease)

#Using randomForesst
RFnew = randomForest(formula = Disease ~ ., data= clean.data, proximity = TRUE)

print(RFnew)


# PDF Generation ===================================================
# # install.packages("gridExtra")   # Install & load gridExtra
# library("gridExtra")
# 
# 
# predict.top3.diseases <- function(train.data, model, inp.symptoms) {
#   # train.data <- reduced.data
#   # model <- rf.fit1.1
#   # inp.symptoms <- c("dark_urine", "itching")
#   
#   # Code to get predictions from actual data
#   predictors <- names(train.data)[-1]
#   
#   if(!all(inp.symptoms %in% predictors)) {
#     stop("Not all given symptoms used during training")
#   }
#   
#   diseases.info.df <- read.csv("data/symptom_Description.csv") %>% as_tibble()
#   
#   View(diseases.info.df)
#   
#   test.data <- data.frame(matrix(nrow = 1, ncol = length(predictors)))  %>%
#     as_tibble()
#   colnames(test.data) <- predictors
#   test.data[is.na(test.data)] <- FALSE
#   
#   # Populating given input symptoms in test data
#   for(i in 1:length(inp.symptoms)) {
#     test.data[1, inp.symptoms[1]] <- TRUE
#   }
#   
#   preds <- predict(model, test.data, type = "prob")
#   preds_t <- t(preds) %>% as_tibble()
#   preds_t <- cbind(colnames(preds), preds_t)
#   colnames(preds_t) <- c("Disease", "Probability")
#   
#   # Attach Disease description to the result
#   preds_t <- dplyr::arrange(preds_t, desc(Probability)) %>% top_n(3)
#   
#   return(left_join(preds_t, diseases.info.df, by = "Disease"))
# }
# 
# pdf("data_gridExtra.pdf", width = 16, height = 9)       # Export PDF
# res.df <- predict.top3.diseases(reduced.data, rf.fit1.1, c("dark_urine", "itching"))
# res.df$Description <- sapply(lapply(res.df$Description, strwrap, width=50), paste, collapse="\n") 
# grid.table(res.df)
# dev.off()