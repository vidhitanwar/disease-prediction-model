library(caret)
library(tidyverse)
library(dplyr)
library(doParallel)
registerDoParallel(cores=5)

# Utility functions
clean.cell <- function(x) {
  x <- trimws(x)
  return(gsub(" ", "", x))
}

#Importing the raw data and doing basic imputation =============================
raw.data <- read.csv("data/dataset.csv", na.strings = c("NA", ""))
glimpse(raw.data)

#Replacing NA values with -1 or -9999 and trimming the blank space
raw.data <- data.frame(lapply(raw.data, clean.cell), stringsAsFactors = FALSE)
raw.data[is.na(raw.data)] = -1
# raw.data <- mutate_if(raw.data, is.character, as.factor)
glimpse(raw.data)

## Getting all the Symptoms
Symptoms <- cbind(raw.data$Symptom_1, raw.data$Symptom_2, raw.data$Symptom_3 , raw.data$Symptom_4,
                  raw.data$Symptom_5,raw.data$Symptom_6, raw.data$Symptom_7, raw.data$Symptom_8,
                  raw.data$Symptom_9, raw.data$Symptom_10,raw.data$Symptom_11, raw.data$Symptom_12, 
                  raw.data$Symptom_13, raw.data$Symptom_14, raw.data$Symptom_15,
                  raw.data$Symptom_16, raw.data$Symptom_17)

# Converting all the vectors into one vector
Symptoms <- as.vector(Symptoms)
Symptoms <- Symptoms[Symptoms!=-1] # Removing NA's or NA represenation (-1)
Symptoms <- unique(Symptoms)
# View(data.frame(Symptoms))
length(Symptoms) # 131 Symptoms

## Wrangling the Raw data from semi-long form to wide form ===========================================
clean.data <- raw.data %>%
  rowwise() %>% #To make the operation row wise
  mutate(Symptoms=paste(Symptom_1,Symptom_2,Symptom_3,Symptom_4,
                        Symptom_5,Symptom_6,Symptom_7,Symptom_8,
                        Symptom_9,Symptom_10,Symptom_11,Symptom_12,
                        Symptom_13,Symptom_14,Symptom_15,Symptom_16,
                        Symptom_17, sep = ", ")) %>% #To attach all symptoms as one
  ungroup() %>% # To remove row wise operation
  select(Disease, Symptoms) #Selecting only disease and all its symptoms

# Pulling unique symptoms for each Disease
clean.data$Symptoms <- sapply(strsplit(clean.data$Symptoms, ", "), 
                              function(x) paste(unique(x), collapse=", ")) 

Symptoms <- sort(Symptoms)
for(i in 1:length(Symptoms)) {
  clean.data[, Symptoms[i]] <- NA
}

glimpse(clean.data)

for(i in 1:nrow(clean.data)) {
  temp.symptoms <- strsplit(as.character(clean.data[i, "Symptoms"]), ", ") %>%
    as.data.frame()
  temp.symptoms <- as.vector(temp.symptoms[,1])
  
  for(j in 1:length(temp.symptoms)) {
    clean.data[i, temp.symptoms[j]] <- TRUE
  }
}

clean.data[is.na(clean.data)] <- FALSE
clean.data$Symptoms <- NULL
clean.data$`-1` <- NULL

clean.data$Disease <- as.factor(clean.data$Disease)

View(clean.data)
glimpse(clean.data)

nearZeroVarCols <- nearZeroVar(clean.data)
clean.data <- clean.data[, -nearZeroVarCols]


## Modelling ===============================================
# LDA - Dimensionality reduction
lda.fit <- train(Disease ~ ., data = clean.data, method = "lda")

lda.fit


#KNN
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5, repeats = 3)

knn.fit <- train(Disease ~ ., data = clean.data,
                 method = "knn", trControl=fitControl)

knn.fit
plot(knn.fit)
