# To empty environment
rm(list=ls())

library(tidyverse)
#To plot correlation matrix
library(corrplot)

# Data Quality reports ----------------------------------------------------------------------------------
# Function to generate numeric variables data quality report
gen.data.report.num <- function(df) {
  # df <- read.csv("data/dataset.csv", stringsAsFactors = T, na.strings = c("NA", ""))
  
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
               stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd")) %>%
             pivot_longer(cols[1]:cols[length(cols)], names_to = "variable", values_to = "value") %>%
             pivot_wider(names_from = stat, values_from = value) %>%
             mutate(missing_pct = 100*missing/n,
                    unique_pct = 100*unique/n) %>%
             select(variable, n, missing, missing_pct, unique, unique_pct, everything()))
  }
  
  # If the dataset doesn't have any numeric variables then below message is printed
  return(data.frame(Message="No Numeric values in data"))
}

#Function for non numerical data quality report
gen.data.report.nnum <- function(df) {
  # df <- read.csv("data/dataset.csv", stringsAsFactors = T, na.strings = c("NA", ""))
  
  # Function to generate summary of a non-numeric column
  gen.n.num.summary <- function(x)
  {
    # modes generation
    getmodes <- function(v,type=1) 
    {
      tbl <- table(v)
      m1<-which.max(tbl)
      if(length(tbl) < type) { return(NA) }
      
      if (type==1) {
        return (names(m1)) #1st mode
      }
      else if (type==2) {
        return (names(which.max(tbl[-m1]))) #2nd mode
      }
      else if (type==-1) {
        return (names(which.min(tbl))) #least common mode
      }
      else {
        stop("Invalid type selected")
      }
    }
    
    # modes frequency count 
    getmodesCnt <- function(v,type=1) {
      tbl <- table(v)
      m1<-which.max(tbl)
      if(length(tbl) < type) { return(NA) }
      
      if (type==1) {
        return (max(tbl)) #1st mode freq
      }
      else if (type==2) {
        return (max(tbl[-m1])) #2nd mode freq
      }
      else if (type==-1) {
        return (min(tbl)) #least common freq
      }
      else {
        stop("Invalid type selected")
      }
    }
    
    # Frequency ratio of type 1 to type 2
    freRat <- function(x){ getmodesCnt(x, type = 1)/getmodesCnt(x,type = 2) }
    
    c(length(x), n_distinct(x), sum(is.na(x)),round(freRat(x),digits = 2), getmodes(x,type = 1),
      getmodesCnt(x,type= 1), getmodes(x, type = 2), getmodesCnt(x,type= 2),
      getmodes(x,type= -1),getmodesCnt(x,type= -1))
  }
  
  # Extracting the Non-Numeric Columns
  nnum.df <- df %>% 
    dplyr::transmute(across(!where(is.numeric), as.factor)) %>% 
    as_tibble() 
  
  if(length(nnum.df) > 0) {
    cols <- colnames(nnum.df)
    
    return(nnum.df %>%
             dplyr::summarise(dplyr::across(.fns = gen.n.num.summary)) %>%
             cbind(
               stat=c("n","unique","missing","freqRatio","1st mode","1st mode freq","2st mode",
                      "2st mode freq", "least common","least common freq")) %>%
             pivot_longer(cols[1]:cols[length(cols)], names_to = "variable", values_to = "value") %>%
             pivot_wider(names_from = stat, values_from = value) %>%
             dplyr::mutate(dplyr::across(c(missing, unique, n, freqRatio, `1st mode freq`,
                                           `2st mode freq`, `least common freq`),as.numeric)) %>%
             dplyr::mutate(missing_pct = 100*missing/n, unique_pct = 100*unique/n) %>%
             select(variable, n, missing, missing_pct, unique, unique_pct, everything()))
  }
  
  return(data.frame(Message="No Non Numeric values in data"))
}

## Reading the Raw Dataset ===========================================================================
raw.data <- read.csv("data/dataset.csv", stringsAsFactors = T, na.strings = c("NA", ""))

# Raw Data Report
#Numeric data report generation
gen.data.report.num(raw.data) %>% View(title = "Raw Data Numeric Data Report")
#Non Numeric data report generation
gen.data.report.nnum(raw.data) %>% 
  write.csv("Raw Data Non-Numeric Data Report.csv", row.names = F)

## Wrangling the Raw data from semi-long form to wide form ===========================================
clean.data <- raw.data %>%
  mutate(across(.fns = trimws, .cols = everything()), Disease=as.factor(Disease)) %>% # changing disease into factor
  group_by(Disease) %>% #grouped by disease
  summarise(Symptom_1=toString(unique(Symptom_1)), #To retain only unique symptoms after grouping
            Symptom_2=toString(unique(Symptom_2)),
            Symptom_3=toString(unique(Symptom_3)),
            Symptom_4=toString(unique(Symptom_4)),
            Symptom_5=toString(unique(Symptom_5)),
            Symptom_6=toString(unique(Symptom_6)),
            Symptom_7=toString(unique(Symptom_7)),
            Symptom_8=toString(unique(Symptom_8)),
            Symptom_9=toString(unique(Symptom_9)),
            Symptom_10=toString(unique(Symptom_10)),
            Symptom_11=toString(unique(Symptom_11)),
            Symptom_12=toString(unique(Symptom_12)),
            Symptom_13=toString(unique(Symptom_13)),
            Symptom_14=toString(unique(Symptom_14)),
            Symptom_15=toString(unique(Symptom_15)),
            Symptom_16=toString(unique(Symptom_16)),
            Symptom_17=toString(unique(Symptom_17))) %>%
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

# Converting comma-separated symptoms to individual rows (to long form)
clean.data <- clean.data %>% 
  mutate(Symptom = strsplit(as.character(Symptoms), ", ")) %>% 
  unnest(Symptom) %>%
  select(Disease, Symptom) %>%
  filter(Symptom!="NA")

clean.data <- cbind(clean.data, val=1) %>%
  pivot_wider(names_from = Symptom, values_from = val, values_fill = 0)

# Clean Data Report Generation
gen.data.report.num(clean.data) %>% 
  write.csv("Cleaned Data Numeric Data Report.csv",row.names = F)

# gen.data.report.nnum(clean.data) %>% 
#   write.csv("Cleaned Data Non Numeric Data Report.csv",row.names = F)


# Visualizations -----------------------------------------------------------------------------

# Visualization 1
## Visualize the top 10 Symptoms recorded in the initial RAW dataset =========================
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
## Finding Highly co-related variables in the Cleaned Data =====================================
custom.corr <- function(data=df,sig=0.5){
  # Convert characters to factor then to numeric (Intermediate factor conversion is to handle
  # NA value in the data)
  df_cor <- data %>% mutate(across(.cols = where(is.character), .fns = as.factor))
  df_cor <- df_cor %>% mutate(across(.cols = where(is.factor), .fns = as.numeric))
  
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

custom.corr(data=clean.data[, -1], sig=0.7) %>% View(title = "Correlation between Variables (>0.7)")

# Top 5 symptoms seen in observations ================================
x <- data.frame(count=colSums(clean.data[,-1]), symptom=names(colSums(clean.data[,-1]))) %>%
  arrange(desc(count))

x[1:5, ] %>%
  mutate(symptom = fct_reorder(symptom, count, .desc = TRUE)) %>%
  ggplot(aes(y=count, x=symptom)) +
  geom_col(fill = "#0099f9") +
  geom_text(aes(label = count), vjust = 2, size = 5, color = "#ffffff") +
  labs(title="Top 5 Symptoms", x="Symptoms", y="Count")
