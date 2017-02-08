### functions for summarizing and cleaning data

## function for summarizing data
summarize_data <- function(df)
{
  # loading libraries
  library(data.table)
  
  # converting to dataframe
  df <- as.data.frame(df)
  
  suppressWarnings(
  ldf <- lapply(1:ncol(df), function(k)
  {
    return(data.table(Column = colnames(df)[k],
                      Type = class(df[,k]),
                      Unique_Values = length(unique(df[,k])),
                      Missing_Values = sum(is.na(df[,k])),
                      Max = round(as.numeric(max(df[,k], na.rm=T)),4),
                      Mean = round(as.numeric(mean(df[,k], na.rm=T)),4),
                      Min = round(as.numeric(min(df[,k], na.rm=T)),4)
                      ))
  })
  )
  
  ldf <- rbindlist(ldf)
  
  return(ldf)
}


## function for removing constant columns
remove_redundant_columns <- function(train, test)
{
	count_unique <- lapply(train, function(k){length(unique(k))})
	constant_columns <- names(count_unique[count_unique == 1])

	if (length(constant_columns) > 0)
	{
		train <- train[, .SD, .SDcols=-c(constant_columns)]
    test <- test[, .SD, .SDcols=-c(constant_columns)]
    
    for (i in 1:length(constant_columns))
    {
      cat("Column:", constant_columns[i], "is a redundant column\n")      
    }
	}else
	{
    cat("No redundant columns found\n")
	}
	
	cat("\n")
  
  return(list("train"=train,"test"=test))
}


## function for removing duplicate columns
remove_duplicate_columns <- function(train, test)
{
  dups <- sum(duplicated(lapply(train,c)))
  
  if (dups > 0)
  {
    cat(dups, "duplicate columns found in data\n")
  }else
  {
    cat("No duplicate columns found\n")
  }
  
  cat("\n")
  
  return(list("train"=train,"test"=test))
}


## function for converting two-element columns to binary
convert_binary_columns <- function(train, test)
{
	count_unique <- lapply(train, function(k){length(unique(k))})
	binary_columns <- names(count_unique[count_unique == 2])

	change <- 0

	for (i in which(colnames(train) %in% binary_columns))
	{
		if (all(unique(train[[i]]) %in% c(0,1)) != T)
		{
      # converting column to binary
		  test[[i]] <- as.numeric(factor(test[[i]], levels=unique(train[[i]])))
		  train[[i]] <- as.numeric(as.factor(train[[i]])) - 1

			change <- 1

			cat("Column", colnames(train)[i], "converted to binary column\n")
		}
	}

	if (change == 0)
	{
		cat("No binary columns found\n")
	}

	cat("\n")
  
  return(list("train"=train,"test"=test))
}


## function for cleaning data by combining cleaning functions
clean_data <- function(train,test)
{
  # loading libraries
  library(data.table)
  
  df <- list("train"=train,"test"=test)
  
	df <- remove_redundant_columns(df$train,df$test)
  df <- remove_duplicate_columns(df$train,df$test)
  df <- convert_binary_columns(df$train,df$test)
  
  return(list("train"=df$train,"test"=df$test))
}

# code for predicting cutoff

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0,P=0,N=0)
cutoffs=seq(0,1,length=100)

for (cutoff in cutoffs){
  predicted=as.numeric(test$prob>cutoff)
  
  TP=sum(predicted==1 & test$Target==1)
  FP=sum(predicted==1 & test$Target==0)
  FN=sum(predicted==0 & test$Target==1)
  TN=sum(predicted==0 & test$Target==0)
  P=FN+TP
  N=TN+FP
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN,P,N))
}

# removing the dummy data cotaining top row
cutoff_data=cutoff_data[-1,]

cutoff_data=cutoff_data %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  select(-P,-N)

KS_cutoff=cutoff_data$cutoff[which(cutoff_data$KS==max(cutoff_data$KS))][1]

table(y = test$Target,cutoff = as.numeric(test$prob>KS_cutoff))



