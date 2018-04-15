## This function loads a data table into an object
loadData <- function(wd, subdir, fname, columnsNames, dropColumns)
{
  library(data.table)
  
  if (subdir == "")
      filespec <- paste0("./",wd,"/",fname)
  else
      filespec <- paste0("./",wd,"/",subdir,"/",fname)
  
  
  if (is.null(columnsNames))
      if (is.null(dropColumns))
          return (fread(filespec, header = FALSE))
      else 
          return (fread(filespec, drop = dropColumns, header = FALSE))
  else
      return(fread(filespec,col.names = columnsNames, header = FALSE))
}


## compileData - merges all of the data from the test and training file 
compileData <- function(wd, dataType)
{
  xFile <- "X_"
  yFile <- "Y_"
  subjectsFile <- "subject_"
  if ((dataType == "test") | (dataType == "train"))
  {
    xFile <- paste0(xFile, dataType, ".txt")
    yFile <- paste0(yFile, dataType, ".txt")
    subjectsFile <- paste0(subjectsFile, dataType, ".txt")
  }
  else 
    stop ("dataType is illegal")
  x_data <- loadData(wd, dataType, xFile, NULL, NULL)
  x_headers <- loadData(wd, "", "features.txt", NULL, NULL)
  headersVector <- unlist(x_headers[,2])
  names(x_data) <- headersVector
  y_data <- loadData(wd, dataType, yFile, NULL, NULL)
  names(y_data) <- c("activity")
  x_data <- cbind(y_data[,1],x_data)
  subject_data <- loadData(wd, dataType, subjectsFile, NULL, NULL)
  names(subject_data) <- c("subjectId")
  x_data <- cbind(subject_data[,1],x_data)
  return (x_data)
}

## selectCols: This function selects only the columns which describe the mean or standard
##             deviation as requested in the assignment
selectCols <- function(df)
{
  colNames <- names(df)
  means <- grep("*mean\\(\\)",colNames)
  stds <- grep("*std()\\(\\)",colNames)
  cols <- c(c(1,2),means,stds)
  return(cols)
}


## runAnalysis: The main function. Calls all of the above functions.
runAnalysis <- function()
{
  wd <- "UCI HAR Dataset"
  
  ## Answer to Q1
  x_test <- compileData(wd, "test")
  x_train <- compileData(wd, "train")
  fullDataset <- rbind(x_train,x_test)
  
  ## Answer to Q2
  colsIds <- selectCols(fullDataset)
  partialDataset <- subset(fullDataset, select=colsIds)
  
  ## Answer to Q3+Q4
  library(dplyr)
  lookupTable <- loadData(wd, "", "activity_labels.txt", NULL, NULL)
  colnames(lookupTable) <- c("activity","activityName")
  pdWithLookupLabels <- left_join(lookupTable, partialDataset, by="activity")
  
  
  ## Answer to Q5
  tidyData <-pdWithLookupLabels %>% 
                select(-activity) %>% 
                group_by(subjectId,activityName) %>% 
                summarise_all(mean)
  write.csv(tidyData,"tidydata-Q5.csv")
}