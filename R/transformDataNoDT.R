#' @title Read data into R dataframe
#' 
#' @description Reads the csv data file received from the Adwords API into a dataframe.
#'  The function is used inside \code{\link{transformData}} when the data.table package is not available.
#' 
#' @param data Raw csv data from Adwords API.
#' @param report filename with report field metadata
#' 
#' @importFrom utils read.csv2
#' 
#' @return Dataframe with the Adwords Data.
transformDataNoDT <- function(data, report){
  data <- read.csv2(textConnection(data),sep=",",header=F)[-1,]
  data <- as.data.frame(data)
  #Rename columns
  for(i in 1:ncol(data)){
    names(data)[i] <- as.character(data[1,i])
  }
  
  if(ncol(data)==1){
    variableName <- names(data)
    data <- as.data.frame(data[2:(nrow(data)-1),1])
    names(data) <- variableName
  }
  else if(ncol(data)>1) {
    #eliminate row with names
    data <- data[-1,]
    #eliminate row with total values
    data <- data[-nrow(data),]
  }
  
  #change data format of variables
  
  #transform factor into character
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)
  
  #read report fields from file
  reportType <- read.csv(report, sep = ',', encoding = "UTF-8")
  
  # Convert date variables
  Type <- NULL
  dateVar <- as.character(subset(reportType, Type == "Date")$Display.Name)
  for (var in dateVar) {
    if (var %in% colnames(data)) {
      data[,var] <- as.Date(data[,var])
    }
  }
  
  #elimitate % in numeric data (Type=Double) however ignore % in non-double variables like ad text
  #and convert percentage values into numeric data
  #define double variables
  Type <- NULL # pass note in R CMD check
  doubleVar <- as.character(subset(reportType, Type == 'Double')$Display.Name)
  #find variables containing %
  perVar <- as.numeric(grep("%",data))
  perVar <- names(data)[perVar]
  #transform variable of type double which contain %
  for(var in doubleVar){
    if(var %in% colnames(data) && var %in% perVar){
      data[,var] <- sub("%","",data[,var])
      data[,var] <- as.numeric(data[,var])/100 
    }
  }
  #   perVar <- as.numeric(grep("%",data))
  #   #kill % and divide by 100
  #   for(i in perVar){
  #     data[,i] <- sub("%","",data[,i])
  #     data[,i] <- as.numeric(data[,i])/100 
  #   }
  
  Behavior = NULL
  #eliminate ',' thousend separater in data and convert values into numeric data
  metricVar <- as.character(subset(reportType, Behavior == 'Metric')$Display.Name)
  for(var in metricVar){
    if(var %in% colnames(data)){
      data[,var] <- as.character(data[,var])
      data[,var] <- sub(',','',data[,var])
      data[,var] <- as.numeric(data[,var])
    }
  }
  
  #since v201409 returnMoneyInMicros is deprecated, convert all monetary values
  Type <- NULL
  monetaryVar <- as.character(subset(reportType, Type == "Money")$Display.Name)
  for (var in monetaryVar) {
    if (var %in% colnames(data)) {
      data[,var] <- as.character(data[,var]) #Variables like Max. CPC are not recognized as metric in previous task since their "Behavior" is "Attribute". Hence convert all "Money" metrics in numeric again.
      data[,var] <- suppressWarnings(as.numeric(data[,var]))
      data[, var] <- data[, var] / 1000000 #convert into micros
    }
  }
  
  #eliminate " " spaces in column names
  names(data) <- gsub(" ","",names(data))
  
  data
}