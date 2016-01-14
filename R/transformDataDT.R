globalVariables(c("Type","Display.Name",":=",".SD","Behavior",".","XML.Attribute","new.name"))
.datatable.aware <- TRUE

#' @title Read data into R data.table
#' 
#' @description Reads the csv data file received from the Adwords API into a data.table
#'  The function is used inside \code{\link{transformData}} when the data.table package is available.
#' 
#' @param data Raw csv data from Adwords API.
#' @param report filename with report field metadata
#' 
#' @return data.table with the Adwords Data.
transformDataDT <- function(data, report, apiVersion){
  
  #get metrics for requested report
  report <- gsub('_','-',report)
  report <- paste(system.file(package="RAdwords"),'/extdata/api',apiVersion,'/',tolower(report),'.csv',sep='')
  
  data <- suppressWarnings(data.table::fread(data))
  
  if(nrow(data)>0){
    if(tail(data[[1]],n=1) == "Total"){
      data <- head(data, n=-1)
    }  
  }
  
  
  #read report fields from file
  reportType <- suppressWarnings(data.table::fread(report))
  data.table::setnames(reportType, gsub(" ",".", names(reportType)))
  
  #change data format of variables
  
  #convert date variables
  dateVar <- reportType[Type == 'Date', Display.Name, with=T]
  dateVar <- intersect(names(data), dateVar)
  if(length(dateVar)>0) {
    data[, (dateVar):=lapply(.SD, function(x) as.Date(x)), .SDcols=dateVar]
  }
  
  #elimitate % in numeric data (Type=Double) however ignore % in non-double variables like ad text
  #and convert percentage values into numeric data
  #define double variables
  doubleVar <- reportType[Type == 'Double', Display.Name]
  
  #find variables containing %
  perVar <- data[, names(which(sapply(.SD, function(x) length(grep('%', x))>0))), .SDcols=intersect(doubleVar, names(data))]
  
  #transform variable of type double which contain %
  if(length(perVar)>0) {
    data[, (perVar):=lapply(.SD, function(x) as.numeric(gsub("%|<|>", "", x))/100), .SDcols=perVar]
  }
  
  #eliminate ',' thousend separater in data and convert values into numeric data
  metricVar <- reportType[Behavior == 'Metric', Display.Name]
  metricVar <- intersect(names(data), metricVar)
  if(length(metricVar)>0) {
    data[, (metricVar):=lapply(.SD, function(x) as.numeric(sub(',','',as.character(x)))), .SDcols=metricVar]
  }
  
  #since v201409 returnMoneyInMicros is deprecated, convert all monetary values
  monetaryVar <- reportType[Type == "Money", Display.Name]
  monetaryVar <- intersect(names(data), monetaryVar)
  if(length(monetaryVar)>0){
    data[, (monetaryVar):=lapply(.SD, function(x) suppressWarnings(as.numeric(as.character(x)))/1e6), 
         .SDcols=monetaryVar]
  }
  
  #replace column xml attribute names
  colsToChange <- reportType[Display.Name %in% names(data), .(Display.Name, XML.Attribute)]
  colsToChange[, new.name:=tolower(gsub("([[:upper:]])", ".\\1", XML.Attribute))]
  data.table::setnames(data, colsToChange[, Display.Name], colsToChange[, new.name])
  
  data
}