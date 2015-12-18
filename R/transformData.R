#' @title Transform data into R dataframe
#' 
#' @description Transforms the csv data file received from the Adwords API into a dataframe. Moreover the variables are converted into suitable formats.
#'  The function is used inside \code{\link{getData}} and parameters are set automatically.
#' 
#' @param data Raw csv data from Adwords API.
#' @param report Report type.
#' @param apiVersion set automatically by \code{\link{getData}}. Supported are 201509 or 201506. Default is 201509.
#' 
#' @importFrom utils read.csv read.csv2
#' @export
#' 
#' @return Dataframe with the Adwords Data.
transformData <- function(data,
                          report,
                          apiVersion="201509"){
  # Transforms the csv into a dataframe. Moreover the variables are converted into suitable formats.
  #
  # Args:
  #   data: csv from Adwords Api
  #   report: Report type
  #
  # Returns:
  #   R Dataframe
  
  #get metrics for requested report
  report <- gsub('_','-',report)
  report <- paste(system.file(package="RAdwords"),'/extdata/api',apiVersion,'/',tolower(report),'.csv',sep='')

  if (requireNamespace("data.table", quietly = TRUE)) {
    data <- transformDataDT(data, report)
  } else {
    data <- transformDataNoDT(data, report)
  }
  
  return(data)
}
