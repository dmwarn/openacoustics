#' Export integration by regions by cells from an Echoview acoustic variable
#' 
#' This function performs integration by regions by cells for a specified region class and exports the results using COM scripting. The difference between this function and the original
#' function EVIntegrationByRegionsByCellsExport is that this version does not require the
#' user to provide a data threshold. This function was created because it was not clear
#' if the original function used the thresholds applied in the EV file or not.
#' 
#' @param EVFile An Echoview file object
#' @param acoVarName A string containing the name of an Echoview acoustic variable
#' @param regionClassName A string containing the name of an Echoview region class
#' @param exportFn export filename and path 
#' @return a list object with one element, $msg: message for processing log.
#' @keywords Echoview COM scripting
#' @export
#' @references \url{http://support.echoview.com/WebHelp/Echoview.htm/}
#' @seealso \code{\link{EVOpenFile}}
#' @examples
#' \dontrun{
#' EVAppObj <- COMCreate('EchoviewCom.EvApplication')
#' EVFile <- EVOpenFile(EVAppObj,'~~/KAOS/KAOStemplate.EV')$EVFile
#' EVIntegrationByRegionsByCellsExport(EVFile, "120 aggregations", "aggregations", exportFn = "~~/KAOS/EVIntegrationByRegionsExport_example.csv")
#' }
EVIntegrationByRegionsByCellsExportUseEVFileThresholds <- function (EVFile, acoVarName, regionClassName, exportFn) {
  
  acoVarObj <- EVAcoVarNameFinder(EVFile = EVFile, acoVarName = acoVarName)
  msgV      <- acoVarObj$msg
  acoVarObj <- acoVarObj$EVVar
  EVRC      <- EVRegionClassFinder(EVFile = EVFile, regionClassName = regionClassName)
  msgV      <- c(msgV, EVRC$msg)
  RC        <- EVRC$regionClass

  
  msg <- paste(Sys.time(), ' : Starting integration and export of ', regionClassName, sep = '')
  message(msg)
  success <- acoVarObj$ExportIntegrationByRegionsByCells(exportFn, RC)
  
  if (success) {
    msg <- paste(Sys.time(), ' : Successful integration and export of ', regionClassName, sep = '')
    message(msg)
    msgV <- c(msgV, msg)
  } else {
    msg <- paste(Sys.time(), ' : Failed to integrate and/or export ', regionClassName, sep = '')
    warning(msg)
    msgV <- c(msgV, msg)
  } 
  
  invisible(list(msg = msgV))
}