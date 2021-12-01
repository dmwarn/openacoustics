#' Function to export TS frequency distribution.
#' 
#' 
#' This function runs region-by cell exports of TS frequency distribution 
#' for EV files identified in the data directory selected and exports them to 
#' the output directory selected. Can be called on its own but is also used in 
#' the function ExportSvTSdistRegionbyCell.
#' @param EVFile 
#' @param TSacoVarName 
#' @param regionClassName 
#' @param exportFn
#'
#' @return
#' This function exports single target frequency distribution by region-cells.
#' @keywords Echoview COM scripting
#' @export
#' @references \url{http://support.echoview.com/WebHelp/Echoview.htm/}
#' @export
#' 


EVFreqDistRegionsByCellsExport <- function (EVFile, TSacoVarName, regionClassName, exportFn) {
  #EVFile <- EVAppObj$OpenFile(uniqueTransect[i])
  EVVar <- EVFile[["Variables"]]$FindByName(TSacoVarName)
  acoVarObj <- EVAcoVarNameFinder(EVFile = EVFile, acoVarName = TSacoVarName)
  msgV <- acoVarObj$msg
  acoVarObj <- acoVarObj$EVVar
  EVRC <- EVRegionClassFinder(EVFile = EVFile, regionClassName = regionClassName)
  msgV <- c(msgV, EVRC$msg)
  RC <- EVRC$regionClass
  success <- acoVarObj$ExportFrequencyDistributionByRegionsByCells(exportFn, RC)
  if (success) {
    msg <- paste(Sys.time(), " : Successful TS frequency dist. ",
                 regionClassName, sep = "")
    message(msg)
    msgV <- c(msgV, msg)
  }
  else {
    msg <- paste(Sys.time(), " : Failed to or export ",
                 regionClassName, sep = "")
    warning(msg)
    msgV <- c(msgV, msg)
  }
}