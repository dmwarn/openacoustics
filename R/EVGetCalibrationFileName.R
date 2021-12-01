#'Get Name of ECS File for an EV File
#'
#'This reads the name and path of the ECS file for a given
#'EV file.  
#'@details None to mention
#'@param EVFile Echoview file COM object
#'@export 


EVGetCalibrationFileName <- function (EVFile, filesetName) {
  
  
  fileset <- EVFile[["Filesets"]]$FindByName(filesetName)
  
  if (is.null(fileset)) {
    stop(paste(Sys.time(), "Error: Couldn't file the fileset", filesetName, sep = " "))
  } else {
    message(paste(Sys.time(), "Found the fileset", filesetName, sep = " "))
  }
  calibration.file <- fileset$GetCalibrationFileName()
  
  return(calibration.file)
  
}