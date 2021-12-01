#' Get the names of raw data files used in the EV file from
#' which sphere target data were exported.
#'
#' @param file.list
#'   This is essentially one EV file selected by the user.
#'
#' @import EchoviewR
#' @return
#' Returns the full filename for the raw data files in the EV file
#' represented by "file.list".
#'
#' #' @details
#' This function is a helper function for Sphere2BeamPattern and is not called
#' on its own.



GetEVRawPaths <- function(file.list)
{
  library(EchoviewR)
  #file.list <- choose.files()
  # now a for loop to get raw data file list for each EV file in all_EV_files nested in a loop to
  # open each of the EV files in all_EV_files and then close each EV file
  EVAppObj <- COMCreate('EchoviewCom.EvApplication')
  # create an empty list in which to store the k data frames
  klist <- list()
  file.list <- choose.files()

  for (k in 1:length(file.list)) {
    try(
      EVFile <- EVAppObj$OpenFile(file.list[k]) #Selects the kth EV file in the list and opens it.
    )
    Sys.sleep(1)   #This slows R down to wait for Echoview.  Generally also fails without this.

    filesetName <- EVFile[["Filesets"]]$Item(0)$Name()
    try(
      fileset.loc <- EVFindFilesetByName(EVFile, filesetName)$filesetObj #select the fileset COM obj for the named fileset
    )
    nbr.of.raw.in.fileset <- fileset.loc[["DataFiles"]]$Count() # identify # of raw data files in the kth EV file/fileset
    raw.file.names <- NA #create file.names object, will be overwritten with character vector of all the data file paths
    # loop gets file.path for each DT4 file associated with the kth EV file.
    ev.filename <- file.list[k]
    for (j in 0:(nbr.of.raw.in.fileset - 1)) {
      raw.file.names[j + 1] <- file.path(fileset.loc[["DataFiles"]]$Item(j)$FileName())   #Loop through the j raw data files
    } # close inner loop
    # grabbing their full names and creating a character vector of the full names
    files <- data.frame(ev.filename = ev.filename, raw.file.names = raw.file.names) # convert the character vector of file names for the kth EV file
    klist[[k]] <- files
    try(
      EVCloseFile(EVFile = EVFile) #close the EV file so you don't have a ton of EV files open.
    )
    Sys.sleep(1)
  } #Close outer loop
  EVAppObj$Quit()

  # combine all of the k data frames into one big data frame
  onebigdf <<- dplyr::bind_rows(klist)

  write.csv(onebigdf, paste0(ev.filename, "_raw_files_list.csv"), row.names=FALSE)  #Save a csv file that includes the
  # the full path of the raw data files for each EV file, the full name of that EV file, and the name of the ECS file used
  # by each EV file.
}










