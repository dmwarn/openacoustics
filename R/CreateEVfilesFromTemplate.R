#' Create EV Files from a Template
#'
#' @param FileSetName character vector providing name of fileset on which to
#' conduct operations.
#' @param SvVarName character vector naming the Sv variable to be created and
#' edited. Must match the name of the variable in the template used.
#' @param TsVarName character vector naming the single target variable to be created
#' and edited. Must match the name of the variable in the template used.
#' @param UpperLineName character vector naming the upper line to be used for
#' exclusion. Also used in creation of line-relative region.
#' @param LowerLineName character vector naming the upper line to be used for
#' exclusion. Also used in creation of line-relative region.
#' @param CreateAnalysisRegion logical vector indicating whether or not the
#' user wants to create a line-relative region between UpperLineName and LowerLineName
#' and spanning the entire echogram.
#'
#' @details This function can be used to create EV files from a template, pick a
#' bottom line from an Sv variable named by the user, set exclusion lines for that
#' Sv variable and a TS variable named by the user, and create a line-relative
#' analysis region covering the entire span of the data in the EV file.
#'
#' Though not included in the parameter list above, the user will be prompted to
#' select
#' 1. A pre-existing folder in which to save the EV files.
#' 2. The folder in which raw data files are stored. This folder must have
#' subfolders with transect names that contain the raw data for the respective
#' transects. It works well for multi-ship surveys to have a folder called transects
#' or raw data, a next-level folder for each ship, and next-level folders for each transect.
#' 3. The EV file template
#' 4. The ecs file associated with the template.
#' @return
#' @export
#' @author David Warner and Kristy Phillips
CreateEVFilesFromTemplate <- function(FileSetName = NULL,
                                      SvVarName = NULL,
                                      TsVarName = NULL,
                                      UpperLineName = NULL,
                                      LowerLineName = NULL,
                                      CreateAnalysisRegion = TRUE
) {
  options(stringsAsFactors = FALSE)
  dd <-
    choose.dir(caption = 'Folder where you want your new EV files saved')

  #Get list of raw files with full path....this path should be to the location of the folder containing subfolders that represent individual transect and the folder names are used as transect names
  path <-
    choose.dir(caption = "Input folder where transect subfolders of raw/DT4 files are located.")

  #Selet an Echoview template to use for creating the EV files
  my.template <-
    choose.files(caption = "Echoview template you wish to use.")
  my.ECS <-
    choose.files(caption = "Echoview ECS file you wish to use.")

  all_dt4_files <-
    list.files(path,
               pattern = "dt4$|raw",
               recursive = TRUE,
               full.names = TRUE)

  #split above so that you just have the name of the dt4 file
  my.filenames <-
    data.frame(filename = matrix(unlist(all_dt4_files), byrow = T),
               stringsAsFactors = FALSE) #this creates
  #data.frame with filename as a field
  my.filenames.split <-
    strsplit(my.filenames$filename, "/") # this creates a list of parts of the path

  transects <-
    (data.frame(transect = do.call(
      "rbind", lapply(my.filenames.split, "[[", 2)
    ))) #this creates data.frame that has transect
  #name as a field
  tran.and.file <-
    data.frame(transect = transects$transect,
               filename = my.filenames$filename)
  uniqueTransect <- unique(transects$transect)



  EVAppObj <- COMCreate('EchoviewCom.EvApplication')

  for (i in 1:length(uniqueTransect)) {
    Sys.sleep(1) #this is present to slow R down to a speed Echoview can handle
    try(EVFile <- EVCreateNew(
      EVAppObj = EVAppObj,
      templateFn = my.template,
      EVFileName = paste(dd, '/', uniqueTransect[i], '.ev', sep =
                           ''),
      filesetName = "Fileset 1",
      dataFiles = as.character(tran.and.file$filename[tran.and.file$transect ==
                                                        uniqueTransect[i]]),
      CloseOnSave = FALSE
    )$EVFile)
    Sys.sleep(1)
    #add a calibration file
    EVAddCalibrationFile(
      EVFile = EVFile,
      filesetName = "Fileset 1",
      calibrationFile = my.ECS
    )
    Sys.sleep(1)
    Sv <- EVAcoVarNameFinder(EVFile, SvVarName)$EVVar
    #EVVar<-EVFile[["Variables"]]$FindByName('120 kHz Sv minus noise')
    Sys.sleep(1)

    #Set Bottom detection settings
    setlp <- EVFile[["Properties"]][["LinePick"]]
    setlp[["Algorithm"]] <-
      2 # 0 for Delta Sv, 1 for Maximum Sv, 2 for Best bottom Candidate
    setlp[["StartDepth"]] <- 3
    setlp[["StopDepth"]] <- 275
    setlp[["MinSv"]] <- -70
    setlp[["UseBackstep"]] <- "True"
    setlp[["DiscriminationLevel"]] <- -50.0
    setlp[["BackstepRange"]] <- -0.50
    setlp[["PeakThreshold"]] <- -50.0
    setlp[["MaxDropouts"]] <- 2
    setlp[["WindowRadius"]] <- 8
    setlp[["MinPeakAsymmetry"]] <- -1.0

    #Detect Bottom
    try(bottom <- EVFile[["Lines"]]$CreateLinePick(Sv, T))
    #bottom[['Name'()]]
    #Overwrite old bottom line (if needed)
    oldbottom <- EVFindLineByName(EVFile, "bottom")
    oldbottom$OverwriteWith(bottom)
    #EVDeleteLine(EVFile, 'Line 1')

    EVsetExclusionLines(
      EVFile = EVFile,
      acoVarName = SvVarName,
      newAboveExclusionLine = 'surface',
      newBelowExclusionLine = 'bottom'
    )
    Sys.sleep(1)
    EVsetExclusionLines(
      EVFile = EVFile,
      acoVarName = TsVarName,
      newAboveExclusionLine = 'surface',
      newBelowExclusionLine = 'bottom'
    )

    Sys.sleep(1)
    EVNewLineRelativeRegion(
      EVFile,
      SvVarName,
      uniqueTransect[i],
      "surface",
      "bottom",
      firstPing = NA,
      lastPing = NA
    )

    EVSaveFile(EVFile = EVFile)
    Sys.sleep(1)

    EVCloseFile(EVFile = EVFile)
    Sys.sleep(1)
  }
  QuitEchoview()
}
