#' Export Sv and TS distribution by region-cell from a folder of EV files.
#'
#' This function runs region-by cell exports of Sv and TS frequency distribution 
#' for EV files identified in a single folder selected by the user and exports them to 
#' the output directory selected by the user. Can of course be used to process
#' multiple folders of data with additional code. The function also exports 
#' @param lake Two letter abbreviation for the lake from which data were collected. This must
#' match the characters identifying the lake or survey in the EchoNet2Fish directory. For 
#' example, if the EchoNet2Fish processing folder is MI2019, the correct value for this parameter
#' would be "MI".
#' @param year Four digit year in which the data were collected. Used in directing output.
#' @param dat.dir Parent directory in which one or more folders containing the EV files of interest
#' are located. 
#' @param exp.dir Output directory for the Sv and TS files. Currently the intent is for the exported
#' csv files to go to the Sv and TS directories in the EchoNet2Fish directories if the lake-year being 
#' processed. The Sv and TS directorys MUST exist already. The function does not create them. The function 
#' use the 
#' @param SvacoVarName The name of the Sv variable from which to export
#' integration results.
#' @param TSacoVarName The name of the single targets variable from which
#' to export the single target frequency distribution.
#' @param regionClassName The name of the class of the region(s) from which
#' exports should be made.
#' @return
#' This function generates csv files of Sv and TS frequency distribution data 
#' with the result being one of these files for each EV file processed. The 
#' exported csv files are saved in an Sv and TS subfolder of the year-lake
#' specific EchoNet2Fish subfolder. 
#' @keywords Echoview COM scripting
#' @export
#' @references \url{http://support.echoview.com/WebHelp/Echoview.htm/}

ExportSvTSdistRegionbyCell <-
  function(lake,
           year,
           dat.dir,
           exp.dir,
           SvacoVarName,
           TSacoVarName,
           regionClassName) {
    dat.dir <- paste0(choose.dir(caption = "Select the folder in which EV_file folders are found"))
    #dat.dir <- choose.dir()
    all.ev.dirs <- Sys.glob(file.path(dat.dir), dirmark = TRUE)
    all.evfiles <-
      list.files(all.ev.dirs, pattern = "EV|ev", full.names = TRUE, recursive = TRUE)
    scrap.files <-
      grep("evwx|evb|csv|evi|CAL|cal|evw|backup|NOISE|noise|template|GTB|EVR|evr|html|
           Rmd|RData", all.evfiles, value = TRUE)
    
    #now we have a list of all ev files.
    evfiles <- setdiff(all.evfiles, scrap.files)
    
    #This is the base path of the EchoNet2Fish folder where you want exports to go.
    exp.dir <- choose.dir(caption = "Select the EchoNet2Fish folder")
     EVAppObj <- COMCreate('EchoviewCom.EvApplication')
    for (i in 1:length(evfiles)) {
      try(EVFile <- EVAppObj$OpenFile(evfiles[i]))
      Sys.sleep(1)
      EVname <- evfiles[i]
      exp.path <- strsplit(EVname, "[\\]")
      trans.name <- strsplit(basename(evfiles[i]), "[.]") 
      exp.fname.sv <-
        paste0(exp.dir,
               "\\",
               lake,
               year,
               "\\Sv\\",
               "Sv_",
               trans.name[[1]][[1]],
               ".csv")
      exp.fname.svimg <-
        paste0(exp.dir,
               "\\",
               lake,
               year,
               "\\Sv\\",
               "Sv_Echogram_",
               trans.name[[1]][[1]],
               ".png")
      exp.fname.ts <-
        paste0(exp.dir,
               "\\",
               lake,
               year,
               "\\TS\\" ,
               "TS_",
               trans.name[[1]][[1]],
               ".csv")
      
      Sys.sleep(1)
      try (EVIntegrationByRegionsByCellsExport_Use_EV_thresholds(
        EVFile,
        SvacoVarName,
        regionClassName,
        exportFn = exp.fname.sv
      ))
      try(EVFreqDistRegionsByCellsExport(EVFile, TSacoVarName, regionClassName, exp.fname.ts))
      Sys.sleep(1)
      EVCloseFile(EVFile = EVFile)
    }
    QuitEchoview(EVAppObj)
  }
