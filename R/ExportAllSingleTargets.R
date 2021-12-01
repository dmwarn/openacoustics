#' Export Properties for All Single Targets in Selected EV Files
#'
#' @param dat.dir Character vector representing name of the directory with EV files. This
#' must be the folder containing the EV file or files, not an EV file itself. Will search
#' the folder recursively to find any EV files.
#' @param out.dir Character vector representing name of the directory to which single
#' target exports should be saved.
#' @param TargetsVarName Character vector representing name of the single targets variable.
#' @param start.ping Integer vector stating the starting ping of the analysis domain.
#' @param end.ping Integer vector stating the end ping of the analysis domain.
#' @return
#' This function will open an Echoview instance and export the properties of all single targets
#' from the names single target variable in each EV file.
#' @import RDCOMClient
#' @export
ExportAllSingleTargets <- function(dat.dir = NULL, out.dir = NULL, TargetsVarName = NULL,
                                   start.ping = 1, end.ping = 1000000 ) {
  options(stringsAsFactors = FALSE)
  dat.dir <-
    choose.dir(caption = "Select folder with EV files")
  out.dir <-
    choose.dir(caption = "Select folder for output")

  ev.folder.list <-
    list.files(dat.dir, pattern = "EVfiles", full.names = TRUE)

  all.evfiles <-
    list.files(
      ev.folder.list,
      pattern = "EV|ev",
      full.names = TRUE,
      recursive = TRUE
    )

  scrap.files <-
    grep(
      "evwx|evb|csv|evi|CAL|cal|evw|backup|NOISE|noise|template|GTB|gtb|EVR|evr|Rmd|R|RData",
      all.evfiles,
      value = TRUE
    )
  evfiles <- setdiff(all.evfiles, scrap.files)

  # Establish the name of the single targets variable
  acoVar <- TargetsVarName

  for (i in 1:length(evfiles)) {

    EVAppObj <- COMCreate('EchoviewCom.EvApplication')
    #open the ith EV file
    EVFile = EVAppObj$OpenFile(evfiles[i])
    EVVar <- EVFile[["Variables"]]$FindByName(acoVar)
    exportFn <- paste0(out.dir,
                     basename(evfiles[i]), " single_targets.csv")

    EVVar$ExportData(exportFn, start.ping, end.ping)
    try(EVAppObj$CloseFile(EVFile))
}
QuitEchoview(EVAppObj)

}
