#' Update EV File Data Paths and Change ECS file
#'
#' This function can be used to add a new path for data files to the
#' EV file properties for a list of EV files. 
#' 
#' @details This function assumes a specific file storage pattern that may not 
#' match that of many or most users. The EV files and ECS files are stored as 
#' "C:/Acoustics/2019/vessel", where vessel is the name of the vessel and the 
#' folder contains a vessel specific ECS file. Some use ECS file specific to
#' an EV file. Data files are stored, as an example, as "C:/Acoustics/2019/raw_data",
#' with sub folders in the "raw_data" folder whose names match the names of the EV
#' files. This approach is set up so that EV files are each a distinct transect,
#' 
#' @param filesetName Character vector for name of fileset
#' @param UpdateECS Logical stating whether or not to update the ECS file
#' @return nothing
#' @export
#'

UpdateDataPathAndECS <- function (file.list, filesetName = "Primary Fileset", UpdateECS = FALSE)
{
options(stringsAsFactors = FALSE)

# Select the parent directory containing subdirectories with EV files. Expectation is that
# the function will scrape all sub folders for files that have EV or ev in the name.
the.ev.dir <- choose.dir(caption = "Choose the parent directory where EV file subdirectories are located")
# Select the parent directory containing subdirectories with raw data files. Expectation is that
# the raw data for each EV file are in a folder of their own with a folder name matching the name
# of the EV file. 
newdatapath <-
  choose.dir(caption = "Choose folder where transect subfolders of DT4/raw files are located.")

# Get the list of all files with EV or ev in the name.
all.evfiles <- list.files(the.ev.dir, pattern="EV|ev", full.names = TRUE,
                          recursive = TRUE)
# Create a list of file name patterns that we don't want. May require editing by the user.
# 
scrap.files <-
  grep("evwx|evb|csv|evi|CAL|cal|evw|backup|NOISE|noise|template|GTB|EVR|evr|html|Rmd|RData", all.evfiles, value = TRUE)

#now we have a list of all ev files by getting rid of anything we did not want that showed up 
# by mistake. 
evfiles <- setdiff(all.evfiles, scrap.files)

# Get list of ECS files. These live in the EV file subfolders. USGS example is top level folder
# is year (e.g. 2019), next level folders include vessel specific folder of EV files like
# EVfiles_sturgeon which contain the EV files AND the ECS file for the vessel.
all_ecs_files <- list.files(the.ev.dir, pattern = "ecs$", recursive=TRUE,full.names = TRUE)

file.list <- evfiles

filesetName <- "Primary fileset"



EVAppObj <- COMCreate('EchoviewCom.EvApplication')

for (i in 1:length(file.list)) {
EVFile <- EVAppObj$OpenFile(file.list[i])
fileset <- EVFindFilesetByName(EVFile, filesetName)$fileset
# This creates the data file path to add to the EV file properties
newdatafullpath <- paste0(newdatapath, "/", strsplit(basename(file.list[i]), split = "\\.")[[1]][1])

EVFile[["Properties"]][["DataPaths"]]$Add(newdatafullpath)
# This only happens if TRUE because you want to replace ECS path. Can't add one via COM
if(UpdateECS ==TRUE) {
  new.ecs.df <- data.frame(ecs.path = grep("Sturgeon|Steelhead|Baird|LTBB", all_ecs_files, 
                                           value = TRUE), ecs.basename = 
                             basename(grep("Sturgeon|Steelhead|Baird|LTBB", all_ecs_files, 
                                           value = TRUE)))
  ecs.file.orig <- basename(EVGetCalibrationFileName(EVFile = EVFile, filesetName = filesetName))
  ecs.file.new <- new.ecs.df$ecs.path[new.ecs.df$ecs.basename == ecs.file.orig ]
  
  fileset$SetCalibrationFile(ecs.file.new) 
}


try(EVSaveFile(EVFile = EVFile))

try(
  EVCloseFile(EVFile = EVFile) #close the EV file so you don't have a ton of EV files open.
)
}
QuitEchoview(EVAppObj)
}

