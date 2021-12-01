#' Beam Pattern Estimates from Calibration Sphere Data
#'
#' Estimate 3 dB beam angles, angle offsets, and TS gain using
#' the formula employed by Simrad Lobe program. Creates graphs
#' showing sphere TS across the beam and a summary text file that
#' includes numerous pieces of information to help make the process
#' reproducible.
#' @param UseEchoview
#'   Logical scalar indicating if the user wishes to use Echoview
#'   to automatically determine the fileset that includes the sphere
#'   data variable used to export the single targets. Default is FALSE,
#'   and if the default is used, the user must supply a fileset name.
#' @param filesetName
#'   A character scalar giving the name of the Echoview fileset, which can
#'   be identified automatically by setting the UseEchoview parameter to TRUE.
#'   \code{\link{GetEVRawPaths}}.
#' @import EchoviewR tcltk plyr ggplot2
#' @return
#'   Several items included text file (.txt), a csv file, and graphs are saved
#'  to a folder valled "Calibration OUTPUT" created by this function.
#'
#'   Seven different data frames are saved as objects in an Rdata
#'   file and are written to csv files in \code{maindir}:
#'   \itemize{
#'     \item \code{calculated TS.csv} = the original spehre data input along
#'     with the sphere TS calculated with the estimated beam pattern.
#'     \item \code{Observed vs Corrected plot - Along axis.png} = a plot with
#'     original (from Echoview, default beam settings) and corrected sphere TS
#'     versus along-axis beam angle.
#'     \item \code{Observed vs Corrected plot - Athwart axis.png} = a plot with
#'     original (from Echoview, default beam settings) and corrected sphere TS
#'     versus Athwart-axis beam angle.
#'     \item \code{Summary info.txt} = a text file containing date, equipment
#'     identifiers and default parameters, the EV file name, the raw data files
#'     used, and the output of the beam pattern fitting process.
#'     \item \code{TS gradient plot.png} = bullseye plot of sphere targets
#'     in the acoustic beam with coloring according to the TS.
#'
#'   }
#'   The files all have a prefix = the name of the sphere input data
#'   file.
#'
#' @details
#' This software is intended to aid those attempting to estimate transducer beam
#' patterns using tungsten carbide calibration sphere single target data. The function
#' uses the optimization routine optim() to minimize RMSE of a modified Bessel function
#' (Ona 1990, ICES, B:30 Sess. R.). Some users employ a two-dimensional polynomial.
#'
#'
#' 1. Adjust theoretical sphere TS above based on calcaulated TS by entering water temp, sphere size, frequency, etc (see. southwest fisheries science center website...).
#' here https://swfscdata.nmfs.noaa.gov/AST/SphereTS/
#'
#' 2. Select all, press 'Run' or hit ctrl-enter on the keyboard to run script.
#'
#' 3. Will be prompted to select an INPUT file. This should be exported single target TS data in .csv format. Only one file can be selected at a time. Select 'open'.
#'
#' 4. Check Calibration output folder and view plots.
#'
#' 5. Repeat as necesssary for all frequencies.
#'
#'
#'
#'
Sphere2BeamCal<-
  function(UseEchoview =FALSE,
           filesetName = NULL)
  {
    source("R/inputs.R")
    source("R/GetEVRawPaths.R")
    onebigdf<-data.frame()
    myvals <-inputs()
    Frequency <- myvals[1]
    Transducer_serial_number <- myvals[2]
    Echosounder_serial_number <- myvals[3]
    sphere.ts <- as.numeric(myvals[4])
    athw.angle <- as.numeric(myvals[5])
    along.angle <- as.numeric(myvals[6])
    min.ts <- as.numeric(myvals[7])
    max.ts <- as.numeric(myvals[8])




    # Select input file (raw echoview TS exports)
    path <- choose.files(caption="Select INPUT file (exported single target TS data). ")
    path2 <- choose.files(caption="Select source EV file. ")
    # Define directories based on selected input files. Create output directory as needed...
    EV_filename_for_sphere<-path2
    #filesetName <- "Primary fileset"
    #EVAppObj <- COMCreate('EchoviewCom.EvApplication')
    file.list <- EV_filename_for_sphere
    GetEVRawPaths(file.list)

    file.name <- basename(path)
    INPUT <- dirname(path)
    dir.create(paste0(INPUT, "\\Calibration OUTPUT"), showWarnings=T)
    OUTPUT <- paste0(dirname(path), "\\Calibration OUTPUT\\")

    # load ts data  (exported from echoview)
    raw.ts <- read.csv(path)

    # cut down to only columns of interest
    cut.ts <- raw.ts[,c("Target_on_axis_depth","TS_comp","TS_uncomp","Angle_minor_axis","Angle_major_axis" )]
    cut.ts <- cut.ts[cut.ts$TS_comp >= min.ts & cut.ts$TS_comp < max.ts,]

    # Define input parameters
    # These are the suggested starting values for the optimization step.  They should match the values stored in the transducer ROM chip
    # MUST MATCH THE TRANSDUCER YOU ARE CALIBRATING.  These values can be viewed by right clicking on an echogram from this transducer
    # and selecting variable properties then calibration tab,
    # major axis (athw) beam angle
    #athw.angle <- 7.6

    # minor axis (along) beam angle
    #along.angle <- 7.6

    # gain correction
    gain.cor <- 0

    # major offset (athwart)
    athw.offset <- 0

    # minor offset (along)
    along.offset <- 0

    ################### Sphere TS #################################
    #sphere.ts <- -40.77 # Change this value based on frequency used!

    #### Part 2. Parameter calculations #####

    # List parameters to be optimized
    pars <- c(athw.angle, along.angle, gain.cor, athw.offset, along.offset)

    # Function to calculate root mean square error (to be minimized)
    RMSE.calc <- function(dat, par) {
      expand.ts <- dat
      expand.ts$along.minor.offs = expand.ts$Angle_minor_axis - par[5]
      expand.ts$athw.major.offs = expand.ts$Angle_major_axis - par[4]
      expand.ts$x = expand.ts$along.minor.offs/par[2]
      expand.ts$y = expand.ts$athw.major.offs/par[1]
      expand.ts$B = 24 * (expand.ts$x^2 + expand.ts$y^2)
      expand.ts$TS_c = expand.ts$TS_uncomp + expand.ts$B + par[3]
      expand.ts$diff.TS = expand.ts$TS_c - sphere.ts
      expand.ts$abs.diff.TS = abs(expand.ts$diff.TS)
      expand.ts$Square.error <- (expand.ts$TS_c - sphere.ts)^2

      round(sqrt(mean(expand.ts$Square.error)),digits=6)
    }

    # Test the function...
    # test <- RMSE.calc(cut.ts, pars)

    #### Part 3. Optimize parameter inputs  #####

    # Initial optimization using specified starting values
    init.result <- optim(par=pars, RMSE.calc, dat=cut.ts)

    # Secondary optimization using the results of first optimization as starting values
    final.result <- optim(par=init.result$par, RMSE.calc, dat=cut.ts)

    # Generate calculated TS dataset using optimized parameter values
    calc.ts <- cut.ts
    calc.ts$along.minor.offs = calc.ts$Angle_minor_axis - final.result$par[5]
    calc.ts$athw.major.offs = calc.ts$Angle_major_axis - final.result$par[4]
    calc.ts$x = calc.ts$along.minor.offs/final.result$par[2]
    calc.ts$y = calc.ts$athw.major.offs/final.result$par[1]
    calc.ts$B = 24 * (calc.ts$x^2 + calc.ts$y^2)
    calc.ts$TS_c = calc.ts$TS_uncomp + calc.ts$B + final.result$par[3]
    calc.ts$diff.TS = calc.ts$TS_c - sphere.ts
    calc.ts$abs.diff.TS = abs(calc.ts$diff.TS)
    calc.ts$Square.error <- (calc.ts$TS_c - sphere.ts)^2

    calc.ts <- round(calc.ts, digits=6)

    #### Part 4. Exports ####

    # export calculated TS dataset
    write.csv(calc.ts, paste0(OUTPUT, substr(file.name, 0, nchar(file.name)-4), " - calculated TS.csv"), row.names=F)

    # Export summary info
    sink(paste0(OUTPUT, substr(file.name, 0, nchar(file.name)-4), " - Summary info.txt"))

    writeLines("Calibration coefficients Data Summary")
    date()
    writeLines("\n EV file source for sphere data")
    noquote(EV_filename_for_sphere)
    writeLines("\n Raw data files used in calibration")
    noquote(onebigdf[2:2])
    writeLines("\n Frequency")
    noquote(Frequency)
    writeLines("Transducer serial number")
    noquote(Transducer_serial_number)
    writeLines("Echosounder serial number")
    noquote(Echosounder_serial_number)
    writeLines("\n \n Optimized parameter values: \n  - major axis (athw) beam angle: ")
    final.result$par[1]
    writeLines("\n - minor axis (along) beam angle: ")
    final.result$par[2]
    writeLines("\n - gain correction: ")
    final.result$par[3]
    writeLines("\n - major (athw) offset: ")
    final.result$par[4]
    writeLines("\n - minor (along) offset: ")
    final.result$par[5]
    writeLines("\n - Sphere TS: ")
    sphere.ts
    writeLines("\n - Final Optimized Root Mean Square Error: ")
    final.result$value

    writeLines("\n \n TS summary Info: \n - Observed Compensated TS: ")
    summary(calc.ts$TS_comp)
    writeLines("\n - Calculated Compensated TS:")
    summary(calc.ts$TS_c)
    writeLines("\n - Absolute difference from theoretical sphere TS:")
    summary(calc.ts$abs.diff.TS)
    writeLines("\n - Root square Error:")
    summary(calc.ts$Square.error)

    sink()

    # Calculate 1db bins to assign color values for gradient plot

    calc.ts$ceil <- as.factor(ceiling(calc.ts$TS_c))
    median(calc.ts$TS_c)
    range(calc.ts$TS_comp)
    # Plot 1 - Beam gradient plot
    # Create color coded plot of TS across the beam to look for patterns in color that are directional.
    range(calc.ts$TS_comp)

    png(paste0(OUTPUT, substr(file.name,0,nchar(file.name)-4), " - TS gradient plot.png"), width=8, height=8, units="in",
        pointsize=20, res=300)

    shape <- rbind(c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(2,2,2,2))
    layout(shape)
    angles_plot <- ggplot() +
      geom_raster(data = calc.ts, aes(Angle_major_axis, Angle_minor_axis, colour = round(TS_c))) +
      scale_color_viridis(option="plasma")  +
      labs(colour = "Compensted TS")+
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      ggtitle("                                                  TS_c and beam position") +
      labs(caption="Plot should have little to no gradient or pattern in color across the beam in any direction. Color= TS_c") +
      geom_hline(yintercept=0, color='black')+
      geom_vline(xintercept=0, color='black')
    print(angles_plot)
    dev.off()

    # plot 2 - Observed vs. Calculated Ts_c on minor (along) axis
    # Two plots, top is observed TS_c (from echoview) bottom is calculated TS along the sounders minor axis (along ship)
    # Should be relatively flat, with no to very minor slope or curve, particularly in calculated TS (lower) plot

    png(paste0(OUTPUT, substr(file.name,0,nchar(file.name)-4), " - Observed vs Corrected plot - Along axis.png"), width=8,
        height=8, units="in",
        pointsize=20, res=300)

    shape <- rbind(c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(2,2,2,2), c(2,2,2,2), c(2,2,2,2), c(3,3,3,3))
    layout(shape)

    par(mar=c(2,2,2,2))

    plot(calc.ts$TS_comp ~ calc.ts$Angle_minor_axis, ylim = c(-36, -46), main = "Echoview TS_c vs. along angle")

    plot(calc.ts$TS_c ~ calc.ts$Angle_minor_axis, ylim = c(-36, -46), main = "R calculated TS_c vs. along angle")

    mtext("***Plots should be relatively flat, with little to no slope or curvature, \n particularly in calculated TS_c (lower) plot", side=1, line=5, font=3)

    dev.off()

    # Plot 3 - Observed vs. Calculated TS_c on major (athw) axis
    # Two plots, same as plot 2 but along sounders major axis (athwartship)
    # Should be relatively flat, with no to very minor slope or curve, particularly in calculated TS (lower) plot
    png(paste0(OUTPUT, substr(file.name,0,nchar(file.name)-4), " - Observed vs Corrected plot - Athwart axis.png"), width=8,
        height=8, units="in",
        pointsize=20, res=300)

    shape <- rbind(c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(2,2,2,2), c(2,2,2,2), c(2,2,2,2), c(3,3,3,3))
    layout(shape)

    par(mar=c(2,2,2,2))

    plot(calc.ts$TS_comp ~ calc.ts$Angle_major_axis, ylim = c(-36, -46), main = "Echoview TS_c vs. Athwart angle")

    plot(calc.ts$TS_c ~ calc.ts$Angle_major_axis, ylim = c(-36, -46), main = "R calculated TS_c vs. Athwart angle")

    mtext("***Plots should be relatively flat, with little to no slope or curvature, \n particularly in calculated TS_c (lower) plot", side=1, line=5, font=3)

    dev.off()
  }
