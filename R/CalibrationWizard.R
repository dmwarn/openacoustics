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
#' @import tcltk raster
#' @return
#'   Several items included text file (.txt), a csv file, and graphs are saved
#'  to a folder called "Calibration OUTPUT" created by this function.
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
CalibrationWizard<-
function(UseEchoview =FALSE,
         filesetName = NULL)
{
  library(raster)
  library(akima)
  library(rasterVis)
  library(viridis)
  library(png)
  #source("R/inputs.R")
  #source("R/GetEVRawPaths.R")
  #onebigdf<-data.frame()
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
  #path2 <- choose.files(caption="Select source EV file. ")
  # Define directories based on selected input files. Create output directory as needed...
  #EV_filename_for_sphere<-path2
  #filesetName <- "Primary fileset"
  #EVAppObj <- COMCreate('EchoviewCom.EvApplication')
  #file.list <- EV_filename_for_sphere
  GetEVRawPaths()

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
  print(date())
  writeLines("\n EV file source for sphere data")
  print(noquote(unique(onebigdf$ev.filename)))
  writeLines("\n Raw data files used in calibration")
  print(noquote(onebigdf$raw.file.names))
  writeLines("\n Frequency")
  print(noquote(Frequency))
  writeLines("Transducer serial number")
  print(noquote(Transducer_serial_number))
  writeLines("Echosounder serial number")
  print(noquote(Echosounder_serial_number))
  writeLines("\n \n Optimized parameter values: \n  - major axis (athw) beam angle: ")
  print(final.result$par[1])
  writeLines("\n - minor axis (along) beam angle: ")
  print(final.result$par[2])
  writeLines("\n - gain correction: ")
  print(final.result$par[3])
  writeLines("\n - major (athw) offset: ")
  print(final.result$par[4])
  writeLines("\n - minor (along) offset: ")
  print(final.result$par[5])
  writeLines("\n - Sphere TS: ")
  print(sphere.ts)
  writeLines("\n - Final Optimized Root Mean Square Error: ")
  print(final.result$value)

  writeLines("\n \n TS summary Info: \n - Observed Compensated TS: ")
  print(summary(calc.ts$TS_comp))
  writeLines("\n - Calculated Compensated TS:")
  print(summary(calc.ts$TS_c))
  writeLines("\n - Absolute difference from theoretical sphere TS:")
  print(summary(calc.ts$abs.diff.TS))
  writeLines("\n - Root square Error:")
  print(summary(calc.ts$Square.error))

  sink()

  # Calculate 1db bins to assign color values for gradient plot

  calc.ts$ceil <- as.factor(ceiling(calc.ts$TS_c))

  calc.ts$sigmabs <- 10^(calc.ts$TS_uncomp/10)

  # Plot the uncompensated TS with a filled contour

  png(paste0(OUTPUT, substr(file.name,0,nchar(file.name)-4),
             " - Uncompensated TS gradient plot.png"),
      width = 12, height=12, units               ="in",
      pointsize=20, res=300)
  #     s1 <- data.frame(X = calc.ts$Angle_major_axis,
  #     Y = calc.ts$Angle_minor_axis,
  #     Z = calc.ts$TS_uncomp, sigmanbs = 10^(calc.ts$TS_uncomp/10))
  #     s100<- as.matrix(s1)
  #
  #     e <- extent(s100[,1:2])
  #     r <- raster(e, ncol=27, nrow=27)
  # # you need to provide a function 'fun' for when there are multiple points per cell
  #     x <- rasterize(s100[, 1:2], r, s100[,4], fun=mean)
  #     values(x) <- log10(values(x))*10
  #     zed <- values(x)[!(is.na(values(x)))]
  #     binsnum <- floor(max(zed) - min(zed))
  #     X <- xFromCol(x, 1:ncol(x))
  #     Y <- yFromRow(x, nrow(x):1)
  #     Z <- t(matrix(getValues(x), ncol = x@ncols, byrow = TRUE)[nrow(x):1,
  #     ])
  #
  #     filled.contour(x=X, y=Y, z=Z, plot.title = title(main="Uncompensated TS",
  #                     ylab = "Major angle (degrees)",
  #                    xlab ="Minor angle (degrees)"),
  #                    key.title = title(main="TS (dB)"),
  #                    nlevels=binsnum+1,
  #                    col = plasma(binsnum+1))
  x0 <- seq(-3.55,3.55,0.15)
  y0 <- seq(-3.55,3.55,0.15)
  jub <-interp(x = calc.ts$Angle_minor_axis, y=calc.ts$Angle_major_axis, z=10^(calc.ts$TS_uncomp /10),
               duplicate="mean", linear=T,  xo = x0, yo = y0)
  jub$z <- log10(jub$z)*10

  rr <-raster(
    jub$z,
    xmn=range(jub$x)[1], xmx=range(jub$x)[2],
    ymn=range(jub$y)[1], ymx=range(jub$y)[2])

  q <- levelplot(rr, par.settings=plasmaTheme(plasma(6)), xlab = "Minor axis (degrees)",
            ylab = "Major axis (degrees)", colorkey=list(space="top",
                                                         title = "TS (dB)  ",
                                                         raster=F,
                                                         interpolate=F, width =3, height =1.5),
            pretty=T, margin=T)
  print(q)
  dev.off()

  # Plot Compensated TS using levelplot
  png(paste0(OUTPUT, substr(file.name,0,nchar(file.name)-4),
             " - Compensated TS gradient plot.png"),
      width = 12, height=12, units               ="in",
      pointsize=20, res=300)
  # s1 <- data.frame(X = calc.ts$Angle_major_axis,
  #                  Y = calc.ts$Angle_minor_axis,
  #                  Z = calc.ts$TS_c, sigmanbs = 10^(calc.ts$TS_c/10))
  # s100<- as.matrix(s1)
  #
  # e <- extent(s100[,1:2])
  # r <- raster(e, ncol=54, nrow=54)
  # # you need to provide a function 'fun' for when there are multiple points per cell
  # x <- rasterize(s100[, 1:2], r, s100[,4], fun=mean)
  # values(x) <- log10(values(x))*10
  # zed <- values(x)[!(is.na(values(x)))]
  # binsnum <- round(max(zed) - min(zed))
  # X <- xFromCol(x, 1:ncol(x))
  # Y <- yFromRow(x, nrow(x):1)
  # Z <- t(matrix(getValues(x), ncol = x@ncols, byrow = TRUE)[nrow(x):1,
  # ])
  #
  # levelplot(x, par.settings=plasmaTheme(plasma(18)), xlab = "Minor axis (degrees)",
  #        ylab = "Major axis (degrees)", colorkey=list(space="top",
  #        title = "Difference (dB)",
  #        raster=F,
  #        interpolate=T, width =3, height =1.5),
  #        pretty=T, margin=T, main = "TS difference (dB)")
  jub <-interp(x = calc.ts$Angle_minor_axis, y=calc.ts$Angle_major_axis, z=10^(calc.ts$TS_c /10),
               duplicate="mean", linear=T,  xo = x0, yo = y0)
  jub$z <- log10(jub$z)*10

  rr <-raster(
    jub$z,
    xmn=range(jub$x)[1], xmx=range(jub$x)[2],
    ymn=range(jub$y)[1], ymx=range(jub$y)[2])
  q <- levelplot(rr, par.settings=plasmaTheme(plasma(6)), xlab = "Minor axis (degrees)",
            ylab = "Major axis (degrees)", colorkey=list(space="top",
                                                         title = "TS (dB)  ",
                                                         raster=F,
                                                         interpolate=F, width =3, height =1.5),
            pretty=T, margin=T)
  print(q)
  dev.off()

  # Plot comp and uncomp TS versus major angle
  png(
    paste0(
      OUTPUT,
      substr(file.name, 0, nchar(file.name) - 4),
      " - Calibrated Uncompensated vs Compensated plot - Major axis.png"
    ),
    width = 14,
    height = 12,
    units = "in",
    pointsize = 20,
    res = 300
  )
  uncomp <-
    data.frame(
      Angle_major_axis = calc.ts$Angle_major_axis,
      Angle_minor_axis =
        calc.ts$Angle_minor_axis,
      TS = calc.ts$TS_uncomp,
      vers = "Uncompensated"
    )
  comp <-
    data.frame(
      Angle_major_axis = calc.ts$Angle_major_axis,
      Angle_minor_axis =
        calc.ts$Angle_minor_axis,
      TS = calc.ts$TS_c,
      vers = "Compensated"
    )
  datlong <- rbind(uncomp, comp)
  cols <- c("Uncompensated" = "blue", "Compensated" = "magenta")
  p <- ggplot() +
    geom_point(data = datlong, aes(x = Angle_major_axis, y = TS, color =
                                     vers)) +
    scale_color_manual(values = cols) +
    theme(
      axis.line = element_line(linetype = "solid"),
      axis.title = element_text(size = 28),
      axis.text = element_text(size = 18),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      plot.title = element_text(size = 18),
      panel.background = element_rect(fill = NA)
    ) + labs(x = "Major angle (degrees)", y = "TS (dB)") +
    theme(legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)) + labs(colour = "") + theme(legend.key = element_rect(fill = NA),
                                                                              legend.background = element_rect(fill = NA))
  print(p)
  dev.off()

  # Plot comp and uncomp TS versus minor angle
  png(
    paste0(
      OUTPUT,
      substr(file.name, 0, nchar(file.name) - 4),
      " - Calibrated Uncompensated vs Compensated plot - Minor axis.png"
    ),
    width = 14,
    height = 12,
    units = "in",
    pointsize = 20,
    res = 300
  )

  p <- ggplot() +
    geom_point(data = datlong, aes(x = Angle_minor_axis, y = TS, color =
                                     vers)) +
    scale_color_manual(values = cols) +
    theme(
      axis.line = element_line(linetype = "solid"),
      axis.title = element_text(size = 28),
      axis.text = element_text(size = 18),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      plot.title = element_text(size = 18),
      panel.background = element_rect(fill = NA)
    ) + labs(x = "Minor angle (degrees)", y = "TS (dB)") +
    theme(legend.text = element_text(size = 18),
          legend.title = element_text(size = 18)) + labs(colour = "") + theme(legend.key = element_rect(fill = NA),
                                                                              legend.background = element_rect(fill = NA))
  print(p)
  dev.off()

}
