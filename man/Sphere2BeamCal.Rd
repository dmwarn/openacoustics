% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sphere2BeamCal.R
\name{Sphere2BeamCal}
\alias{Sphere2BeamCal}
\title{Beam Pattern Estimates from Calibration Sphere Data}
\usage{
Sphere2BeamCal(UseEchoview = FALSE, filesetName = NULL)
}
\arguments{
\item{UseEchoview}{Logical scalar indicating if the user wishes to use Echoview
to automatically determine the fileset that includes the sphere
data variable used to export the single targets. Default is FALSE,
and if the default is used, the user must supply a fileset name.}

\item{filesetName}{A character scalar giving the name of the Echoview fileset, which can
be identified automatically by setting the UseEchoview parameter to TRUE.
\code{\link{GetEVRawPaths}}.}
}
\value{
Several items included text file (.txt), a csv file, and graphs are saved
 to a folder valled "Calibration OUTPUT" created by this function.

  Seven different data frames are saved as objects in an Rdata
  file and are written to csv files in \code{maindir}:
  \itemize{
    \item \code{calculated TS.csv} = the original spehre data input along
    with the sphere TS calculated with the estimated beam pattern.
    \item \code{Observed vs Corrected plot - Along axis.png} = a plot with
    original (from Echoview, default beam settings) and corrected sphere TS
    versus along-axis beam angle.
    \item \code{Observed vs Corrected plot - Athwart axis.png} = a plot with
    original (from Echoview, default beam settings) and corrected sphere TS
    versus Athwart-axis beam angle.
    \item \code{Summary info.txt} = a text file containing date, equipment
    identifiers and default parameters, the EV file name, the raw data files
    used, and the output of the beam pattern fitting process.
    \item \code{TS gradient plot.png} = bullseye plot of sphere targets
    in the acoustic beam with coloring according to the TS.

  }
  The files all have a prefix = the name of the sphere input data
  file.
}
\description{
Estimate 3 dB beam angles, angle offsets, and TS gain using
the formula employed by Simrad Lobe program. Creates graphs
showing sphere TS across the beam and a summary text file that
includes numerous pieces of information to help make the process
reproducible.
}
\details{
This software is intended to aid those attempting to estimate transducer beam
patterns using tungsten carbide calibration sphere single target data. The function
uses the optimization routine optim() to minimize RMSE of a modified Bessel function
(Ona 1990, ICES, B:30 Sess. R.). Some users employ a two-dimensional polynomial.


1. Adjust theoretical sphere TS above based on calcaulated TS by entering water temp, sphere size, frequency, etc (see. southwest fisheries science center website...).
here https://swfscdata.nmfs.noaa.gov/AST/SphereTS/

2. Select all, press 'Run' or hit ctrl-enter on the keyboard to run script.

3. Will be prompted to select an INPUT file. This should be exported single target TS data in .csv format. Only one file can be selected at a time. Select 'open'.

4. Check Calibration output folder and view plots.

5. Repeat as necesssary for all frequencies.
}
