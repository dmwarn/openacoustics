#' Beam Pattern Estimates from Calibration Sphere Data
#'
#' Estimate 3 dB beam angles, angle offsets, and TS gain using
#' the formula employed by Simrad Lobe program. Creates graphs
#' showing sphere TS across the beam and a summary text file that
#' includes numerous pieces of information to help make the process
#' reproducible.
#'
#' @import tcltk
#' @export
#' @details
#' This function is used to prompt the user for input about the data they wish
#' to process and the source information.
#'
#'
inputs <- function(){
library(tcltk)
  xvar <- tclVar("")
  yvar <- tclVar("")
  zvar <- tclVar("")
  xxvar <- tclVar("")
  yyvar <- tclVar("")
  zzvar <- tclVar("")
  zzzvar <- tclVar("")
  yyyvar <- tclVar("")
  tt <- tktoplevel()
  tkwm.title(tt,"Input acoustic equipment and calibration info")
  x.entry <- tkentry(tt, textvariable=xvar)
  y.entry <- tkentry(tt, textvariable=yvar)
  z.entry <- tkentry(tt, textvariable=zvar)
  xx.entry <- tkentry(tt, textvariable=xxvar)
  yy.entry <- tkentry(tt, textvariable=yyvar)
  zz.entry <- tkentry(tt, textvariable=zzvar)
  zzz.entry <- tkentry(tt, textvariable=zzzvar)
  yyy.entry <- tkentry(tt, textvariable=yyyvar)
  reset <- function()
  {
    tclvalue(xvar)<-""
    tclvalue(yvar)<-""
    tclvalue(zvar)<-""
    tclvalue(xxvar)<-""
    tclvalue(yyvar)<-""
    tclvalue(zzvar)<-""
    tclvalue(yyyvar)<-""
    tclvalue(zzzvar)<-""
  }

  reset.but <- tkbutton(tt, text="Reset", command=reset)

  submit <- function() {
    x <- as.integer(tclvalue(xvar))
    y <- as.character(tclvalue(yvar))
    z <- as.character(tclvalue(zvar))
    xx <- tclvalue(xxvar)
    yy <- tclvalue(yyvar)
    zz <- tclvalue(zzvar)
    yyy <- tclvalue(yyyvar)
    zzz <- tclvalue(zzzvar)
    e <- parent.env(environment())
    e$x <- x
    e$y <- y
    e$z <- z
    e$xx <- xx
    e$yy <- yy
    e$zz <- zz
    e$yyy <- yyy
    e$zzz <- zzz
    tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)

  tkgrid(tklabel(tt,text="Enter acoustic equipment ID information"),columnspan=2)
  tkgrid(tklabel(tt,text="Frequency"), x.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Transducer serial number"), y.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Sounder serial number"), z.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Sphere TS"), xx.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Athwart angle"), yy.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Along angle"), zz.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Min. acceptable TS"), yyy.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Max. acceptable TS"), zzz.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)

  tkwait.window(tt)
  return(c(x,y,z, xx, yy, zz, yyy, zzz))
}

#myvals <-inputs()
