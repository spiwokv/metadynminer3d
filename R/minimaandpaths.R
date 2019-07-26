#' Find free energy minima in the fes3d object
#'
#' `fesminima.fes3d` finds free energy minima on 3D free energy surface.
#' The surface is divided by a 3D grid and minima are found for each
#' bin. Next the program determines whether the minimum of a bin is a local
#' minimum of the whole free energy surface. Free energy minima are labeled
#' constitutively by capital letters.
#'
#' @param inputfes fes3d object.
#' @param nbins number of bins for each CV (default 8).
#' @return minima object.
#'
#' @export
#' @examples
#' tfes<-fes(acealanme3d, imax=5000)
#' minima<-fesminima(tfes)
#' minima
fesminima.fes3d<-function(inputfes, nbins=8) {
  fes<-inputfes$fes
  rows<-inputfes$rows
  rb <- rows/nbins
  if(rb<2) {
    stop("Error: nbins too high, try to reduce it")
  }
  if(rows%%nbins>0) {
    stop("Error: number of rows in FES must be integer multiple of nbins")
  }
  per<-inputfes$per
  minx<-c()
  miny<-c()
  minz<-c()
  for(i in 0:(nbins-1)) {
    ni<-i*rb+0:(rb+1)
    if(per[1]) {
      ni[ni==0]<-rows
      ni[ni==(rows+1)]<-1
    } else {
      ni<-ni[ni!=0]
      ni<-ni[ni!=(rows+1)]
    }
    for(j in 0:(nbins-1)) {
      nj<-j*rb+0:(rb+1)
      if(per[2]) {
        nj[nj==0]<-rows
        nj[nj==(rows+1)]<-1
      } else {
        nj<-nj[nj!=0]
        nj<-nj[nj!=(rows+1)]
      }
      for(k in 0:(nbins-1)) {
        nk<-k*rb+0:(rb+1)
        if(per[3]) {
          nk[nk==0]<-rows
          nk[nk==(rows+1)]<-1
        } else {
          nk<-nk[nk!=0]
          nk<-nk[nk!=(rows+1)]
        }
        binmin<-which(fes[ni,nj,nk]==min(fes[ni,nj,nk]), arr.ind = TRUE)
        if(binmin[1]!=1 && binmin[2]!=1 && binmin[3]!=1 && binmin[1]!=length(ni) && binmin[2]!=length(nj) && binmin[3]!=length(nk)) {
          minx<-c(minx,i*rb+binmin[1]-1)
          miny<-c(miny,j*rb+binmin[2]-1)
          minz<-c(minz,k*rb+binmin[3]-1)
        }
      }
    }
  }
  myLETTERS <- c(LETTERS, paste("A", LETTERS, sep=""), paste("B", LETTERS, sep=""))[1:length(minx)]
  minima<-data.frame(myLETTERS, minx, miny, minz, inputfes$x[minx], inputfes$y[miny], inputfes$z[minz], fes[cbind(minx,miny,minz)])
  names(minima) <- c("letter", "CV1bin", "CV2bin", "CV3bin", "CV1", "CV2", "CV3", "free_energy")
  minima <- minima[order(minima[,8]),]
  rownames(minima) <- seq(length=nrow(minima))
  minima[,1]<-myLETTERS
  minima<-list(minima=minima, hills=inputfes$hills, fes=fes, rows=rows, dimension=inputfes$dimension, per=per,
               x=inputfes$x, y=inputfes$y, z=inputfes$z, pcv1=inputfes$pcv1, pcv2=inputfes$pcv2, pcv3=inputfes$pcv3)
  class(minima) <- "minima3d"
  return(minima)
}

#' Creates one ad hoc 3D free energy minimum for a fes object
#'
#' `oneminimum.fes3d` creates an ad hoc 3D free energy minimum on free energy surface.
#' This can be used to calculate 3D free energy surface evolution at arbitrary
#' point of free energy surface.
#'
#' @param inputfes fes3d object.
#' @param cv1 the value of collective variable 1.
#' @param cv2 the value of collective variable 2.
#' @param cv3 the value of collective variable 3.
#' @return minima object.
#'
#' @export
#' @examples
#' tfes<-fes(acealanme3d)
#' minima<-fesminima(tfes)
#' minima<-minima+oneminimum(tfes, cv1=0, cv2=0, cv3=0)
#' minima
oneminimum.fes3d<-function(inputfes, cv1, cv2, cv3) {
  fes<-inputfes$fes
  rows<-inputfes$rows
  per<-inputfes$per
  icv1<-as.integer(rows*(cv1-min(inputfes$x))/(max(inputfes$x)-min(inputfes$x)))+1
  if(icv1<0)    stop("Error: Out of range")
  if(icv1>rows) stop("Error: Out of range")
  icv2<-as.integer(rows*(cv2-min(inputfes$y))/(max(inputfes$y)-min(inputfes$y)))+1
  if(icv2<0)    stop("Error: Out of range")
  if(icv2>rows) stop("Error: Out of range")
  icv3<-as.integer(rows*(cv3-min(inputfes$z))/(max(inputfes$z)-min(inputfes$z)))+1
  if(icv2<0)    stop("Error: Out of range")
  if(icv2>rows) stop("Error: Out of range")
  minima<-data.frame(c("A"), c(icv1), c(icv2), c(icv3), c(cv1), c(cv2), c(cv3), c(fes[icv1,icv2,icv3]))
  names(minima) <- c("letter", "CV1bin", "CV2bin", "CV3bin", "CV1", "CV2", "CV3", "free_energy")
  minima<-list(minima=minima, hills=inputfes$hills, fes=fes, rows=rows, dimension=inputfes$dimension, per=per,
               x=inputfes$x, y=inputfes$y, z=inputfes$z, pcv1=inputfes$pcv1, pcv2=inputfes$pcv2, pcv3=inputfes$pcv3)
  class(minima) <- "minima3d"
  return(minima)
}

#' @export
`+.minima3d`<-function(min1, min2) {
  if(class(min1)!="minima3d") {
    stop("Error: You can sum only two minima objects")
  }
  if(class(min2)!="minima3d") {
    stop("Error: You can sum only two minima objects")
  }
  if(sum(min1$fes)!=sum(min2$fes)) {
    stop("Error: You can sum only minima objects with same FESes")
  }
  myLETTERS <- c(LETTERS, paste("A", LETTERS, sep=""), paste("B", LETTERS, sep=""))[1:(nrow(min1$minima)+nrow(min2$minima))]
  minima1<-min1$minima
  minima2<-min2$minima
  minima<-rbind(minima1, minima2)
  names(minima) <- c("letter", "CV1bin", "CV2bin", "CV3bin", "CV1", "CV2", "CV3", "free_energy")
  minima <- minima[order(minima[,8]),]
  rownames(minima) <- seq(length=nrow(minima))
  minima[,1]<-myLETTERS
  minima<-list(minima=minima, hills=min1$hills, fes=min1$fes, rows=min1$rows, dimension=min1$dimension, per=min1$per,
               x=min1$x, y=min1$y, z=min1$z, pcv1=min1$pcv1, pcv2=min1$pcv2, pcv3=min1$pcv3)
  class(minima) <- "minima3d"
  return(minima)
}

#' Print minima3d object
#'
#' `print.minima3d` prints 3D free energy minima (identifier, values of bins and collective variables and free energy).
#'
#' @param x minima object.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @examples
#' tfes<-fes(acealanme3d, imax=5000)
#' minima<-fesminima(tfes)
#' minima
print.minima3d<-function(x,...) {
  print(x$minima)
}

#' Print minima3d object summary
#'
#' `summary.minima3d` prints summary for 3D free energy minima (identifier, values of bins and collective variables,
#' free energy and equilibrium populations).
#'
#' @param object minima3d object
#' @param temp temperature in Kelvins
#' @param eunit energy units (kJ/mol or kcal/mol, kJ/mol is default)
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @examples
#' tfes<-fes(acealanme3d, imax=5000)
#' minima<-fesminima(tfes)
#' summary(minima)
summary.minima3d<-function(object, temp=300, eunit="kJ/mol",...) {
  minims<-object
  toprint <- minims$minima
  tind = 8
  if(eunit=="kJ/mol") {
    toprint<-cbind(toprint, exp(-1000*toprint[,tind]/8.314/temp))
  }
  if(eunit=="kcal/mol") {
    toprint<-cbind(toprint, exp(-1000*toprint[,tind]/8.314/temp/4.184))
  }
  sumpop<-sum(toprint[,tind+1])
  toprint<-cbind(toprint, 100*toprint[,tind+1]/sumpop)
  names(toprint)[tind+1]<-"relative_pop"
  names(toprint)[tind+2]<-"pop"
  return(toprint)
}

#' Plot minima3d object
#'
#' `plot.minima3d` plots 3D free energy surface with minima. The free energy surface is plotted the same
#' way as by plot.fes3d with additional minima labels.
#'
#' @param x minima3d object.
#' @param xlab a title for the x axis: see 'title'.
#' @param ylab a title for the y axis: see 'title'.
#' @param zlab a title for the z axis: see 'title'.
#' @param main an overall title for the plot: see 'title'.
#' @param sub a sub title for the plot: see 'title'.
#' @param col color of the free energy surface. It can be a single color
#'        or a vector with multiple colors for multiple 3D isosurfaces.
#' @param alpha number or numeric vector of alpha levels (transparency) of
#'        3D isosurfaces.
#' @param level number or numeric vector of levels at which to draw 3D isosurface.
#' @param fill a logical value indicating whether 3D isosurface is ploted as
#'        solid surface (True) or wireframe (False).
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @examples
#' tfes<-fes(acealanme3d, imax=5000)
#' minima<-fesminima(tfes)
#' plot(minima)
plot.minima3d <- function(x, xlab="CV1", ylab="CV2", zlab="CV3",
                    level=NULL, col=NULL, alpha=NULL,
                    main=NULL, sub=NULL,
                    fill=TRUE,...) {
  minims <- x
  fes<-minims$fes
  rows<-minims$rows
  minlabs<-minims$minima[,1]
  minpoints<-minims$minima[,5:7]
  x<-minims$x
  y<-minims$y
  z<-minims$z
  if(is.null(xlab)) xlab="CV1"
  if(is.null(ylab)) ylab="CV2"
  if(is.null(zlab)) zlab="CV3"
  if(is.null(level)) level=(max(fes)+min(fes))/2
  if(length(level)>1) {
    if(is.null(col)) col<-rainbow(1.35*length(level))[length(level):1]
    if(is.null(alpha)) {
      alpha<-length(level):1/length(level)
      level<-sort(level)
    }
  } else {
    if(is.null(col)) col<-"orange"
    if(is.null(alpha)) alpha<-1
  }
  contour3d(f=fes, level=level, x=x, y=y, z=z, 
            color=col, alpha=alpha, fill=fill)
  text3d(x=minpoints[,1], y=minpoints[,2], z=minpoints[,3], texts=minlabs)
  axes3d()
  title3d(xlab=xlab, ylab=ylab, zlab=zlab,
          main=main, sub=sub)
  box3d()
}

#' Calculate free energy profile for minima3d object
#'
#' `feprof.minima3d` calculates free energy profiles for free energy minima.
#' It finds the global minimum at the `imax` and calculates the evolution of
#' free energies of a local vs. the global free energy minimum. The free
#' energy of the global minimum is constant (zero).
#'
#' @param minims minima3d object.
#' @param imax index of a hill from which summation stops (default the rest of hills).
#'
#' @export
#' @examples
#' tfes<-fes(acealanme3d, imax=5000)
#' minima<-fesminima(tfes)
#' prof<-feprof(minima)
#' prof
feprof.minima3d <- function(minims, imax=NULL) {
  fes<-minims$fes
  rows<-minims$rows
  mins<-minims$minima
  hills<-minims$hills
  if(is.null(imax)) {
    imax<-nrow(hills)
  }
  if(imax>nrow(hills)) {
    imax<-nrow(hills)
    warning("Warning: You requested more hills by imax than available, using all hills\n")
  }
  tt <- 1:imax
  mms <- data.frame(tt)
  for(i in 1:nrow(mins)) {
    if(minims$per[1]==T && minims$per[2]==T && minims$per[3]==T) {
      mm<-fe3dp123(hills[,2], hills[,3], hills[,4], hills[,5], hills[,6], hills[,7], hills[,8], mins[i,5], mins[i,6], mins[i,7],
                   minims$pcv1[2]-minims$pcv1[1], minims$pcv2[2]-minims$pcv2[1], minims$pcv3[2]-minims$pcv3[1], 0, imax-1)
    }
    if(minims$per[1]==T && minims$per[2]==T && minims$per[3]==F) {
      mm<-fe3dp12(hills[,2], hills[,3], hills[,4], hills[,5], hills[,6], hills[,7], hills[,8], mins[i,5], mins[i,6], mins[i,7],
                  minims$pcv1[2]-minims$pcv1[1], minims$pcv2[2]-minims$pcv2[1], 0, imax-1)
    }
    if(minims$per[1]==T && minims$per[2]==F && minims$per[3]==T) {
      mm<-fe3dp13(hills[,2], hills[,3], hills[,4], hills[,5], hills[,6], hills[,7], hills[,8], mins[i,5], mins[i,6], mins[i,7],
                  minims$pcv1[2]-minims$pcv1[1], minims$pcv3[2]-minims$pcv3[1], 0, imax-1)
    }
    if(minims$per[1]==F && minims$per[2]==T && minims$per[3]==T) {
      mm<-fe3dp23(hills[,2], hills[,3], hills[,4], hills[,5], hills[,6], hills[,7], hills[,8], mins[i,5], mins[i,6], mins[i,7],
                  minims$pcv2[2]-minims$pcv2[1], minims$pcv3[2]-minims$pcv3[1], 0, imax-1)
    }
    if(minims$per[1]==T && minims$per[2]==F && minims$per[3]==F) {
      mm<-fe3dp1(hills[,2], hills[,3], hills[,4], hills[,5], hills[,6], hills[,7], hills[,8], mins[i,5], mins[i,6], mins[i,7],
                 minims$pcv1[2]-minims$pcv1[1], 0, imax-1)
    }
    if(minims$per[1]==F && minims$per[2]==T && minims$per[3]==F) {
      mm<-fe3dp2(hills[,2], hills[,3], hills[,4], hills[,5], hills[,6], hills[,7], hills[,8], mins[i,5], mins[i,6], mins[i,7],
                 minims$pcv2[2]-minims$pcv2[1], 0, imax-1)
    }
    if(minims$per[1]==F && minims$per[2]==F && minims$per[3]==T) {
      mm<-fe3dp3(hills[,2], hills[,3], hills[,4], hills[,5], hills[,6], hills[,7], hills[,8], mins[i,5], mins[i,6], mins[i,7],
                 minims$pcv3[2]-minims$pcv3[1], 0, imax-1)
    }
    if(minims$per[1]==F && minims$per[2]==F && minims$per[3]==F) {
      mm<-fe3d(hills[,2], hills[,3], hills[,4], hills[,5], hills[,6], hills[,7], hills[,8], mins[i,5], mins[i,6], mins[i,7], 0, imax-1)
    }
    mms<-cbind(mms,mm)
  }
  profs<-list(mms=mms, mins=mins, fes=fes, rows=rows, dimension=minims$dimension, per=minims$per, pcv1=minims$pcv1, pcv2=minims$pcv2, pcv3=minims$pcv3)
  class(profs) <- "profiles"
  return(profs)
}

