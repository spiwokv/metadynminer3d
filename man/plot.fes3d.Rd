% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readingandfes.R
\name{plot.fes3d}
\alias{plot.fes3d}
\title{Plot 3D free energy surface object}
\usage{
\method{plot}{fes3d}(x, xlab = NULL, ylab = NULL, zlab = NULL, 
  xlim = NULL, ylim = NULL, zlim = NULL, level = NULL,
  col = NULL, alpha = NULL, main = NULL, sub = NULL, fill = TRUE, ...)
}
\arguments{
\item{x}{fes3d object.}
  
\item{xlab}{a title for the x axis: see 'title'.}

\item{ylab}{a title for the y axis: see 'title'.}

\item{zlab}{a title for the z axis: see 'title'.}

\item{xlim}{numeric vector of length 2, giving the x coordinates range.}

\item{ylim}{numeric vector of length 2, giving the y coordinates range.}

\item{zlim}{numeric vector of length 2, giving the z coordinates range.}

\item{main}{an overall title for the plot: see 'title'.}

\item{sub}{a sub title for the plot: see 'title'.}

\item{level}{number or numeric vector of levels at which to draw 3D isosurface.}

\item{col}{color of the free energy surface. It can be a single color
or a vector with multiple colors for multiple 3D isosurfaces.}

\item{alpha}{number or numeric vector of alpha levels (transparency) of
3D isosurfaces.}

\item{fill}{a logical value indicating whether 3D isosurface is ploted as
solid surface (True) or wireframe (False).}

\item{...}{further arguments passed to or from other methods.}
}
\description{
`plot.fes3d` plots 3D free energy surface using .
}
\examples{
tfes3d<-fes(acealanme3d, imax=5000)
plot(tfes3d)
}
