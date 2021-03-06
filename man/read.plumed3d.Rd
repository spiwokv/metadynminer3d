% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readingandfes.R
\name{read.plumed3d}
\alias{read.plumed3d}
\title{Read 3D free energy surface from PLUMED sum_hills}
\usage{
read.plumed3d(file = "fes.dat", per = c(FALSE, FALSE, FALSE))
}
\arguments{
\item{file}{input file from PLUMED sum_hills.}

\item{per}{logical vector specifying periodicity of collective variables.}
}
\value{
fes3d object.
}
\description{
`read.plumed3d` reads 3D free energy surface from PLUMED sum_hills.
The grid in the inputfile must contain the same number of points
for CV1, CV2 and CV3. It does not use the header of the file.
Periodicity must be specified.
}
\examples{
l1<-" -3.14 -3.14 -3.14 -61.13 -47.43  19.00   2.04"
l2<-" -1.05 -3.14 -3.14 -70.72  25.95  25.78   2.43"
l3<-"  1.05 -3.14 -3.14 -65.58   8.34   2.82  -3.09"
l4<-" -3.14 -1.05 -3.14 -51.31 -43.88 -19.91   1.51"
l5<-" -1.05 -1.05 -3.14 -66.43   7.67 -22.45  -0.39"
l6<-"  1.05 -1.05 -3.14 -61.08  -7.50  -7.36  -0.83"
l7<-" -3.14  1.05 -3.14 -53.07 -55.12   0.19  -0.28"
l8<-" -1.05  1.05 -3.14 -62.81  36.19   1.65   0.45"
l9<-"  1.05  1.05 -3.14 -65.28  22.84  11.47   0.59"
l10<-" -3.14 -3.14 -1.05 -13.03 -32.17   8.24 -35.25"
l11<-" -1.05 -3.14 -1.05 -21.88  17.89  21.91 -51.20"
l12<-"  1.05 -3.14 -1.05 -14.49   3.60   6.04 -44.05"
l13<-" -3.14 -1.05 -1.05  -2.26  -7.00  -7.01 -10.65"
l14<-" -1.05 -1.05 -1.05  -8.21   3.69 -22.89 -28.48"
l15<-"  1.05 -1.05 -1.05  -1.10   0.52   3.59  -1.99"
l16<-" -3.14  1.05 -1.05  -3.75 -11.70  -5.65 -15.36"
l17<-" -1.05  1.05 -1.05  -1.15   5.75   1.05  -2.42"
l18<-"  1.05  1.05 -1.05 -10.67   8.23 -10.42 -36.77"
l19<-" -3.14 -3.14  1.05  -4.64 -13.79  10.51  14.96"
l20<-" -1.05 -3.14  1.05  -7.80  12.24  20.59  23.03"
l21<-"  1.05 -3.14  1.05  -5.32   3.46   3.17  21.99"
l22<-" -3.14 -1.05  1.05  -2.06  -6.59   0.17  10.04"
l23<-" -1.05 -1.05  1.05  -9.69   8.43  -0.97  36.97"
l24<-"  1.05 -1.05  1.05  -0.19  -0.44  -0.26   0.91"
l25<-" -3.14  1.05  1.05  -7.98 -23.02   3.97  26.98"
l26<-" -1.05  1.05  1.05  -4.64  13.66  -9.74  10.15"
l27<-"  1.05  1.05  1.05 -13.42  15.78  16.36  41.60"
twentysevenpoints<-c(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,
                     l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,
                     l21,l22,l23,l24,l25,l26,l27)
tf <- tempfile()
writeLines(twentysevenpoints, tf)
read.plumed3d(tf, per=c(TRUE,TRUE,TRUE))
}
