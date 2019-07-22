[![Build Status](https://travis-ci.org/spiwokv/metadynminer3d.svg?branch=master)](https://travis-ci.org/spiwokv/metadynminer3d) 
[![Build Status](https://ci.appveyor.com/api/projects/status/github/spiwokv/metadynminer3d?branch=master&svg=true)](https://ci.appveyor.com/project/spiwokv/metadynminer3d) 
[![CRAN status](https://www.r-pkg.org/badges/version/metadynminer3d)](https://cran.r-project.org/package=metadynminer3d) 
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/metadynminer3d)](https://cran.r-project.org/package=metadynminer3d)
[![Rdoc](http://www.rdocumentation.org/badges/version/metadynminer3d)](http://www.rdocumentation.org/packages/metadynminer3d)
[![codecov](https://codecov.io/gh/spiwokv/metadynminer3d/branch/master/graph/badge.svg)](https://codecov.io/gh/spiwokv/metadynminer3d/)

# metadynminer3d

## Introduction
metadynminer is R packages for reading, analysis and visualization of metadynamics HILLS files produced by Plumed.
It reads HILLS files from Plumed, calculates free energy surface by fast Bias Sum algorithm, finds minima and analyses
transition paths by Nudged Elastic Band method.

metadynminer3d is its addendum for plotting 3D free energy surfaces. It uses RGL package. metadynminer3d installs and
loads metadynminer automatically.

## Usage
```R
# Install from R repository
install.packages("metadynminer3d")

# Install from GitHub by devtools
install.packages("devtools")
devtools::install_github("spiwokv/metadynminer3d")

# Load library
library(metadynminer3d)
# Read hills file
hillsf<-read.hills3d("HILLS", per=c(TRUE, TRUE, TRUE)) # HILLS with periodicity on CV1, CV2 and CV3

# Sum two hills files
hillsf+hillsf

# Summary of a hills file
summary(hillsf)

# Plot CVs
plot(hillsf)

# Plot heights
plotheights(hillsf)

# Calculate FES by bias sum (alternatively use fes2 for conventional calculation)
tfes<-fes(hillsf)

# Calculate FES for given range (indexes of hills)
tfes<-fes(hillsf, imin=5000, imax=10000)

# Sum two FESes
tfes+tfes

# Calculate and subtract min, max or mean from a FES
tfes<-tfes-min(tfes)

# Summary of FES
summary(tfes)

# Plot FES
plot(tfes, level=20)

# Find minima
minima<-fesminima(tfes)

# Summary of minima
summary(minima)

# Plot free energy minima
plot(minima)

# Calculate free energy profile for minima
prof<-feprof(minima)

# Plot free energy profile for minima
plot(prof)
```

