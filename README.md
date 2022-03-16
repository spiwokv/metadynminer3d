[![Build Status](https://github.com/spiwokv/metadynminer3d/actions/workflows/r.yml/badge.svg)](https://github.com/spiwokv/metadynminer3d/actions/) 
[![Build Status](https://ci.appveyor.com/api/projects/status/github/spiwokv/metadynminer3d?branch=master&svg=true)](https://ci.appveyor.com/project/spiwokv/metadynminer3d) 
[![CRAN status](https://www.r-pkg.org/badges/version/metadynminer3d)](https://cran.r-project.org/package=metadynminer3d) 
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/metadynminer3d)](https://cran.r-project.org/package=metadynminer3d)
[![Rdoc](http://api.rdocumentation.org/badges/version/metadynminer3d)](http://www.rdocumentation.org/packages/metadynminer3d)
[![codecov](https://codecov.io/gh/spiwokv/metadynminer3d/branch/master/graph/badge.svg)](https://codecov.io/gh/spiwokv/metadynminer3d/)

# MetadynMiner3d

## Web site
http://metadynamics.cz/metadynminer3d/

## Introduction
MetadynMiner is R packages for reading, analysis and visualization of metadynamics HILLS files produced by Plumed.
It reads HILLS files from Plumed, calculates free energy surface by fast Bias Sum algorithm, finds minima and analyses
transition paths by Nudged Elastic Band method.

MetadynMiner3d is its addendum for plotting 3D free energy surfaces. It uses RGL package. MetadynMiner3d installs and
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
```
![hills3d](./figs/hills3d.png)
```R
# Plot heights
plotheights(hillsf)
```
![hills3dh](./figs/hills3dh.png)
```R
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
```
![fes3d](./figs/fes3d.png)
```R
# Find minima
minima<-fesminima(tfes)

# Summary of minima
summary(minima)

# Plot free energy minima
plot(minima)
```
![min3d](./figs/min3d.png)
```R
# Calculate free energy profile for minima
prof<-feprof(minima)

# Plot free energy profile for minima
plot(prof)
```
![prof3d](./figs/prof3d.png)

## Tips and Tricks
### Publication quality figures
Following script can be used to generate a publication quality figure:
```R
hillsf<-read.hills3d("HILLS", per=c(TRUE, TRUE, TRUE))
tfes<-fes(hillsf)
plot(tfes)
```
Change window size, zoom in and rotate if necessary, then (without closing the window) type:
```R
rgl.snapshot(filename="plot.png")
```

### Publication of interactive FES on web
You can save free energy surface in WebGL and present it on a web site by typing:
```R
writeWebGL(filename="index.html")
```

### Making movie
It is possible to make movie of rotation of the plot by:
```R
movie3d(spin3d(axis=c(0,0,1)), duration=3)
```
This will create a three-second animated GIF file of rotation around z-axis.
It needs installation of `convert` command from ImageMagic program. Alternatively, the command:
```R
movie3d(spin3d(axis=c(0,0,1)), duration=3,
        dir=".", convert=FALSE)
```
These files can be concatenated by a movie making program such as mencoder.

### Modifying aspect ratio of box
The aspect ratio of the box can be modified by:
```R
aspect3d(2, 1, 1)
```
after `plot` command. This makes the x-axis twice longer than other axes. 

## Contact
Vojtech Spiwok - spiwokv{youknowwhat}vscht.cz

To contribute, se [CONTRIBUTING.md](./CONTRIBUTING.md)

