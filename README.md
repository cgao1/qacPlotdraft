
# qacPlot

<!-- badges: start -->
<!-- badges: end -->

The goal of qacPlot is to do all the plotting work for you

## Installation

You can install the development version of qacPlot like so:

``` r
if(!require("remotes")){
install.packages("remotes")
}
remotes::install_github("cgao1/qacPlot")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(qacPlot)
library(ggplot2)
library(visreg)
thePlot(mtcars,mpg) #Q
thePlot(mtcars,cyl) #C
thePlot(mtcars,cyl,am) #CC color
thePlot(mtcars,am,mpg)#CQ color density
thePlot(mtcars,mpg,cyl) #logistic
thePlot(mtcars,mpg,wt)#QQ
thePlot(mtcars,vs,am,cyl)#CCC color
thePlot(mtcars,am,mpg,vs)#CQC color density
thePlot(mtcars,mpg,am,vs)#log
thePlot(mtcars,mpg,wt,am)#QQC
```

