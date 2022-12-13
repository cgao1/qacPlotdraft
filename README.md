# qacPlot

<!-- badges: start -->
<!-- badges: end -->

Using R, you can plot various plots to visualize the data you are interested in. However, there are dozens of types of plots and it is hard to memorize and decide which plot type to use.

The goal of **qacPlot** is help you **quickly identify a plot type** and **visualize the variables you are interested in**. 

## Installation

You can install the development version of qacPlot like so:

``` r
if(!require("remotes")){
install.packages("remotes")
}
remotes::install_github("cgao1/qacPlot")
```


To install the package, you can use the following code
``` r
library(qacPlot)
```

## Example
There is only one function **thePlot** in this package, because we want to keep it simple for you to use. The function can handle univariate, bivariate and multivariate graphs. Here's an example:

``` r
thePlot(mtcars, cyl, mpg)
```

For more detailed instructions, please visit [**thePlot**](https://rkabacoff.github.io/qacPlot)



