## RXKCD

__Authors:__ Paolo Sonego, Mikko Korpela<br/>
__License:__ [GPL-2.0](https://opensource.org/licenses/GPL-2.0)<br/>
__Status:__ Stable

[![Travis-CI Build
Status](https://travis-ci.com/onertipaday/RXKCD.svg?branch=master)](https://travis-ci.com/onertipaday/RXKCD)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/RXKCD)](https://github.com/metacran/cranlogs.app)
[![rstudio mirror downloads grand-total](http://cranlogs.r-pkg.org/badges/grand-total/RXKCD)](https://github.com/metacran/cranlogs.app)

### Description

Wrapper for accessing XKCD comics from R

### Installation

The CRAN version can be retrieved with:

    install.packages("RXKCD")
    
The latest version can be obtained via:

    devtools::install_github("onertipaday/RXKCD")

### Usage

```{r}
library(RXKCD)
searchXKCD("significant")
getXKCD(882)
```


