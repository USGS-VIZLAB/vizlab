# vizlab
Package with utilities for building vizlab pages
[![Build Status](https://travis-ci.org/USGS-VIZLAB/vizlab.svg)](https://travis-ci.org/USGS-VIZLAB/vizlab)
[![Coverage Status](https://coveralls.io/repos/github/USGS-VIZLAB/vizlab/badge.svg?branch=master)](https://coveralls.io/github/USGS-VIZLAB/vizlab?branch=master)

## Installation

The most stable+current version of this package can be installed with this R command:
```r
install.packages("vizlab", dependencies=TRUE, 
  repos=c("http://owi.usgs.gov/R","https://cran.rstudio.com"))
```
and updated with this command:
```r
update.packages(oldPkgs=c("vizlab"),
  dependencies=TRUE, repos=c("http://owi.usgs.gov/R", "https://cran.rstudio.com"))
```

The most cutting edge version of the package can be installed if you have the `devtools` package:
```r
devtools::install_github("USGS-VIZLAB/vizlab")
```

Some packages are only suggested and will need to be installed manually to open up that functionality.  One example is sbtools which can be used if project data is stored on sciencebase.

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
