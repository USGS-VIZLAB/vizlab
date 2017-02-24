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

## Using this package

To setup a new project, use the following functions. `createProfile` creates a `profile.yaml` which is necessary to run make, `initializeVizRepo` creates a GitHub repo on the specified organization ("USGS-VIZLAB" by default) with common issues, `vizSkeleton` creates the necessary directories for this project, and `createMakefiles` will setup all the makefiles. `createProfile` only needs to happen once (the file should not be stored in your project directory, but in some home directory instead). `createMakefiles` only needs to happen once per project. Then you can use `make` at the command line to run the whole process.

```r
library(vizlab)

#optional, profile.yaml only needs to exist in one place on your computer
createProfile() 

# to create and initialize the GitHub repository (includes standard issues)
initializeVizRepo(repo_name="myFirstViz", description="This is the GitHub repo for my first viz.")

# to setup each project
vizSkeleton(name="my awesome viz")
createMakefiles() 
```

## Adding your own mimetypes
To specify your own mimetypes (or override defaults), create a `.yaml` to specify the mimetype and it's corresponding reader, publisher, or resource (see inst/mimetypes.default.yaml for example structure). Then add the filename to your `viz.yaml` file under info with the name `mimetypeDictionary`.

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
