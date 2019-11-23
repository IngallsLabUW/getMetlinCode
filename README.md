# getMetlinCode
Code used to query the Metlin database for mass and MSn data

The [Metlin database](https://metlin.scripps.edu/landing_page.php?pgcontent=mainPage) is a massive repository of mass-spectrometry data, but it can be hard to access via scripts. This repository contains code and examples illustrating a couple convenient wrapper functions I've written to ask Metlin for various bits of data and get them into R nicely.

## Expected packages:

 - **httr**
 - **dplyr**
 - **xml2**

## Usage

These wrappers essentially interact with the "Advanced Search" option on the Metlin website, but do NOT require an authorization or login. There are 3 main functions currently written. The first, **getMetlinName**, queries the Metlin database for a specific compound name and returns a list of all entries matching that name. **getMetlinMz** queries the database for a given *m/z* and ppm error (defaults to 2.5) and returns all entries that have a documented mass within that window, i.e. all isomers. **getMetlinMS2** extracts the MS/MS data for a given compound, and operates on a specific compound ID assigned within the Metlin database - typically, a call to **getMetlinMz** or **getMetlinName** is performed first, both of which return the compound ID. See [the vignette](https://github.com/IngallsLabUW/getMetlinCode/blob/master/getMetlinVignette.md) for more detailed functionality and the code.

## Package

These functions have also been wrapped into a neat package with helpful documentation, generated via Roxygen.
To download and install the package, grab the "getMetlin_0.1.0.tar.gz" file from "Package things" and
install it via the below code, to be run in R:

install.packages("path/to/tar/file", source = TRUE, repos=NULL)
