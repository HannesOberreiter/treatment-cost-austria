# Austria Beekeeping Treatment Expenses Analysis

Statistical analysis of the treatment expenses against **Varroa Desctructor** with the survey data set of COLOSS winter losses in Austria for the winters 2018/19 and 2019/20.

## Build Book

`bookdown::render_book("index.Rmd", "pdf_document2")`

## Version Control

Version control is done with the package renv, the `renv.lock` can be found in the root directory. One important exception is the `sf` package, which is loaded directly from base path on mac, as it did not work from the cached location. You can find this workaround in `partials/setup`, as this would only work in mac.

## Data Availability

Because of privacy concerns the data is not public available but can be requests if good reason is given.

## LaTeX Template

This thesis was typeset with LaTeX. It uses the *Clean Thesis* style developed by Ricardo Langner. The design of the *Clean Thesis* style is inspired by user guide documents from Apple Inc.

## Shape Files

Shapefiles 2020: We are using a modified version from data.gv.at, CC BY 4.0, map date 01.10.2019