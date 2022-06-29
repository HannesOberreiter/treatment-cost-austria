# Austria Beekeeping Treatment Expenses Analysis

Statistical analysis of the treatment expenses against **Varroa destructor** with the survey data set of COLOSS winter losses in Austria for the winters 2018/19, 2019/20 and 2020/21.

## Build Book

`bookdown::render_book("index.Rmd", "pdf_document2")`

## Folder Structure

- data: folder which would contain the data, see data availability section
- latex: LaTeX template style `.sty` and pages which are written purely in LaTeX, e.g. titlepages, preface
- output: helper folder for plots created from R
- playground: junk stuff and testing of various methods
- src: main folder for R code
- root: RMarkdown files of the thesis, rely on src files

## Data Availability

Because of privacy concerns the data is not public available but can be requests if good reason is given.

## Template

This thesis was written in bookdown, a open-source (GPL-3) R package to facilitate writing books and long-form articles/reports with R Markdown <https://github.com/rstudio/bookdown>.

Typeset was archived with LaTeX. It uses the *Clean Thesis* style developed by Ricardo Langner. The design of the *Clean Thesis* style is inspired by user guide documents from Apple Inc. The style and template was adapted by Hannes Oberreiter for his Master thesis.

## Shape Files

Shapefiles 2020: We are using a modified version from data.gv.at, CC BY 4.0, map date 01.10.2019
