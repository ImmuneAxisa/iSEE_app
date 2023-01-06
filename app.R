# check Libraries installation
if (!require(BiocManager))
  install.packages("BiocManager")
if (!require(tidyverse))
  install.packages("tidyverse"); library(tidyverse)
if (!require(iSEE))
  BiocManager::install("iSEE"); library(iSEE)
if (!require(iSEEu))
  BiocManager::install("iSEEu"); library(iSEEu)

setwd(dirname(rstudioapi::getActiveDocumentContext()[["path"]]))
# source options: these files need to be in the same directory as app.R
source("hexplots.R")
source("custom_config.R")

options(shiny.maxRequestSize = 10000 * 1024 ^ 2)


# start app
app <- iSEE(
  landingPage = lp,
  initial = list(
    ReducedDimensionPlot(),
    ReducedDimensionHexPlot(),
    RowDataTable(),
    FeatureAssayPlot(),
    ColumnDataPlot(),
    RowDataPlot(),
    SampleAssayPlot(),
    ColumnDataTable(),
    ComplexHeatmapPlot()
  ),
  colormap = ecm,
  saveState = function(x) {
    dir.create("~/.ISEE_state", showWarnings = F)
    withProgress(message = "Saving!", saveRDS(x[["memory"]], file = tempfile(tmpdir = "~/.ISEE_state", fileext =
                                                                               '.rds')))
    showNotification("hooray, saved!", type = "message")
  }
)


shiny::runApp(app)
