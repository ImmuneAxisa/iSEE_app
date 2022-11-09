# Libraries
if (!require(BiocManager))
  install.packages("BiocManager")
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(iSEE))
  BiocManager::install("iSEE")
if (!require(iSEEu))
  BiocManager::install("iSEEu")


options(shiny.maxRequestSize = 10000 * 1024 ^ 2)

load_object <- function(x) {
  print(x)
  x <- x$datapath
  print(basename(x))
  
  # check that extension is right and choose import method (qs or rds, delayed array)
  if (length(x) == 2 && sum(grepl("\\.rds$", basename(x))) == 1 && sum(grepl("\\.h5$", basename(x))) == 1) {
    cat("HDF5 import")
    
    if (!require(HDF5Array)) {
      showModal(modalDialog(
        title = "HDF5Array package not installed",
        "run `BiocManager::install(\"HDF5Array\")` to install",
        easyClose = TRUE
      ))
      showNotification(
        "HDF5Array package not installed",
        duration = NULL,
        type = "error"
      )
      stop("missing required package")
    }
    
    cat("renaming HDF5 archive files")
    file.rename(grep("\\.h5$", x, value = T), file.path(dirname(x[1]), "assays.h5"))
    file.rename(grep("\\.rds$", x, value = T), file.path(dirname(x[1]), "se.rds"))
    
    print(list.files(dirname(x[1])))
    
    object <- try(HDF5Array::loadHDF5SummarizedExperiment(dirname(x[1])))
  } else if (length(x) > 1) {
    showModal(modalDialog(
      title = "Invalid file type",
      "Unless importing HDF5 saved data, only select one file",
      easyClose = TRUE
    ))
    #session$reload()
    return(stop("Invalid file type"))
  } else if (grepl("\\.rds$", basename(x))) {
    cat("rds import")
    
    object <- try(grepl("\\.h5$", basename(x)))
  } else if (grepl("\\.qs$", basename(x))) {
    cat("qs import")
    # throw an error if qs is not installed
    if (!require(qs)) {
      showModal(modalDialog(
        title = "qs package not installed",
        "run `install.packages(\"qs\")` to install",
        easyClose = TRUE
      ))
      showNotification(
        "qs package not installed",
        duration = NULL,
        type = "error"
      )
      stop("missing required package")
    }
    
    object <- try(qs::qread(x))
    
  } else {
    showModal(modalDialog(
      title = "Invalid file type",
      "file/folder is not supported (rds, qs, HDF5), convert to supported file type, refresh to restart",
      easyClose = TRUE
    ))
    #session$reload()
    return(stop("Invalid file type"))
  }
  
  # check class of imported file
  
  if (is(object, "SingleCellExperiment"))
    return(object)
  else if (is(object, "Seurat")) {
    if (!require(Seurat)) {
      
      # Throw an error if Seurat is not installed
      showModal(modalDialog(
        title = "Seurat package not installed",
        "run `install.packages(\"Seurat\")` to install",
        easyClose = TRUE
      ))
      showNotification(
        "Seurat package not installed",
        duration = NULL,
        type = "error"
      )
      stop("missing required package")
    }
    # else convert to sce and return object
    return(Seurat::as.SingleCellExperiment(object))
  } else {
    showModal(modalDialog(
      title = "Invalid object type",
      "Imported object is not of class Seurat or SingleCellExperiment. Check input file",
      easyClose = TRUE
    ))
    showNotification(
      "Invalid object type",
      duration = NULL,
      type = "error"
    )
    return(stop("Invalid object type"))
  }
    
}


lp <-
  createLandingPage(
    seUI = function(id)
      fileInput(id,
                HTML(
                  paste(
                    "path to sce file (rds, qs, HDF5).",
                    "",
                    "NB: in the case of HDF5, object saved with `HDF5Array::saveHDF5SummarizedExperiment` is expected,",
                    "where a folder is generated containing the .h5 array and the shell saved as .rds (See HDF5Array package manual for more details).",
                    "In this case, select the both .h5 and .rds files within the folder archive",
                    sep = "<br/>"
                  )
                ),
                width = '200%',
                multiple = T),
    seLoad = function(x)
      load_object(x),
    initUI = function(id)
      fileInput(id, "Initial state RDS file: (optional)",
                multiple = FALSE)
  )

iSEE(landingPage = lp)



