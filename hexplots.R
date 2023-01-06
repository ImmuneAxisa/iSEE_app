# Original code from https://github.com/iSEE/iSEEhex/blob/fd38bb2c6c2c047ab9611861296d092d7b67196c/R/ReducedDimensionHexPlot.R


# Definition ----

collated <- character(0)

.plotBinResolution <- "BinResolution"
collated[.plotBinResolution] <- "numeric"

.plotMinPercentile <- "MinPercentile"
collated[.plotMinPercentile] <- "numeric"

.plotMaxPercentile <- "MaxPercentile"
collated[.plotMaxPercentile] <- "numeric"

.plotBinShape <- "BinShape"
collated[.plotBinShape] <- "character"


#' @export
#' @importClassesFrom iSEE ReducedDimensionPlot
#' @import SummarizedExperiment
setClass("ReducedDimensionHexPlot", contains="ReducedDimensionPlot", slots=collated)

#' @export
#' @importFrom methods new
ReducedDimensionHexPlot <- function(...) {
  new("ReducedDimensionHexPlot", ...)
}

setMethod(".fullName", "ReducedDimensionHexPlot", function(x) "Hexagonal reduced dimension plot")

#' @export
#' @importMethodsFrom iSEE .panelColor
setMethod(".panelColor", "ReducedDimensionHexPlot", function(x) "#991717")

# Initialization ----

#' @export
#' @importFrom methods callNextMethod
#' @importFrom iSEE .emptyDefault
setMethod("initialize", "ReducedDimensionHexPlot", function(.Object, ...) {
  args <- list(...)
  
  args <- .emptyDefault(args, .plotBinResolution, 100)
  args <- .emptyDefault(args, .plotMinPercentile, 0)
  args <- .emptyDefault(args, .plotMaxPercentile, 100)
  args <- .emptyDefault(args, .plotBinShape, "hex")
  args[["Downsample"]] <- FALSE
  
  do.call(callNextMethod, c(list(.Object), args))
})

# Interface ----

#' @export
setMethod(".hideInterface", "ReducedDimensionHexPlot", function(x, field) {
  hidden_fields <- c("Downsample")
  if (field %in% hidden_fields)
    TRUE
  else callNextMethod()
})

#' @export
setMethod(".defineVisualShapeInterface", "ReducedDimensionHexPlot", function(x) {
  NULL
})

#' @export
#' @importFrom shiny tagList
setMethod(".defineVisualSizeInterface", "ReducedDimensionHexPlot", function(x) {
  plot_name <- .getEncodedName(x)
  
  .addSpecificTour(class(x), .plotBinResolution, function(plot_name) {
    data.frame(
      element=paste0("#", plot_name, "_", .plotBinResolution),
      intro="Here, we can change the bin size of the plot.
Larger values will result in larger bins, sacrificing resolution for more stable counts.
One should avoid setting this too low as then the plot just collapses to showing each point as a separate bin."
    )
  })
  
  tagList(
    .selectInput.iSEE(x, .plotBinShape, label = "Bin shape",
                      choices = c("hexbin" = "hex", "tile" = "2d"), selected = x[[.plotBinShape]]),
    .numericInput.iSEE(x, .plotBinResolution, label="Bin resolution:",
                       min=1, value=x[[.plotBinResolution]], step = 1)
  )
})



#' @export
setMethod(".allowableColorByDataChoices", "ReducedDimensionHexPlot", function(x, se) {
  .getCachedCommonInfo(se, "ColumnDotPlot")$continuous.colData.names
})

#' @export
#' @importFrom shiny tagList
setMethod(".getDotPlotColorHelp", "ReducedDimensionHexPlot", function(x, color_choices) {
  force(color_choices)
  function(plot_name) {
    start <- paste0("#", plot_name, "_", iSEE:::.colorByField)
    base <- "The default is to color bins by the number of points inside them (<em>None</em>). However, we can also color by various per-column attributes. Try out some of the different choices here, and note how further options become available when each choice is selected."
    steps <- list(c(element=start, intro=base))
    
    if ("Column data" %in% color_choices) {
      steps <- c(steps, list(
        c(
          element=start,
          intro="For example, if we <strong>select <em>Column data</em></strong>..."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorByColData, " + .selectize-control"),
          intro="... we can choose between different <code>colData</code> fields that we might want to color by.
For continuous variables, this will take the average value across all points in each bin."
        )
      ))
    }
    
    if ("Feature name" %in% color_choices) {
      steps <- c(steps, list(
        c(
          element=start,
          intro="If we <strong>select <em>Feature name</em></strong>..."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorByFeatName, " + .selectize-control"),
          intro="... each bin is colored according to the assay values of a feature of interest for the corresponding column.
For continuous assay values, this involves taking the average value across all points in each bin."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorByFeatNameAssay, " + .selectize-control"),
          intro="We can change the choice of assay."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorByRowTable, " + .selectize-control"),
          intro="And we can even synchronize the choice of feature to a selection in another panel. This assumes that our current application actually has another panel that allows us to select a single feature from our <code>SummarizedExperiment</code>."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorByFeatDynamic),
          intro="In fact, we don't even need to manually choose another panel - if dynamic feature selection is enabled, the plot will automatically respond to any single feature selection from any applicable panel in our application."
        )
      ))
    }
    
    if ("Sample name" %in% color_choices) {
      steps <- c(steps, list(
        c(
          element=start,
          intro="If we <strong>select <em>Sample name</em></strong>..."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorBySampName, " + .selectize-control"),
          intro="... we can highlight the location of a particular point based on the column name."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorBySampNameColor),
          intro="We can fiddle with the choice of color for the highlighted point."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorByColTable, " + .selectize-control"),
          intro="We can even synchronize the choice of sample to a selection in another panel. This assumes that our current application actually has another panel that we can use to select a single sample."
        ),
        c(
          element=paste0("#", plot_name, "_", iSEE:::.colorBySampDynamic),
          intro="In fact, we don't even need to manually choose another panel - if dynamic sample selection is enabled, the plot will automatically respond to any single sample selection from any applicable panel in our application."
        )
      ))
    }
    
    data.frame(do.call(rbind, steps))
  }
})


#' @export
setMethod(".defineVisualOtherInterface", "ReducedDimensionHexPlot", function(x) {
  plot_name <- .getEncodedName(x)
  
  tagList(
    .sliderInput.iSEE(x, .plotMinPercentile, label="Minimum percentile",
                       min=0, max = 100, value=x[[.plotMinPercentile]], step = 1),
    .sliderInput.iSEE(x, .plotMaxPercentile, label="Maximum percentile",
                       min=0, max = 100, value=x[[.plotMaxPercentile]], step = 1)
  )
})

# Observers ----

#' @export
#' @importFrom methods callNextMethod
setMethod(".createObservers", "ReducedDimensionHexPlot", function(x, se, input, session, pObjects, rObjects) {
  callNextMethod()
  
  plot_name <- .getEncodedName(x)
  
  .createProtectedParameterObservers(plot_name,
                                     fields=c(.plotBinResolution, .plotMinPercentile, .plotMaxPercentile, .plotBinShape),
                                     input=input, pObjects=pObjects, rObjects=rObjects)
  
  invisible(NULL)
})

# Plotting ----

#' @export
#' @importMethodsFrom iSEE .generateDotPlot
#' @importFrom iSEE .addFacets .buildAes .buildLabs
setMethod(".generateDotPlot", "ReducedDimensionHexPlot", function(x, labels, envir) {
  plot_data <- envir$plot.data
  is_subsetted <- exists("plot.data.all", envir=envir, inherits=FALSE)
  is_downsampled <- exists("plot.data.pre", envir=envir, inherits=FALSE)
  plot_type <- envir$plot.type
  
  args <- list(plot_data,
               param_choices=x,
               x_lab=labels$X,
               y_lab=labels$Y,
               color_lab=labels$ColorBy,
               shape_lab=labels$ShapeBy,
               size_lab=labels$SizeBy,
               title=labels$title,
               is_subsetted=is_subsetted,
               is_downsampled=is_downsampled)
  
  plot_cmds <- do.call(.reduced_dimension_hex_plot, args)
  
  # Adding a faceting command, if applicable.
  facet_cmd <- .addFacets(x)
  if (length(facet_cmd)) {
    N <- length(plot_cmds)
    plot_cmds[[N]] <- paste(plot_cmds[[N]], "+")
    plot_cmds <- c(plot_cmds, facet_cmd)
  }
  
  # Adding self-brushing boxes, if they exist.
  plot_cmds <- .addMultiSelectionPlotCommands(x,
                                              flip=(plot_type == "violin_horizontal"),
                                              envir=envir, commands=plot_cmds)
  
  list(plot=.textEval(plot_cmds, envir), commands=plot_cmds)
})

#' @importFrom hexbin hexcoords
.reduced_dimension_hex_plot <- function(plot_data, param_choices,
                                        x_lab, y_lab, color_lab, shape_lab, size_lab, title,
                                        by_row=FALSE, is_subsetted=FALSE, is_downsampled=FALSE)
{
  plot_cmds <- list()
  plot_cmds[["ggplot"]] <- "ggplot() +"
  
  color_set <- !is.null(plot_data$ColorBy)
  color_discrete <- is.factor(plot_data$ColorBy)
  
  if (color_set) {
    if (param_choices[["ColorBy"]] == "Sample name") {
      new_aes <- .buildAes()
    } else if (!color_discrete) {
      new_aes <- .buildAes(alt=c(color = "ggplot2::after_scale(fill)", z="ColorBy"))
    } else {
      # If this somehow manages to happen (e.g., discrete assays),
      # don't even try to plot it.
      color_lab <- "Count"
      new_aes <- .buildAes()
    }
  } else {
    color_lab <- "Count"
    new_aes <- .buildAes()
  }
  
  plot_cmds[["hex"]] <- .create_hex(param_choices, new_aes, color_set, is_subsetted)
  
  # Adding axes labels.
  plot_cmds[["labs"]] <- .buildLabs(x=x_lab, y=y_lab, fill=color_lab, shape=shape_lab, size=size_lab, title=title)
  
  # Adding further aesthetic elements.
  plot_cmds[["theme_base"]] <- "theme_bw() +"
  plot_cmds[["theme_custom"]] <- sprintf(
    "theme(legend.position='%s', legend.box='vertical', legend.text=element_text(size=%s), legend.title=element_text(size=%s),
        axis.text=element_text(size=%s), axis.title=element_text(size=%s), title=element_text(size=%s))",
    tolower(param_choices[["LegendPosition"]]),
    param_choices[["FontSize"]]*9,
    param_choices[["FontSize"]]*11,
    param_choices[["FontSize"]]*10,
    param_choices[["FontSize"]]*12,
    param_choices[["FontSize"]]*12)
  
  return(unlist(plot_cmds))
}

#' @importFrom ggplot2 geom_hex stat_summary_hex coord_cartesian geom_point
.create_hex <- function(param_choices, aes, color, is_subsetted) {
  plot_cmds <- list()
  
  # NOTE: the 'ggplot2::' prefixing is QUITE deliberate here, as the commands
  # are evaluated by baseline iSEE where they may not be imported.
  fallback <- sprintf(
    "ggplot2::geom_hex(%s, bins = %i, alpha=%s, plot.data) +",
    aes,
    as.integer(param_choices[[.plotBinResolution]]),
    param_choices[["PointAlpha"]]
  )
  
  color_choice <- param_choices[["ColorBy"]]
  if (color_choice %in% c("Column data", "Feature name")) {
    if(color_choice == "Column data") {
      color_scale <- sprintf("colDataColorMap(colormap, '%s')(100)",
                             param_choices[["ColorByColumnData"]])
    } else {
      color_scale <- sprintf("assayColorMap(colormap, '%s')(100)",
                             param_choices[["ColorByFeatureNameAssay"]])
    }
    
    scale_args <- sprintf("limits = quantile(plot.data$ColorBy, c(%i, %i)/100), oob = scales::oob_squish, colors = %s",
                          as.integer(param_choices[[.plotMinPercentile]]),
                          as.integer(param_choices[[.plotMaxPercentile]]),
                          color_scale)
    
    plot_cmds[["hex"]] <- sprintf('ggplot2::stat_summary_%s(%s, bins = %i, fun=%s, alpha=%s, plot.data) +',
                                  param_choices[[.plotBinShape]],
                                  aes,
                                  as.integer(param_choices[[.plotBinResolution]]),
                                  deparse("mean"),
                                  param_choices[["PointAlpha"]])
    
    plot_cmds[["fill_scale"]] <- sprintf("scale_fill_gradientn(%s) +", scale_args)
    
    plot_cmds[["color_scale"]] <- sprintf("scale_color_gradientn(%s) +", scale_args)
    
    plot_cmds[["guides"]] <- "guides(color = 'none') +"
    
  } else if (color_choice == "Sample name") {
    plot_cmds[["hex"]] <- c(fallback, sprintf(
      "geom_point(%s, data=subset(plot.data, ColorBy == 'TRUE'), color=%s, alpha=1, size=5*%s) +",
      aes, deparse(param_choices[["ColorBySampleNameColor"]]), param_choices[["PointSize"]]))
  }
  else { # color by nothing -> count of samples per bin
    plot_cmds[["hex"]] <- fallback
  }
  
  # TODO: export functionality from iSEE (copy start)
  # Defining boundaries if zoomed.
  bounds <- param_choices[["ZoomData"]]
  if (length(bounds)) {
    plot_cmds[["coord"]] <- sprintf(
      "coord_cartesian(xlim=c(%s, %s), ylim=c(%s, %s), expand=FALSE) +", # FALSE, to get a literal zoom.
      deparse(bounds["xmin"]), deparse(bounds["xmax"]),
      deparse(bounds["ymin"]),  deparse(bounds["ymax"])
    )
  } else {
    full_data <- ifelse(is_subsetted, "plot.data.all", "plot.data")
    plot_cmds[["coord"]] <- sprintf("coord_cartesian(xlim=range(%s$X, na.rm=TRUE),
    ylim=range(%s$Y, na.rm=TRUE), expand=TRUE) +", full_data, full_data)
  }
  # TODO: export functionality from iSEE (copy end)
  
  return(unlist(plot_cmds))
}

#' @export
setMethod(".definePanelTour", "ReducedDimensionHexPlot", function(x) {
  prev <- callNextMethod()
  
  prev[1,"intro"] <- sprintf("The <font color=\"%s\">Hexagonal reduced dimension plot</font> panel shows a dimensionality reduction result across samples where points are binned into hexagons. The color of each hexagon is proportional to the number of points contained within, which provides a more quantitative way of assessing density. It also allows for faster plotting as each point does not need to be rendered.<br/><br/>The bin resolution controls the size of the hexagons; this can be changed in the <em>Size</em> section of the <em>Visual parameters</em>.", .getPanelColor(x))
  
  prev
})