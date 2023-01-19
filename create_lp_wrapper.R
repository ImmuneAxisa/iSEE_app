# Derived from iSEE::createLandingPage
library(iSEE)
library(shiny)
library(htmltools)

create_lp_wrapper <-
  function (seUI = NULL,
            seLoad = NULL,
            initUI = NULL,
            initLoad = NULL,
            requireButton = TRUE) {
    if (is.null(seUI)) {
      seUI <-
        function(id)
          fileInput(id, "SummarizedExperiment RDS file:",
                    multiple = FALSE)
    }
    if (is.null(seLoad)) {
      seLoad <- function(x) {
        print(x)
        readRDS(x$datapath)
      }
    }
    if (is.null(initUI)) {
      initUI <- function(id)
        fileInput(id, "Initial state RDS file:",
                  multiple = FALSE)
    }
    if (is.null(initLoad)) {
      initLoad <- function(x) {
        print(x)
        readRDS(x$datapath)
      }
    }
    force(requireButton)
    function(FUN, input, output, session) {
      .initializeLaunch <- "launch"
      .initializeSE <- "file"
      .initializeInitial <- "state"
      .color_scale_choice <- "color"
      color_scales_plot <- 10 %>% 
        data.frame(x = seq(1:.), 
                   y = "viridis",
                   col = viridis::viridis(.)) %>% 
        ggplot(aes(x, y, fill = I(col))) + 
        geom_tile() + 
        theme_void() +
        theme(axis.text.y = element_text(size = 25))
      
      input_tl <- tagList(
        seUI(.initializeSE),
        initUI(.initializeInitial),
        selectInput(
          .color_scale_choice,
          "color scale",
          c("viridis", "cividis")
        ),
        if (requireButton)
          actionButton(.initializeLaunch, label = "Launch")
      )
      
      color_scale_tl <- tagList(renderPlot(color_scales_plot))
      
      # make 2 column display (from https://www.r-bloggers.com/2019/12/quickly-create-mostly-responsive-html-columns-with-htmltools/)
      merged_tl <- tags$div(tagList(input_tl, color_scale_tl),
                            style = sprintf("column-count:%d", as.integer(2)))
      
      
      output$allPanels <- renderUI(tagList(merged_tl))
      
target <- if (requireButton)
  .initializeLaunch
else .initializeSE
observeEvent(input[[target]], {
  
  print("color scale choice")
  print(input[[.color_scale_choice]])
  
  if(input[[.color_scale_choice]] == "viridis") {
    
    colormap <- ExperimentColorMap(
      all_continuous=list(
        assays=viridis::viridis,
        colData=viridis::viridis
      ),
      global_continuous=viridis::viridis
    )
    
  } else {
    
    colormap <- ExperimentColorMap(
      all_continuous=list(
        assays=viridis::cividis,
        colData=viridis::cividis
      ),
      global_continuous=viridis::cividis
    )
    
  }
  
  print(colormap)
  
  se2 <- try(seLoad(input[[.initializeSE]]))
  if (is(se2, "try-error")) {
    showNotification("invalid SummarizedExperiment supplied",
                     type = "error")
  }
  else {
    init <- try(initLoad(input[[.initializeInitial]]))
    if (is(init, "try-error")) {
      showNotification("invalid initial state supplied",
                       type = "warning")
      init <- NULL
    }
    assign("colormap", colormap, envir =  environment(FUN))
    FUN(SE = se2, INITIAL = init)
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)
invisible(NULL)
    }
  }




#iSEE(landingPage = lpfun())

