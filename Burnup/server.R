cat(file=stderr(),"Start server",format(Sys.time(), "%a %b %d %Y %H:%M:%S "),"\n")
library(shiny)
responses<-NULL
saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

fields <- c("target")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  cat(file=stderr(),"Start session",format(Sys.time(), "%a %b %d %Y %H:%M:%S "),"\n")
  
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })

  observeEvent(input$run, {
    saveData(formData())
    
    inFile2 <- input$file2
    
    if (is.null(inFile2))
      return(NULL)
    
    INPUT_DATA <-read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, quote=input$quote2)
    cat(file=stderr(),"inout: ",nrow(INPUT_DATA),"\n")
    
    TARGET_RELEASE_SPRINT <- input$target
    source("Summary2.R", local=TRUE)
    
    output$BURNUP_CHART <- renderPlotly({
      source("Projections.data.R", local=TRUE)
      source("Burnup.plot.R", local=TRUE)
      cat(file=stderr(),"lmsss:",w$CURRENT_RELEASE_IDENTIFIER,"\n")
      plotBurnupChart(w$SPRINT_INDEX, w$COMPLETED_RELEASE_POINTS, w$TOTAL_RELEASE_POINTS, tail(v$TOTAL_RELEASE_POINTS,1), NO_CHANGE_LINE, TARGET_RELEASE_SPRINT, AVG_5_LM)
    })
    
    output$VELOCITY_CHART <- renderPlotly({
      source("Velocity.plot.R", local=TRUE)
      last20 <- tail(release.summary.df, 20)
      plotVelocityTimeSeries(last20$SPRINT_INDEX, last20$VELOCITY, last20$PLANNED_SPRINT_VELOCITY, last20$VELOCITY_SMA_5)
    })
    output$vtable <- renderDataTable(release.summary.df, options=list(scrollX=TRUE))
    # output$team <- renderMenu({
    #  # cat(file=stderr(),"Responses:",responses$teamname)
    #   teams<-apply(responses, 1, function(row) {
    #     cat(file=stderr(),"Responxses:",row[["teamname"]],"\n")
    #     menuItem(row[["teamname"]], tabName = row[["teamname"]], icon = icon("dashboard"))
    #   })
    #   sidebarMenu(.list=teams)
    # })
  })
})