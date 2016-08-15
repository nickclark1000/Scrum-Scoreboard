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

shinyServer(function(input, output) {
  cat(file=stderr(),"Start session",format(Sys.time(), "%a %b %d %Y %H:%M:%S "),"\n")
  
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # source("TfsApiRequestHandler.R", local=TRUE)
  # source("TfsCollectionProjectTeam.R", local=TRUE)
  # tfs.projects <- ''
  # output$projects <- reactiveUI(function(){
  #   
  #   selectInput("projects", "TFS Projects", as.list(tfs.projects))
  # })
  # observeEvent(input$get.projects, {
  #   tfs.projects <<- GetTfsProjects(URLencode(input$collection))
  #   updateSelectInput('projects', choices = as.list(tfs.projects$value$name))
  # })
  observeEvent(input$run, {
    saveData(formData())
    
    source("Summary2.R", local=TRUE)
    
    output$BURNUP_CHART <- renderPlotly({
      source("Projections.data.R", local=TRUE)
      source("Burnup.plot.R", local=TRUE)
      plotBurnupChart(data.frame(SPRINT_INDEX = as.factor(w$SPRINT_INDEX), END_DATE = w$END_DATE, COMPLETED = w$COMPLETED_RELEASE_POINTS, TOTAL = w$TOTAL_RELEASE_POINTS), NO_CHANGE_LINE, target.release.date, AVG_5_LM)
    })
    
    output$VELOCITY_CHART <- renderPlotly({
      source("Velocity.plot.R", local=TRUE)
      plotVelocityTimeSeries(release.summary.df$SPRINT_INDEX, release.summary.df$VELOCITY, release.summary.df$VELOCITY_SMA_5)
    })
    output$vtable <- renderDataTable(release.summary.df, options=list(scrollX=TRUE))
  })
})