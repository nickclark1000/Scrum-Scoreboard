cat(file=stderr(),"Start server",format(Sys.time(), "%a %b %d %Y %H:%M:%S "),"\n")
library(shiny)
library(rtfs)
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
  
  output$projects <- renderUI({
    tfs_projects <- rtfs::get_projects(input$collection)
    selectInput("project", "TFS Project", as.list(tfs_projects$content$value$name))
  })
  output$teams <- renderUI({
    tfs_teams <- rtfs::get_teams(input$collection, input$project)
    selectInput("team", "TFS Team", as.list(tfs_teams$content$value$name))
  })
  observe({
    tfs_collection <<- input$collection
    tfs_project <<- input$project
    tfs_team <<- input$team
    cat("team",tfs_team," \n")
    cat("collection",tfs_collection," \n")
    cat("project",tfs_project," \n")
  })

  observeEvent(input$run, {
    saveData(formData())
    withProgress(message = 'Making plot', value = 0.5, {
      source("release_summary.R", local=TRUE)
    })
    
    
    output$burnup_chart <- renderPlotly({
      source("burnup_projections.R", local=TRUE)
      source("burnup_plot.R", local=TRUE)
      plotBurnupChart(data.frame(SPRINT_INDEX = as.factor(w$SPRINT_INDEX), END_DATE = w$END_DATE, COMPLETED = w$COMPLETED_RELEASE_POINTS, TOTAL = w$TOTAL_RELEASE_POINTS), NO_CHANGE_LINE, target_release_date, AVG_5_LM)
    })
    
    output$velocity_chart <- renderPlotly({
      source("velocity_plot.R", local=TRUE)
      plotVelocityTimeSeries(release_summary$SPRINT_INDEX, release_summary$VELOCITY, release_summary$VELOCITY_SMA_5)
    })
    output$vtable <- renderDataTable(release_summary, options=list(scrollX=TRUE))
  })
})