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
    
    output$burnup_chart_points <- renderPlotly({
      source("burnup_plot.R", local=TRUE)
      plotBurnupChart(data.frame(Date = release_summary$END_DATE, Completed = release_summary$COMPLETED_RELEASE_POINTS, Total = release_summary$TOTAL_RELEASE_POINTS), target_release_date, "Release Burnup", "Story Points")
    })
    
    output$burnup_chart_count <- renderPlotly({
      source("burnup_plot.R", local=TRUE)
      plotBurnupChart(data.frame(Date = release_summary$END_DATE, Completed = release_summary$COMPLETED_RELEASE_COUNT, Total = release_summary$TOTAL_RELEASE_COUNT), target_release_date, "Release Burnup", "Work Item Count")
    })
    
    output$velocity_chart <- renderPlotly({
      source("velocity_plot.R", local=TRUE)
      plotVelocityTimeSeries(release_summary$SPRINT_NAME, release_summary$VELOCITY, release_summary$PLANNED_VELOCITY, release_summary$VELOCITY_MA_5)
    })
    output$vtable <- renderDataTable(release_summary, options=list(scrollX=TRUE))
  })
})