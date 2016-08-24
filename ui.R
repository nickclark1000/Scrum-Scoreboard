library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyr)

dashboardPage(
  dashboardHeader(title = "Scrum Scoreboard"),
  dashboardSidebar(
    selectInput("collection", "TFS Collection",
                c("FinancialReportingCollection", "DTSCollection")
    ),
    uiOutput("projects"),
    uiOutput("teams"),
    tags$div(
      style = "padding-left: 12px",
      actionButton("run", "Go")
    )
    
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("burnup_chart"),width=12)
    ),
    fluidRow(
      box(plotlyOutput("velocity_chart"),width=12)
      # bsModal("myModal",
      #     "New Team",
      #     "tabBut",
      #     size = "large",
      #     numericInput("target",
      #                  label = h4("Target Release Sprint"),
      #                  value = 1),
      #     actionButton("run", "Save")
      #)
    ),
    fluidRow(
      box(dataTableOutput('vtable'),width=12)
    )
  ),
  tags$head(
    tags$style(
      HTML(".shiny-progress .bar {
              background-color: #FF0000;
              opacity = 0.8;
           }
           .shiny-progress .progress {
              height:7px;
           }"
      )
    )
  )
)
