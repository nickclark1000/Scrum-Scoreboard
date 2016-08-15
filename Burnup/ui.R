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
    actionButton("run", "Go")
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("BURNUP_CHART"),width=12)
    ),
    fluidRow(
      box(plotlyOutput("VELOCITY_CHART"),width=12)
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
  )
)