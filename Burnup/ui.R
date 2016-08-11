library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyr)

dashboardPage(
  dashboardHeader(title = "Scrum Scoreboard"),
  dashboardSidebar(
    tags$head(tags$style(HTML(type = "text/css", "#teams>li{position: relative; margin: 0; padding: 0;}"))),
    sidebarMenu(
      htmlTemplate("menu_item_list.html", name = "menulist")
    ),
    #sidebarMenuOutput("team"),
    actionButton("tabBut", "New Team")
    
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("BURNUP_CHART"),width=12)
    ),
    fluidRow(
      box(plotlyOutput("VELOCITY_CHART"),width=12),
      bsModal("myModal",
          "New Team",
          "tabBut",
          size = "large",
          numericInput("target",
                       label = h4("Target Release Sprint"),
                       value = 1),

          fileInput('file2', 'Choose Manual Data CSV File',
                    accept=c('text/csv',
                             'text/comma-separated-values,text/plain',
                             '.csv')),
          tags$hr(),
          checkboxInput('header2', 'Header', TRUE),
          radioButtons('sep2', 'Separator',
                       c(Comma=',',
                         Semicolon=';',
                         Tab='\t'),
                       ','),
          radioButtons('quote2', 'Quote',
                       c(None='',
                         'Double Quote'='"',
                         'Single Quote'="'"),
                       '"'),
          actionButton("run", "Save")
      )
    ),
    fluidRow(
      box(dataTableOutput('vtable'),width=12)
    ),
    tags$head(tags$script(src="custom.js"))
  )
)