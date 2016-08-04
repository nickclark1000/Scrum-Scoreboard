library(shiny)
library(shinyBS)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyr)

# teams <- c('kore', 'crusaders', 'solidarity')
# createTeamModals <- function(teams) {
#   apply(teams,1,function(row){
#     bsModal(row[],
#             "New Team",
#             "tabBut",
#             size = "large",
#             textInput("teamname",
#                       label = h4("Team Name"),
#                       value = "Enter team name..."),
#             tags$div(title="String in TFS Iteration Path that identifes the release",
#                      textInput("release",
#                                label = h4("Release Name"),
#                                value = "Enter release name...")
#             ),
#             numericInput("firstsprint",
#                          label = h4("First Sprint"),
#                          value = 1),
#             tags$style( type = "text/css", ".datepicker{z-index: 1100 !important;}"),
#             dateInput("firstsprintday",
#                       label = h4("First Day of First Sprint")
#             ),
#             numericInput("currentsprint",
#                          label = h4("Current Sprint"),
#                          value = 1),
#             numericInput("target",
#                          label = h4("Target Release Sprint"),
#                          value = 1),
#             fileInput('file1', 'Choose CSV File',
#                       accept=c('text/csv',
#                                'text/comma-separated-values,text/plain',
#                                '.csv')),
#             tags$hr(),
#             checkboxInput('header', 'Header', TRUE),
#             radioButtons('sep', 'Separator',
#                          c(Comma=',',
#                            Semicolon=';',
#                            Tab='\t'),
#                          ','),
#             radioButtons('quote', 'Quote',
#                          c(None='',
#                            'Double Quote'='"',
#                            'Single Quote'="'"),
#                          '"'),
#             actionButton("run", "Save")
#     )
#   })
#     
#   
# }
# createTeamModals(teams)

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
          textInput("teamname",
                    label = h4("Team Name"),
                    value = "Enter team name..."),
          tags$div(title="String in TFS Iteration Path that identifes the release",
                   textInput("release",
                             label = h4("Release Name"),
                             value = "Enter release name...")
          ),
          numericInput("firstsprint",
                       label = h4("First Sprint"),
                       value = 1),
          tags$style( type = "text/css", ".datepicker{z-index: 1100 !important;}"),
          dateInput("firstsprintday",
                    label = h4("First Day of First Sprint")
          ),
          numericInput("currentsprint",
                       label = h4("Current Sprint"),
                       value = 1),
          numericInput("target",
                       label = h4("Target Release Sprint"),
                       value = 1),
          fileInput('file1', 'Choose TFS-Export CSV File',
                    accept=c('text/csv',
                             'text/comma-separated-values,text/plain',
                             '.csv')),
          tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',
                       c(Comma=',',
                         Semicolon=';',
                         Tab='\t'),
                       ','),
          radioButtons('quote', 'Quote',
                       c(None='',
                         'Double Quote'='"',
                         'Single Quote'="'"),
                       '"'),
          tags$hr(),
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