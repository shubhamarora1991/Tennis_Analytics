library(shiny)
library(shinythemes)
library(markdown)
library(DT)
#setwd("C:/Users/snowf/Desktop/MGMT using R/epicurious-recipes-with-rating-and-nutrition")

# Define UI for app ----
# Add navigation bar
ui <- navbarPage (theme= shinytheme('cerulean'),"Tennis Analytics",
                  
                  #tab1 layout
                  tabPanel ( "Player Data",
                             tags$head(
                               
                               #Set color for warning message: 
                               tags$style(HTML("
                                               .shiny-output-error-validation {
                                               color: red;
                                               }
                                               "))
                               ),
                             
                             
                             sidebarLayout(
                               sidebarPanel(
                                 # Input: number of meals 
                                 selectizeInput('playername', choices = NULL,label ="Enter Player Name"),
                                 dateRangeInput("DateRange",label = "Enter the Date Range", min = "2014-01-01", max = "2018-08-31", startview = "month"),
                                 radioButtons("Gender",label = "Select ATP or WTA", c('ATP', 'WTA')),
                                 checkboxGroupInput("Statistics", label = "Playing Statistics", choices = c("Aces", "Double Faults", "Break Points Won"),selected = NULL, inline = FALSE,
                                                    width = '100%'),
                                 
                                 #actionbutton to execute 
                                 tags$head(
                                   tags$style(HTML('#button{background-color:orange}'))
                                 ),
                                 actionButton(inputId = 'button', label = 'GO')
                               ),
                               
                               # Main panel for displaying outputs ----
                               mainPanel(
                                 
                                 #Add gif for background
                                # tags$div(img(src = "smallplates-hero-foreverloop.gif", width = "950px", height = "300px")),
                                 
                                 #Player result result
                                 DT::dataTableOutput('table')
                                 
                               )
                             )
                               )
                           )           
                 

# Define server logic for slider examples ----
server <- function(input, output,session){
  #tab 1 text 
  data = read.csv("tennis_data.csv")
  atp_rating <- read.csv("atp_rating.csv")
  wta_rating <- read.csv("wta_rating.csv")
  source("pullplayerdata.R")
  
  choi = unique(data$Player_name.x)
  updateSelectizeInput(session, 'playername', choices = choi, server = TRUE)
  
  df_result <- eventReactive(
    input$button,{
      playerdt(input$playername,as.POSIXct(input$DateRange[1]),as.POSIXct(input$DateRange[2]))
    })
  
  output$table <- DT::renderDataTable({
    
    df_result()
  },escape=FALSE,options = list(lengthChange = FALSE)
  ) 
   
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



