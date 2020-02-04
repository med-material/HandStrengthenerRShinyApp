library(shiny)
library(plotly)
library(shinyjs)

# Define UI for application that plots features of movies 
shinyUI(
  fluidPage(
    includeCSS("custom.css"),
    useShinyjs(),
    fluidRow(
      titlePanel("Hand Strengthener Data Analysis"),
      #column(4,
      #       column(1, style = "margin-top : 20px; text-align: right;", icon("user", class = "fa-2x", lib="font-awesome")),
      #       column(11,style = "margin-top : 20px; text-align: center;",selectInput("selectExperiment", NULL, choices = levels(M$Exp.ID)))
      #),
      tabsetPanel(id = "tabs", type = "tabs",
        tabPanel(id = "explorepan", strong("Experiment Exploration"),
          sidebarPanel( 
            p("Choose what to Plot:"),
            selectInput(inputId = "Xaxis", 
                        label = "X-Axis:",
                        choices = colnames(D),
                        selected = "PID"),
            selectInput(inputId = "Yaxis", 
                        label = "Y-Axis:",
                        choices = colnames(D),
                        selected = "Kb.KeyCode"),
          ),
          mainPanel(
            # Output: Histogram ----
            #strong("visualization goes here")
            plotlyOutput("explorationPlot")
            #plotOutput(outputId = "scatterplot", hover = "plot_hover", height="650px"),
          )
        ),
        tabPanel(id = "timepan", strong("Time Exploration"),
                 sidebarPanel( 
                   p("Choose what to Plot:"),
                   selectInput(inputId = "timeYaxis", 
                               label = "Y-Axis:",
                               choices = colnames(D),
                               selected = "PID")
                 ),
                 mainPanel(
                   # Output: Histogram ----
                   #strong("visualization goes here")
                   plotlyOutput("timeExplorationPlot")
                   #plotOutput(outputId = "scatterplot", hover = "plot_hover", height="650px"),
                 )
        )
      ),
      tags$footer()
    )
  )
)

# Sidebar layout with a input and output definitions 
#sidebarLayout(
#
# Inputs
#sidebarPanel(
#
# Select variable for filtering
#selectInput(inputId = "baguetteSize", 
#            label = "Baguette Size",
#            choices = c("Small" = "Small",
#                        "Medium" = "Medium",
#                        "Large", "Large"), 
#            selected = "Small"),   
# Select variable for filtering
#selectInput(inputId = "eyePatch", 
#            label = "Eye Patch",
#            choices = c("Off" = "Off",
#                        "Right" = "Right",
#                        "Left" = "Left"), 
#            selected = "On"), 
# Select variable for y-axis
#selectInput(inputId = "y", 
#            label = "Y-axis:",
#            choices = c("Time to Cut" = "TimeToCut",
#                        "Cut Position" = "CutPosition",
#                        "Trial Number" = "TrialNumber"),
#            selected = "TimeToCut"),
# Select variable for x-axis
#selectInput(inputId = "x", 
#            label = "X-axis:",
#            choices = c("Cut Position" = "CutPosition",
#                        "Time To Cut" = "TimeToCut",
#                        "Participant No." = "ParticipantNumber"), 
#            selected = "CutPosition"),
# Select variable for color
#selectInput(inputId = "z",
#            label = "Color by:",
#            choices = c("Prism Offset" = "PrismOffset",
#                        "Mirror" = "Mirror",
#                        "Time To Cut" = "TimeToCut"),
#            
#            selected = "PrismOffset"),
# Select variables to download
#
#checkboxGroupInput(inputId = "email",
#                   label = "Student Email:",
#                   choices = levels(df$Email),
#                   selected = levels(df$Email))
#),
#
# Outputs
#mainPanel(
#  plotOutput(outputId = "scatterplot", hover = "plot_hover", height="650px"),
#  # Show data table
#  dataTableOutput(outputId = "hovertable"),
#  br()
#)
#)