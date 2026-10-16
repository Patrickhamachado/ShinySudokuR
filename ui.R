


# Librerías ---------------------------------------------------------------

library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
library(bs4Dash)
library(markdown)
library(shinyWidgets)


# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
shinyUI(dashboardPage(  
        #dashboardPagePlus(
  
  #   # Keyboard pressed script
  #   tags$script('
  #   $(document).on("keyup", function (e) {
  #      Shiny.onInputChange("myKeyboard", e.which);
  #   });
  # '),
  
  # Header ----
  dashboardHeader(title = "Shiny Sudoku", 
                      fixed = FALSE
                      ),
  
  # Sidebar ----
  dashboardSidebar(
    
    # sidebarMenu ----
    sidebarMenu(
      id = 'sidebar',
      
      # Menu Play --------------
      menuItem("Game", 
               tabName = 'Play', 
               icon = icon('gamepad') ),
      
      # División para ocultar o mostrar
      div(
        id = 'sidebar_play',
        
        conditionalPanel(
          "input.sidebar === 'Play'",
          
          # Botón Play
          actionBttn("play", label = "New Sudoku",
                     icon = icon("play-circle"),
                     style = "minimal"
          )
        )
      ), # Fin div
      
      # Menú Instructions
      menuItem("Instructions", tabName = "Instructions", 
               icon = icon("question-circle")
      )
      
    ) # Fin sidebarMenu Play        
    
  ),
  
  # dashboardBody ----
  dashboardBody(
    
    # Keyboard pressed script
    tags$script('
      $(document).on("keyup", function (e) {
         Shiny.onInputChange("myKeyboard", [e.which,e.timeStamp]);
      });
                  '
    ),
    
    # Selector de temas
    # shinythemes::themeSelector(),
    
    # Pestañas
    tabItems(
      
      # Tab 1: Play ----
      tabItem(tabName = "Play",
              fluidRow(
                box(width = 10,
                        height = 600,
                        # title = "Controls",
                        # status = "primary",
                        
                        # Botones para llenar
                        radioGroupButtons(
                          inputId = "fill",
                          label = "Fill/mark with:",
                          choices = 1:9,
                          selected = 1, 
                          status = "primary",
                          checkIcon = list(
                            yes = icon("ok", 
                                       lib = "glyphicon"))
                        ),
                        
                        # Selector de Marcar
                        switchInput(
                          inputId = "marking",
                          label = "Action",
                          value = TRUE,
                          # labelWidth = "80px",
                          onLabel = "Fill",
                          offLabel = "Mark",
                          onStatus = "primary",
                          offStatus = "info"
                        ),
                        
                        # Plot Sudoku
                        # plotOutput("plotSudoku",
                        #            width = "498px",
                        #            height = "448px"),
                        
                        uiOutput("plotui")
                        
                ) # Fin box
              ) # Fin fluidRow
              
      ), # Fin Tab 1: Play
      
      # Tab 2: Instructions ----
      tabItem(tabName = "Instructions",
              # h2("Instructions tab content"),
              includeMarkdown("instructions.Rmd")
      ) # Fin Tab 2: Instructions
    ) # Fin tabItems
  ), # Fin dashboardBody
  
  # Otros parámetros dashboardPage ----
  # skin = "red", # "blue", "blue-light", "black", "black-light",
  # "purple", "purple-light", "green", "green-light", 
  # "red", "red-light", "yellow", "yellow-light"
  # collapse_sidebar = TRUE
  # sidebar_background = 'light'
  # sidebar_fullCollapse = FALSE
  
) # Fin fluid page


) # Fin shinyUI
