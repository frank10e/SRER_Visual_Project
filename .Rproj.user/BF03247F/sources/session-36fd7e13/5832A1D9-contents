

source("function.R",encoding="utf-8")



ui = tagList(
  useShinyjs(), 
  shinythemes::themeSelector(),
  autoWaiter(),
  # useWaiter(), # dependencies
  # waiterShowOnLoad(spin_fading_circles()), # shows before anything else 
  
  tags$head(
    tags$style(HTML("
        ##progressContainer {
          position: fixed;
          bottom: 10px;
          right: 10px;
          width: 200px;
          background-color: #ccc;
        }
        #progressBar {
          width: 0%;
          height: 20px;
          background-color: blue;
          text-align: center;
          color: white;
          line-height: 20px;
        }
      "))
  ),
  # shinythemes::themeSelector(),
  includeCSS("www/bootstrap.css"),
  # Application title
  list(tags$head(tags$style("body {background-color: #F3F1F1; }")), h2(HTML(' <center style="color:#05B967">Santa Rita Data Visualization</center> </p>' ))),
  includeCSS("www/bootstrap.css"),
  
  navbarPage( 
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    " ",
    ########################################################################
    #Navbar 1
    ########################################################################
    tabPanel(
      
      HTML("Visualization"),
      icon = icon("cog", lib = "glyphicon"),
      
     
      
      
      sidebarPanel(
        h2("Data Analysis"),
        uiOutput("plot1_sider") ,
        
        conditionalPanel(condition = "input.conditionedPanels1==1",
                         tabPanel(
                           "Navbar1_item1_parameter", uiOutput("plot1_sider1")
                         )) ,
        conditionalPanel(condition = "input.conditionedPanels1==2",
                         tabPanel(
                           "Navbar1_item2_parameter", uiOutput("Navbar1_item2_sider1")
                         ))  
      ), #sidebarPanel
      
      mainPanel(
        
        tabsetPanel(id = "conditionedPanels1",
          type = "tabs",
          
          tabPanel(
            HTML("Plot"),
            icon = icon("cog", lib = "glyphicon"),
            
            # h3("plot1"),
            # verbatimTextOutput("test_print"),
            
            fluidRow( 
              # column(4,plotOutput('axi_plot2', click = "plot_click_axi")),
              column(6,plotOutput('mapPlot' )),
              column(6,plotOutput('avgMapPlot' ))
            ),
            fluidRow(
              column(6, 
                     # uiOutput("map_single_ui")
                     # plotOutput("map_single")
                     tmapOutput("map_single")
                     ),
              column(6, 
                     tmapOutput("map_three")
                     # uiOutput("map_three_ui")
                     )
            ),
            # # verbatimTextOutput("print_Navbar1_item1"), 
            # plotOutput("mapPlot", height = "400px"),
            # plotOutput("avgMapPlot"),
            
            
            value = 1
          ) 
          
          # tabPanel(
          #   HTML("plot2"),
          #   icon = icon("cog", lib = "glyphicon"),
          #   
          #   h3("plot2"),
          #   # plotOutput("avgMapPlot"),
          #   
          #   verbatimTextOutput("print_Navbar1_item2"),
          #   
          #   value = 2
          # ) 
          
        ) # tabsetPanel
      ) # mainPanel
    ),  # tabPanel Navbar 1
    ########################################################################
    # Navbar 2
    ######################################################################## 
    tabPanel(
      HTML("About"),
      icon = icon("bar-chart-o"),
      
      sidebarLayout(
        # Sidebar with a slider input
        sidebarPanel(
          h2("About"),
          
          conditionalPanel(condition = "input.conditionedPanels2==1",
                           tabPanel(
                             "Navbar2_item1_parameter", uiOutput("Navbar2_item1_sider1")
                           )) ,
          conditionalPanel(condition = "input.conditionedPanels2==2",
                           tabPanel(
                             "Navbar2_item2_parameter", uiOutput("Navbar2_item2_sider1")
                           ))
        ),  #sidebarPanel 
        
        mainPanel(
          
          tabsetPanel(
            id = "conditionedPanels2",
            type = "tabs",
            tabPanel(
              HTML("About"),
              icon = icon("cog", lib = "glyphicon"),
              
              h3("About context:"),
              verbatimTextOutput("About context"),
              
              value = 1
            ) 
          ) # tabsetPanel
        ) # mainPanel
      ) #sidebarLayout
    ), # tabPanel Navbar 2 
      
 
  
  ) # navbarPage
  ) # tagList
