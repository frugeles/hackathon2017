
shinydashboard::dashboardPage(
  
  skin = "green",
  
  ## 1/ a header
  shinydashboard::dashboardHeader(title = "911 Calls in Seattle"),
  
  ## 2/ a Sidebar
  shinydashboard::dashboardSidebar(
    
    width = 320,
    
    shinydashboard::sidebarMenu(id = "menu",
                                selectInput("IN_CallType",
                                            label = h4("Call Type"),
                                            choices = Type,
                                            selected = levels(Type)[1]),
                                
                                sliderInput("IN_Date",
                                            label = h4("Date"),
                                            min = min(Date),
                                            max = max(Date),
                                            value = c(min(Date), max(Date))),
                                
                                sliderInput("IN_Time",
                                            label = h4("Time of Day"),
                                            min = min(Time),
                                            max = max(Time),
                                            value = c(min(Time), max(Time))),
                                
                                selectInput("IN_Sort",
                                            label = h4("Sort"),
                                            choices = list("Population", "Calls", "Call.Pop"),
                                            selected = "Call.Pop"),
                                
                                selectInput("IN_Sort2",
                                            label = NULL,
                                            choices = list("ascending" = " ", 
                                                           "descending" = "-"),
                                            selected = "-")
    )
  ),
  
  ## 3/ a body
  shinydashboard::dashboardBody(
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")),
    
    shinydashboard::tabItem(
      "xx",
      shiny::tabsetPanel(
        type = "tabs",
        
        
        ### Stat
        shiny::tabPanel(
          "Stat",
          tableOutput("OUT_Stat")
        ),
        
        
        ### Map
        shiny::tabPanel(
          "Map",
            leafletOutput("LeafletOut.byAll")
            
          )
        )
    )  
  )
)