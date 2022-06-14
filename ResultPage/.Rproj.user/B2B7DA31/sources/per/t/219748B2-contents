library(shiny)
library(DT)
library(shinyalert)
library(shinycssloaders)
library(collapsibleTree)
library(shinyhelper)
library(plotly)
library(data.tree)

tissue_region <- Node$new("Selected tissue region")
cell_type1 <- tissue_region$AddChild("cell_type1")
cell_type2 <- tissue_region$AddChild("cell_type2")
cell_type1_cluster1 <-cell_type1$AddChild("cell_type1_cluster1")
cell_type1_cluster2 <-cell_type1$AddChild("cell_type1_cluster2")
cell_type1_cluster2_01 <- cell_type1_cluster2$AddChild("cell_type1_cluster2_01")
cell_type1_cluster2_02 <- cell_type1_cluster2$AddChild("cell_type1_cluster2_02")
dtree <- tissue_region

ui <- navbarPage(title = "AdultEncephalon",
           id = "lb",
           position = "fixed-top",
           theme = "bootstrap_ad_brain.css",
           
           # Taxonomy tab
           #======================================================================================================
           tabPanel(title = "Taxonomy",
                    value = "taxonomy",
                    br(),
                    br(),
                    br(),
                    br(),
                    
                    useShinyalert(),
                    
                    # Display clickable, collapsible tree of cells
                    wellPanel(fluidRow(column(width = 2,
                                              checkboxInput(inputId = "collapse",
                                                            label = "Collapse tree",
                                                            value = FALSE)),
                                       column(width = 10,
                                              collapsibleTreeOutput(outputId = "plot",
                                                                    height = "800px",
                                                                    width = "auto") %>% withSpinner(color= "#C0C0C0")))),
                    hr(),
                    
                    mainPanel(
                      DTOutput('tbl')
                    )
                    
                    
                    
              
           )
)

server <- function(input, output, session) {
  observe_helpers()
  
  shinyalert(title = "Welcome to the XXXXXX",
             text = "This app allows to XXXXXXXX",
             type = "info",
             closeOnClickOutside = TRUE)
  
  # Taxonomy
  #===========================================================================================
  
  # Plot collapsible tree, treat clicked node/leaf as input to reactive expressions
  output$plot <- renderCollapsibleTree({
    collapsibleTree(dtree, 
                    inputId = "node",
                    fill = "colors",
                    tooltip = TRUE,
                    collapsed = input$collapse)
  })
  output$tbl <- renderDT(iris, options = list(lengthChange = FALSE))
  
  output$node_list <-renderText({
    paste("You have selected",input$node)
  })

  

}

# Run the application 
shinyApp(ui = ui, server = server)
