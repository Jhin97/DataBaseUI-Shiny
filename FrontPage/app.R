library(shiny)

ui <- 
  
  fluidPage(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap_main.css")
    ),
    
    tags$div(class="header",
             tags$a(href = "http://www.ismsz.cn/Web/KXYJKYTDPage?Id=20&PageId=1634", "课题组!",target="_self"),
             tags$a(href = "https://github.com/Jhin97",icon("fab fa-github",target="_self")),
             tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/",target="_self","Preprint",class="last")
             ),
    
    tags$h1(class="title",
            "This is Title!"
            ),
    
    tags$p(class="title2",
           "Database UI for (",tags$i("XXX"),")"),
    
    tags$div(class="main",
             tags$div(class="content",
                      tags$div(class="text","Species1"),
                      tags$a(href="#",target="_self",title="Link to species 1",
                             tags$img(src="adult@0.1x.png",style="width:100%")),
                      ),
             tags$div(class="content",
                      tags$div(class="text","Species2"),
                      tags$a(href="#",target="_self",title="Link to species 2",
                             tags$img(src="ammocoete@0.1x.png",style="width:100%"))
              )),
    
    tags$div(class="footer",
             tags$div(tags$p("Author: XXX | Contact:",
                             tags$a(href="#","email-address"),
                             "|",
                             tags$a(href="#","Privacy Notice")
                             ))),
  
  
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
