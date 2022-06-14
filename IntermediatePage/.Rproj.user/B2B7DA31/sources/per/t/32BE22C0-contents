library(shiny)

ui <- 
  
  fluidPage(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap_intermediate.css")
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
           "Click on a brain region to open its relative atlas."),
    
    tags$div(class="main",
             tags$img(src="ad_brain_transparent.png",alt="lamprey brain", class="background"),
             tags$a(href="#", title="Telencephalon",
                    tags$img(src="ad_tele.png",alt="Telencephalon",id="overlay1")),
             
             tags$a(href="#",title="Diencephalon",
                    tags$img(src="ad_di.png",alt="Diencephalon",id="overlay2")),
             
             tags$a(href="#",title="Mesencephalon",
                    tags$img(src="ad_mes.png",alt="Mesencephalon",id="overlay3")),
             
             tags$a(href="#",title="Rhombencephalon",
                    tags$img(src="ad_rhomb.png",alt="Rhombencephalon",id="overlay4")),
             
             )
    
    
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
