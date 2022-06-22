library(shiny)
library(DT)
library(shinyalert)
library(shinycssloaders)
library(collapsibleTree)
library(shinyhelper)
library(plotly)
library(data.tree)
library(ggplot2)
library(shinyBS)

Cell_Clusters <- Node$new("Selected cell type")
cluster0 <- Cell_Clusters$AddChild("cluster0")
cluster1 <-cluster0$AddChild("cluster1")
cluster2 <-cluster0$AddChild("cluster2")
cluster3 <- cluster0$AddChild("cluster3")
cluster4 <-cluster2$AddChild("cluster4")
cluster5 <-cluster2$AddChild("cluster5")

cluster6 <- Cell_Clusters$AddChild("cluster6")
cluster7 <-cluster6$AddChild("cluster7")
cluster8 <-cluster6$AddChild("cluster8")
cluster9 <- cluster8$AddChild("cluster9")
cluster10 <-cluster8$AddChild("cluster10")
cluster11 <-cluster10$AddChild("cluster11")
cluster12 <-cluster10$AddChild("cluster12")

dtree <- Cell_Clusters

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
                          
                          # Display clickable Table
                          conditionalPanel(condition = "output.node_selected == true",
                                           fluidRow(column(width = 8,offset=2,DTOutput(outputId  = 'tbl')),
                                                    column(width = 1, actionButton(inputId = "search_gene",
                                                                                   label = "Go to Gene！",
                                                                                   style = "background-color: #C0C0C0;"))) %>%
                                             helper(icon = "question-circle",
                                                    colour = "#C0C0C0",
                                                    type = "inline",
                                                    title = "Marker genes",
                                                    easyClose = TRUE,
                                                    content = c("Click on any gene on the table and hit the <b>Go to gene!</b> button to visualize
                                               gene expression and other features."))),
                          
                          hr(),
                          fluidRow(column(width=4,offset=2,plotlyOutput(outputId = "seurat_umap_2d") %>% withSpinner(color= "#C0C0C0")),
                                   column(width=4,plotlyOutput(outputId = "seurat_tsne_2d")%>% withSpinner(color= "#C0C0C0")))
                          
                          
                 ),
                 # Clusters tab
                 #======================================================================================================
                 tabPanel(title = "Clusters",
                          value = "clusters",
                          br(),
                          br(),
                          br(),
                          br(),
                          
                          fluidRow(column(width=2,
                                          sliderInput(inputId = "opacity",
                                                      label = "Point Opacity",
                                                      min = 0,
                                                      max = 1,
                                                      value = 0.5,
                                                      step = 0.1),
                                          sliderInput(inputId = "p_size",
                                                      label = "Point Size",
                                                      min = 1,
                                                      max = 10,
                                                      value = 5,
                                                      step = 0.5)),
                                   column(width = 5,
                                          plotlyOutput(outputId = "seurat_umap_2d_C") %>% withSpinner(color= "#C0C0C0")),
                                   column(width = 5,
                                          plotlyOutput(outputId = "seurat_tsne_2d_C") %>% withSpinner(color= "#C0C0C0")),
                                   
                          )
                 ),
                 
                 # Genes
                 #===========================================================================================
                 tabPanel(title = "Genes",
                          value = "genes",
                          br(),
                          br(),
                          br(),
                          br(),
                          
                          tabsetPanel(
                            tabPanel(title = "Example Species Genes",
                                     value = "example species genes",
                                     fluidRow(column(width = 8,
                                                     offset = 2,
                                                     DTOutput(outputId = "GenePanelTable") %>% withSpinner(color= "#C0C0C0"),
                                                     actionButton(inputId = "go",
                                                                  label = "Go!",
                                                                  style = "background-color: #C0C0C0;"),
                                     ))),
                            
                            tabPanel(title = "Orthologs",
                                     value = "ortho",
                                     fluidRow(wellPanel(
                                       tags$button(id = "ciona",
                                                   class = "btn action-button",
                                                   img(src = "sea_squirt.png",
                                                       height = "50px")),
                                       bsTooltip(id = "ciona",
                                                 title = "<b>Sea squirt</b><br><i>Ciona intestinalis</i><br>Ensembl 97",
                                                 placement = "bottom",
                                                 trigger = "hover"),
                                       tags$button(id = "hagfish",
                                                   class = "btn action-button",
                                                   img(src = "hagfish.png",
                                                       height = "50px")),
                                       bsTooltip(id = "hagfish",
                                                 title = "<b>Inshore hagfish</b><br><i>Eptatretus burgeri</i><br>Ensembl 97",
                                                 placement = "bottom",
                                                 trigger = "hover"),
                                     )),
                                     column(width = 8,
                                            offset = 2,
                                            DTOutput(outputId = "ortho"),
                                            actionButton(inputId = "ortho_go",
                                                         label = "Go!",
                                                         style = "background-color: #C0C0C0;"))
                                     %>% 
                                       helper(icon = "question-circle",
                                              colour = "#C0C0C0",
                                              type = "inline",
                                              title = "Genes",
                                              easyClose = TRUE,
                                              content = c("Some help info"))),
                            
                            
                          ),
                          tabsetPanel(id = "g",
                                      
                                      tabPanel(title = "Expression",
                                               value = "expression",
                                               fluidRow(column(width = 2,
                                                               
                                                               sliderInput(inputId = "opacity2",
                                                                           label = "Point opacity:",
                                                                           min = 0,
                                                                           max = 1,
                                                                           value = 0.5,
                                                                           step = 0.1),
                                                               sliderInput(inputId = "p_size2",
                                                                           label = "Point size:",
                                                                           min = 1,
                                                                           max = 10,
                                                                           value = 5,
                                                                           step = 0.5)
                                               ),
                                               
                                               column(width = 5,
                                                      plotlyOutput(outputId = "seurat_umap_2d_G") %>% 
                                                        withSpinner(color= "#C0C0C0")),
                                               column(width = 5,
                                                      plotlyOutput(outputId = "seurat_tsne_2d_G") %>% 
                                                        withSpinner(color= "#C0C0C0"))),
                                               
                                               hr(),
                                               
                                               fluidRow(column(width = 10,
                                                               offset = 1,
                                                               plotOutput(outputId = "seurat_violin") %>% 
                                                                 withSpinner(color= "#C0C0C0"))),
                                               
                                               hr(),
                                               
                                               fluidRow(column(width = 10,
                                                               offset = 1,
                                                               plotlyOutput(outputId = "mc_barplot") %>% 
                                                                 withSpinner(color= "#C0C0C0")))
                                               
                                      ),
                                      tabPanel(title = "Alignment",
                                               fluidRow(
                                                 column(width = 10,
                                                        textOutput(outputId = "msa") %>% 
                                                          withSpinner(color= "#C0C0C0"))
                                               )
                                      ),
                                      
                          ))
)

server <- function(input, output, session) {
  observe_helpers()
  
  shinyalert(title = "Welcome to the XXXXXX",
             text = "This app allows to XXXXXXXX",
             type = "info",
             closeOnClickOutside = TRUE)
  
  # Taxonomy
  #===========================================================================================
  
  
  #预读取相关数据
  all_marker_table <- read.csv('../DemoData/clusters_markers_table/all_markers.txt',sep='\t',head=TRUE)
  umap_table = read.csv('../DemoData/clusters_embeddings_table/umap.txt',sep='\t')
  expression_table = data.frame(read.csv('../DemoData/clusters_markers_table/merge_expression_embeddings.txt',sep='\t'))
  Orthologs_table <- read.csv('../DemoData/Orthologs_table/example_table.txt',sep='\t')
  
  
  # Conditional Panel 第一页选择node后显示表格
  output$node_selected <- reactive({
    if (length(input$node) > 0) {
      TRUE
    }
  })
  outputOptions(output, "node_selected", suspendWhenHidden = FALSE) 
  
  # Observe event
  v <- reactiveValues(data = NULL)
  
  gene_re <- reactive({
    if (is.null(v$data)) return("CCR7")
    v$data
  })
  
  TaxPanel_click_re <- reactive({
    file_path<-paste('../DemoData/clusters_markers_table/',tail(input$node, n=1),'_markers_table.txt',sep='')
    target_table<-read.csv(file_path,sep='\t',head=TRUE)
    target_table[input$tbl_cells_selected[1],7]
  })
  
  GenePanel_click_re <- reactive({
    all_marker_table[input$GenePanelTable_cells_selected[1],7]
  })
  
  
  observeEvent(input$search_gene,{
    updateNavbarPage(session,"lb",selected = "genes")
    v$data <- TaxPanel_click_re()
  })
  
  observeEvent(input$go,{
    v$data <- GenePanel_click_re()
  })
  
  
  
  # Plot collapsible tree
  output$plot <- renderCollapsibleTree({
    collapsibleTree(dtree, 
                    inputId = "node",
                    fill = "colors",
                    tooltip = TRUE,
                    collapsed = input$collapse)
  })
  # Plot marker gene table based on selected node
  output$tbl <- renderDT({
    if (length(input$node) >0 ) {
      
      file_path<-paste('../DemoData/clusters_markers_table/',tail(input$node, n=1),'_markers_table.txt',sep='')
      target_table<-read.csv(file_path,sep='\t',head=TRUE)
      datatable(target_table,selection = list(mode="single",target="cell"))
    }
  })
  
  # Plot umap in first panel
  output$seurat_umap_2d <- renderPlotly({
    umap_table = read.csv('../DemoData/clusters_embeddings_table/umap.txt',sep='\t')
    if (length(input$node) == 0) {
      plot_ly(type = 'scatter', mode = 'markers') %>% 
        add_markers(data = umap_table ,x =  ~UMAP_1, y = ~UMAP_2,split= ~x,showlegend = F)
    } else if (length(input$node) >0){
      target_cluster = umap_table[which(umap_table$x == gsub('[cluster]','',tail(input$node, n=1))),]
      other_cluster = umap_table[which(umap_table$x != gsub('[cluster]','',tail(input$node, n=1))),]
      plot_ly(type = 'scatter', mode = 'markers') %>%
        add_markers(data = target_cluster ,x =  ~UMAP_1, y = ~UMAP_2,marker=list(opacity = 1,color='blue'),showlegend = F) %>%
        add_markers(data = other_cluster ,x =  ~UMAP_1, y = ~UMAP_2,marker=list(opacity = 0.2,color='grey'),showlegend = F)
    }
  })
  
  # Plot tsne in first panel
  output$seurat_tsne_2d <- renderPlotly({
    tsne_table = read.csv('../DemoData/clusters_embeddings_table/tsne.txt',sep='\t')
    if (length(input$node) == 0) {
      plot_ly(type = 'scatter', mode = 'markers') %>% 
        add_markers(data = tsne_table ,x =  ~tSNE_1, y = ~tSNE_2,split= ~x,showlegend = F)
    } else if (length(input$node) >0) {
      target_cluster = tsne_table[which(tsne_table$x == gsub('[cluster]','',tail(input$node, n=1))),]
      other_cluster = tsne_table[which(tsne_table$x != gsub('[cluster]','',tail(input$node, n=1))),]
      plot_ly(type = 'scatter', mode = 'markers') %>%
        add_markers(data = target_cluster ,x =  ~tSNE_1, y = ~tSNE_2,marker=list(opacity = 1,color='blue'),showlegend = F) %>%
        add_markers(data = other_cluster ,x =  ~tSNE_1, y = ~tSNE_2,marker=list(opacity = 0.2,color='grey'),showlegend = F)
    }
  })
  
  
  # Taxonomy
  #===========================================================================================
  
  # Plot umap in second panel
  output$seurat_umap_2d_C <- renderPlotly({
    umap_table = read.csv('../DemoData/clusters_embeddings_table/umap.txt',sep='\t')
    plot_ly(type = 'scatter', mode = 'markers') %>% 
      add_markers(data = umap_table ,x =  ~UMAP_1, y = ~UMAP_2,split= ~x, marker = list(opacity=input$opacity,size=input$p_size))
  })
  
  # Plot tsne in second panel
  output$seurat_tsne_2d_C <- renderPlotly({
    tsne_table = read.csv('../DemoData/clusters_embeddings_table/tsne.txt',sep='\t')
    plot_ly(type = 'scatter', mode = 'markers') %>% 
      add_markers(data = tsne_table ,x =   ~tSNE_1, y =  ~tSNE_2,split= ~x, marker = list(opacity=input$opacity,size=input$p_size))
  })
  
  
  # Gene
  #===========================================================================================
  
  # 第三页面的table，直接跳转默认搜索CCR7基因，或者根据第一个表格上选择的基因跳转
  output$GenePanelTable <- renderDT({
    datatable(all_marker_table,selection = list(mode="single",target="cell"),options = list(search = list(search = gene_re())))
    
  })
  
  
  # 第三页面的umap plot，直接跳转默认CCR7基因，或者根据当前页面表格上选择的基因跳转
  output$seurat_umap_2d_G <- renderPlotly({
    
    validate(
      need(gene_re() %in% all_marker_table$gene,"Gene expression not detected in this dataset!")
    )
    
    tc = expression_table %>% select(Row.names,x.x,UMAP_1,UMAP_2,.data[[gene_re()]]) %>% filter(.data[[gene_re()]] != 0)
    oc = expression_table %>% select(Row.names,x.x,UMAP_1,UMAP_2,.data[[gene_re()]]) %>% filter(.data[[gene_re()]] == 0)
    
    plot_ly(type = 'scatter', mode = 'markers') %>% 
      add_markers(data = tc,x =  ~UMAP_1, y = ~UMAP_2,split= ~x.x, marker = list(opacity=1,color='blue'),showlegend = F)%>%
      add_markers(data = oc,x =  ~UMAP_1, y = ~UMAP_2,split= ~x.x, marker = list(opacity=0.2,color='grey'),showlegend = F)
    
  })
  
  # 第三页面的tsne plot，直接跳转默认CCR7基因，或者根据当前页面表格上选择的基因跳转
  output$seurat_tsne_2d_G <- renderPlotly({
    
    validate(
      need(gene_re() %in% all_marker_table$gene,"Gene expression not detected in this dataset!")
    )
    
    tc = expression_table %>% select(Row.names,x.y,tSNE_1,tSNE_2,.data[[gene_re()]]) %>% filter(.data[[gene_re()]] != 0)
    oc = expression_table %>% select(Row.names,x.y,tSNE_1,tSNE_2,.data[[gene_re()]]) %>% filter(.data[[gene_re()]] == 0)
    
    plot_ly(type = 'scatter', mode = 'markers') %>% 
      add_markers(data = tc,x =  ~tSNE_1, y = ~tSNE_2,split= ~x.y, marker = list(opacity=1,color='blue'),showlegend = F)%>%
      add_markers(data = oc,x =  ~tSNE_1, y = ~tSNE_2,split= ~x.y, marker = list(opacity=0.2,color='grey'),showlegend = F)
    
  })
  
  output$ortho <- renderDT({
    datatable(Orthologs_table,
              escape = FALSE,
              )
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)