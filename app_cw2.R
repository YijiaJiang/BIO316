library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

data <- read.csv("~/cw2/data/gene_exp.csv")
colnames(data)[10] <- "log2(fold_change)"

# Define UI ----
ui <- dashboardPage(
  dashboardHeader(title = em(h3(strong('Welcome to my shiny app'))), titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem(strong("About"), tabName = 'homepage', icon = icon('home')),
                     menuItem(strong('Data analysis results query'),tabName = 'gallery',icon = icon('dashboard'),
                              menuSubItem(p('Tables for differential gene expression results'), tabName = 'tab1'),
                              menuSubItem(p('Bar chart for showing the gene expression level'), tabName = 'tab2')),
                     menuItem(strong("Source code"),icon = icon("file-code-o"),href = "https://github.com/YijiaJiang/BIO316")
                   )),
  dashboardBody(tabItems(
    tabItem(tabName = 'tab1',
            fluidRow(
              box(title = "Table",solidHeader = TRUE,width = 8,
                  collapsible = TRUE,status = "primary",
                  dataTableOutput("table",height = 500)),
              box(title = "Variable input",solidHeader = TRUE,width = 4,
                  collapsible = FALSE,status = "primary",
                  textInput("geneID1", "Please enter the gene id:", "A1BG")),
              downloadButton("downloadData", "Download data"))),
    tabItem(tabName = 'tab2',
            fluidRow(
              box(title = "Bar Chart",solidHeader = TRUE,width = 8,
                  collapsible = TRUE,status = "primary",
                  plotOutput("barchart", height = 500)),
              
              box(title = "Variable input",solidHeader = TRUE,width = 4,
                  collapsible = FALSE,status = "primary",
                  textInput("geneID2", "Please enter the gene id:", "A1BG"),)
            ))
  ))
)



# Define server logic ----
server <- function(input, output) {
  
  data <- data[, c(2, 4, 5, 6, 8, 9, 10, 12, 14)]
  bardata <- data[, c(1, 3, 4, 5, 6)]
  tabledata <- data[, c(1, 2, 7, 8, 9)]
  
  output$barchart <- renderPlot({
    bardata <- bardata[which(tolower(input$geneID2) == tolower(bardata$gene_id)), ]
    x <-barplot(height = c(bardata$value_1, bardata$value_2),width = 0.5,space = 0.3,
                main = list("Expression level of the gene under two experimental conditions",font = 2),
                beside = TRUE,ylab = "Gene expression level",xlab = 'Sample type',
                col = c("mistyrose", "lavender"),names.arg = c("WT", "DSL3"))
    lbls <- paste(" ", round(c(bardata$value_1, bardata$value_2), 4))
    text(x,c(bardata$value_1, bardata$value_2),labels = lbls,cex = 1,pos = 1)
  })
  
  output$table <- renderDataTable({
    tabledata <-tabledata[which(tolower(input$geneID1) == tolower(tabledata$gene_id)), ]
    datatable(tabledata,height = 500,options = list(pageLength = 10))
  })
  
  tabledata <- tabledata
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
}


# Run the app ----
shinyApp(ui = ui, server = server)

