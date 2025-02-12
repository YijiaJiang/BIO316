library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)

wholedata <- read.csv("~/cw2/data/gene_exp.csv")
colnames(wholedata)[10] <- "log2(fold_change)"

# Define UI ----
ui <- dashboardPage(
  dashboardHeader(title = em(h3(strong('Welcome to my shiny app'))), titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem(strong("Overview"), tabName = 'homepage', icon = icon('home')),
                     menuItem(strong('Data analysis results query'),tabName = 'gallery',icon = icon('dashboard'),
                              menuSubItem(p('Tables for differential gene expression results'), tabName = 'tab1'),
                              menuSubItem(p('Bar chart for showing the gene expression level'), tabName = 'tab2')),
                     menuItem(strong("Source code"),icon = icon("file-code-o"),href = "https://github.com/YijiaJiang/BIO316/blob/main/app_cw2.R")
                   )),
  dashboardBody(tabItems(
    tabItem(tabName = 'homepage',
            fluidRow(
              box(title = "Overview",solidHeader = TRUE,width = 10,
                  collapsible = FALSE,status = "primary",
                  dataTableOutput("overview",height = 500)),
              box(title ="Selection box", solidHeader = TRUE, width =2,
                  collapsed = FALSE,status="primary",
                  checkboxGroupInput("show_vars", "Columns in data to show:",names(wholedata), selected = names(wholedata)),
              downloadButton("downloadData", "Download data")))),
    tabItem(tabName = 'tab1',
            fluidRow(
              box(title = "Table",solidHeader = TRUE,width = 8,
                  collapsible = TRUE,status = "primary",
                  dataTableOutput("table",height = 500)),
              box(title = "Variable input",solidHeader = TRUE,width = 4,
                  collapsible = FALSE,status = "primary",
                  textInput("geneID1", "Please enter the gene id:", "A1BG")))),
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
  
  data <- wholedata[, c(2, 4, 5, 6, 8, 9, 10, 12, 14)]
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
    datatable(tabledata,height = 500,options = list(pageLength = 10),rownames = FALSE)
  })
  
  
  output$overview <- renderDataTable({
    datatable(wholedata[, input$show_vars],height = 500,rownames = FALSE,width = 250,
              options = list(pageLength = 10,orderClasses = TRUE))
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(wholedata, file, row.names = FALSE)
    }
  )
}


# Run the app ----
shinyApp(ui = ui, server = server)

