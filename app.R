library(shiny)
library(readxl)
library(purrr)
library(dplyr)
library(writexl)

ui <- fluidPage(
  titlePanel("FGD Raw File Processing"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload the Excel File", accept = c(".xlsx"))
    ),
    mainPanel(
      downloadButton("download", "Download Processed File")
    )
  )
)

server <- function(input, output) {
  output$download <- downloadHandler(
    filename = function() {
      paste(ver_no(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(df1(), file)
      
    }
  )
  
  ver_no <- reactive({
    req(input$file)
    excel_path <- input$file$datapath
    sheet_names <- excel_sheets(excel_path)
    sheet_names[1]
  })
  
  df1 <- reactive({
    req(input$file)
    excel_path <- input$file$datapath
    sheet_names <- excel_sheets(excel_path)
    sheet_names <- sheet_names[-1]
    
    df <- read_excel(excel_path, sheet = sheet_names[1])
    
    index_column <- which(names(df) == "_index") - 1
    df1 <- df %>%
      select(all_of(1:index_column))
    
    for(rost in 2:length(sheet_names)){
      df2 <- read_excel(excel_path, sheet = sheet_names[rost])
      df2 <- df2[,-1]
      index_column <- which(names(df2) == "_index") - 1
      df3 <- df2 %>%
        select(all_of(1:index_column))
      df1 = cbind(df1, df3)
    }
    df1
  })
}

shinyApp(ui = ui, server = server)
