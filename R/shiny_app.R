

# Uploading and parsing the file.
library(shiny)

ui_upload = sidebarLayout(
  sidebarPanel(
    fileInput("file", "data", buttonLabel = "Upload file")
  ), # ending of sidebarPanel
  mainPanel(
    h3("TALIS 2024 - Within school sampling selection - Beta"),
    tableOutput("preview1")
  )
) # ending of sidebarLayout


# Download the file.

ui_download = fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block"))
) # ending of fluidRow

# assembled into a single fluidPage();

ui = fluidPage(ui_upload,ui_download)

# ----------------------------------------------

server = function(input,output,session){
  # Upload -------------------------------------
  data = reactive({
    req(input$file)
    
    ext = tools::file_ext(input$file$name)
    switch(ext,
           xlsx = readxl::read_xlsx(input$file$datapath),
           validate("Invalid file; Please upload a .xlsx file"))
  })
  output$preview1 = renderTable(head(data()))
  
  # Clean step --------------------------------
  
  transform = reactive({
    db = data()
    
    if(1 == 1){
      df = db[8:15,]
    }
    df
  })
  
  output$preview2 = renderTable(transform())
  
  # Download -----------------------------------
  
  output$download = downloadHandler(
    filename = function(){paste0(tools::file_path_sans_ext(input$file$name), ".xlsx")
    },
    content = function(file){
      writexl::write_xlsx(transform(), path = file)
    }
  )
}
