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
