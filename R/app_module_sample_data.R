source('app_module_load_dataset.R')
sample_data_load_and_select = function(input,output,session,rv_data,config){
  sample_data_files = list.files(config$sample_data_path)
  sample_data_files = c('Select a sample dataset',sample_data_files)
  updateSelectInput(session, "sample_data", choices = sample_data_files,selected = sample_data_files[1])
  
  observeEvent(input$sample_data,{
    if(input$sample_data == 'Select a sample dataset' || input$sample_data == '' || is.null(input$sample_data)){
      print('not selecting a dataset')
      return("")
    }
    
      print(paste0('select:',input$sample_data))
      file_path = file.path(config$sample_data_path,input$sample_data)
      load_ena_data(input,output,session,file_path,rv_data)
    
    

  })
}
