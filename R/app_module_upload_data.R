upload_data <- function(input,output,session,rv_data){
  
  load_ena_data <- function(file_path){
    print('load ena data')
    
    env_ena_data <- load(file=file_path)
    
    rv_data$ena_obj = get(env_ena_data)
    
    # Find all Dimensions (MR1, SVD2, SVD3 ...)
    dims <- list()
    for(i in colnames(rv_data$ena_obj$points)){
      if(i %!in% colnames(rv_data$ena_obj$meta.data)){
        dims=append(dims,i)
      }
    }
    print('update group var')
    rv_data$ena_groupVar <- get_ena_group_var(rv_data$ena_obj)
    rv_data$ena_groups <- unique(rv_data$ena_obj$points[,get(rv_data$ena_groupVar[1])])
    
    print(paste0('rv_data$ena_groupVar:',typeof(rv_data$ena_groupVar)))
    print(paste0('rv_data$ena_groupsL',typeof(rv_data$ena_groups)))
    
    dim_choices <- unique(dims)
    updateSelectInput(session, "x", choices = dim_choices, selected = dim_choices[1])
    updateSelectInput(session, "y", choices = dim_choices, selected = dim_choices[2])
    updateSelectInput(session, "z", choices = dim_choices, selected = dim_choices[3])
    
    # update groups
    updateSelectInput(session, "change_group_1", choices = rv_data$ena_groups, selected = rv_data$ena_groups[1])
    updateSelectInput(session, "change_group_2", choices = rv_data$ena_groups, selected = rv_data$ena_groups[1])
    # update unit selection
    updateSelectInput(session, "group_change_var", choices = rv_data$ena_groupVar, selected = rv_data$ena_groupVar[1])
    
    
    unit_slider_choices=sort(unique(rv_data$ena_obj$points[,get(rv_data$ena_groupVar[1])]))
    updateSliderTextInput(session=session,inputId='unit_change',choices = unit_slider_choices)
    
    print(paste0('choices:',rv_data$ena_groupVar))
    # print(paste0(' updateSlider choices:',a))
    
    shinyjs::show("ena_points_plot")
    
    # initialized(TRUE)
    rv_data$initialized<- TRUE
  }
  
  # Upload file and load the ena obj in the environment
  observeEvent(input$ena_data_file,{
    print(paste0('Receive input file',input$ena_data_file))
    file <- input$ena_data_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(
      need(ext == "Rdata" || ext == "RData", "Please upload a Rdata file")
    )
    load_ena_data(file$datapath)
    
  })
  

}