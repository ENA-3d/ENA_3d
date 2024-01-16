source('./app_module_load_dataset.R')
upload_data <- function(input,output,session,rv_data,state){
  
  # load_ena_data <- function(file_path){
  #   print('load ena data')
  #   
  #   env_ena_data <- load(file=file_path)
  #   
  #   state$ena_obj = get(env_ena_data)
  #   
  #   # Find all Dimensions (MR1, SVD2, SVD3 ...)
  #   dims <- list()
  #   for(i in colnames(state$ena_obj$points)){
  #     if(i %!in% colnames(state$ena_obj$meta.data)){ena_points_plot
  #       dims=append(dims,i)
  #     }
  #   }
  #   print('update group var')
  #   rv_data$ena_groupVar <- get_ena_group_var(state$ena_obj)
  #   rv_data$ena_groups <- unique(state$ena_obj$points[,get(rv_data$ena_groupVar[1])])
  #   
  #   print(paste0('rv_data$ena_groupVar:',typeof(rv_data$ena_groupVar)))
  #   print(paste0('rv_data$ena_groupsL',typeof(rv_data$ena_groups)))
  #   
  #   dim_choices <- unique(dims)
  #   updateSelectInput(session, "x", choices = dim_choices, selected = dim_choices[1])
  #   updateSelectInput(session, "y", choices = dim_choices, selected = dim_choices[2])
  #   updateSelectInput(session, "z", choices = dim_choices, selected = dim_choices[3])
  #   
  #   # update groups
  #   updateSelectInput(session, "change_group_1", choices = rv_data$ena_groups, selected = rv_data$ena_groups[1])
  #   updateSelectInput(session, "change_group_2", choices = rv_data$ena_groups, selected = rv_data$ena_groups[1])
  #   # update unit selection
  #   updateSelectInput(session, "group_change_var", choices = rv_data$ena_groupVar, selected = rv_data$ena_groupVar[1])
  #   
  #   
  #   unit_slider_choices=sort(unique(state$ena_obj$points[,get(rv_data$ena_groupVar[1])]))
  #   updateSliderTextInput(session=session,inputId='unit_change',choices = unit_slider_choices)
  #   
  #   print(paste0('choices:',rv_data$ena_groupVar))
  #   # print(paste0(' updateSlider choices:',a))
  #   
  #   shinyjs::show("ena_points_plot")
  #   
  #   # initialized(TRUE)
  #   rv_data$initialized<- TRUE
  # }
  
  intialize_rv_data = function(){
    rv_data$myList = list()
    rv_data$unit_group_change_plots=list()
    rv_data$current_unit_change_plot_camera=list()
    rv_data$ena_groups=list()
    rv_data$ena_groupVar=list()
    state$ena_obj=list()
    rv_data$ena_points_plot_ready=FALSE
    rv_data$initialized=FALSE
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
    
    intialize_rv_data()
    # load_ena_data(file$datapath)
    load_ena_data(input,output,session,file$datapath,rv_data,state)
  })
  

}