load_ena_data <- function(input,output,session,file_path,rv_data,state){
    print('load ena data')
    
  
    env_ena_data <- load(file=file_path)
    
    state$ena_obj = get(env_ena_data)
    
    # Find all Dimensions (MR1, SVD2, SVD3 ...)
    dims <- list()
    for(i in colnames(state$ena_obj$points)){
      if(i %!in% colnames(state$ena_obj$meta.data)){
        dims=append(dims,i)
      }
    }
    print('update group var')
    rv_data$ena_groupVar <- get_ena_group_var(state$ena_obj)
  
    rv_data$ena_groups <- unique(state$ena_obj$points[,get(rv_data$ena_groupVar[1])])
    
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
    
    
    updateSelectInput(session, "stats_group1", choices = rv_data$ena_groups, selected = rv_data$ena_groups[1])
    updateSelectInput(session, "stats_group2", choices = rv_data$ena_groups, selected = rv_data$ena_groups[1])
    
    unit_slider_choices=sort(unique(state$ena_obj$points[,get(rv_data$ena_groupVar[1])]))
    updateSliderTextInput(session=session,inputId='unit_change',choices = unit_slider_choices)
    
    updateSelectInput(session, "compare_group_1", choices = rv_data$ena_groups, selected = rv_data$ena_groups[1])
    updateSelectInput(session, "compare_group_2", choices = rv_data$ena_groups, selected = rv_data$ena_groups[1])
    
    print(paste0('choices:',rv_data$ena_groupVar))
    # print(paste0(' updateSlider choices:',a))
    
    output$group_colors_container <- renderUI({
      n = length(rv_data$ena_groups)
      checkboxGroupInput(session$ns("select_group"), "Choose Group:",
                         choiceNames = rv_data$ena_groups,
                         choiceValues = rv_data$ena_groups,
                         selected=rv_data$ena_groups
                         
      )
    })
    
    # shinyjs::show("overall")
    
    # initialized(TRUE)
    rv_data$initialized<- TRUE
    state$is_app_initialized <- TRUE
  }
  