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
    
    
    group_colors_list <- color_list[1:length(rv_data$ena_groups)]
    groups <- rv_data$ena_groups
    group_colors <- cbind(group_colors_list,groups)
    colnames(group_colors) <- c('color','group')
    rv_data$group_colors<-group_colors
    
    output$group_colors_container <- renderUI({
      n = length(rv_data$ena_groups)
      checkboxGroupInput(session$ns("select_group"), "Choose Group:",
                         choiceNames = rv_data$ena_groups,
                         choiceValues = rv_data$ena_groups,
                         selected=rv_data$ena_groups
                         
      )
    })
    output$network_groups_container <- renderUI({
      groups <- rv_data$ena_groups
      n <- length(groups)
      
      group_selectors<-list()
      group_selector_info_list <- list()
      for(i in 1:n){
        group_color <- get_group_color(group_colors,'group',groups[i])
        
        
        button_id <- paste0("group-", groups[i],'-btn')
        points_toggle_id<-paste0("group-", groups[i],'-points-btn')
        color_selector_id<-paste0("group-", groups[i],'-color-selector')
        show_mean_btn_id<-paste0("group-", groups[i],'-show-mean-btn')
        show_conf_int_btn_id<-paste0("group-", groups[i],'-show-conf-int-btn')
        
        group_selector_info = c(button_id=button_id,
                                points_toggle_id=points_toggle_id,
                                color_selector_id=color_selector_id,
                                show_mean_btn_id=show_mean_btn_id,
                                show_conf_int_btn_id=show_conf_int_btn_id,
                                group_name = groups[i]
        )
        
        group_selector_info_list[[length(group_selector_info_list)+ 1]] <- group_selector_info
        
        
        button_id<-session$ns(button_id)
        points_toggle_id <- session$ns(points_toggle_id)
        color_selector_id <- session$ns(color_selector_id)
        show_mean_btn_id <- session$ns(show_mean_btn_id)
        show_conf_int_btn_id <- session$ns(show_conf_int_btn_id)
        
        group_selector <- group_selector_ui(button_id=button_id,
                                points_toggle_id=points_toggle_id,
                                color_selector_id=color_selector_id,
                                show_mean_btn_id=show_mean_btn_id,
                                show_conf_int_btn_id=show_conf_int_btn_id,
                                group_name = groups[i],
                                group_color=group_color)
        #browser()

        #rv_data$group_selectors[[length(rv_data$group_selectors) + 1]] <- group_selector
        group_selectors[[length(group_selectors)+ 1]] <- group_selector
        
      }
      #browser()
      rv_data$group_selectors<-group_selector_info_list
      #rv_data$group_selectors<-group_selectors
      
      # group_selectors <- lapply(1:n, function(i) {
      #   group_color <- get_group_color(group_colors,'group',groups[i])
      #   button_id <- session$ns(paste0("group-", groups[i],'-btn'))
      #   points_toggle_id <- session$ns(paste0("group-", groups[i],'-points-btn'))
      #   color_selector_id <- session$ns(paste0("group-", groups[i],'-color-selector'))
      #   show_mean_btn_id <- session$ns(paste0("group-", groups[i],'-show-mean-btn'))
      #   show_conf_int_btn_id <- session$ns(paste0("group-", groups[i],'-show-conf-int-btn'))
      #   
      #   ui <- group_selector_ui(button_id=button_id,
      #                           points_toggle_id=points_toggle_id,
      #                           color_selector_id=color_selector_id,
      #                           show_mean_btn_id=show_mean_btn_id,
      #                           show_conf_int_btn_id=show_conf_int_btn_id,
      #                           group_name = groups[i],
      #                           group_color=group_color)
      #   #browser()
      #   group_selector = c(button_id=button_id,
      #                      points_toggle_id=points_toggle_id,
      #                      color_selector_id=color_selector_id,
      #                      show_mean_btn_id=show_mean_btn_id,
      #                      show_conf_int_btn_id=show_conf_int_btn_id
      #                      )
      #   #rv_data$group_selectors[[length(rv_data$group_selectors) + 1]] <- group_selector
      #   return(ui)
      # })
      do.call(tagList, group_selectors)
    })
    
    
    # shinyjs::show("overall")
    
    # initialized(TRUE)
    rv_data$initialized<- TRUE
    state$is_app_initialized <- TRUE
  }
  