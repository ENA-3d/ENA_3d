stats_module = function(input,output,session,rv_data,config,state){
  # sample_data_files = list.files(config$sample_data_path)
  # sample_data_files = c('Select a sample dataset',sample_data_files)
  # updateSelectInput(session, "sample_data", choices = sample_data_files,selected = sample_data_files[1])
  
  observeEvent({
    input$sample_data
    input$x
    input$y
    input$z
    },{
    # if(input$sample_data == '' || is.null(input$sample_data)){
    #   print('not selecting a dataset')
    #   return("")
    # }
    # # 
    #   print(paste0('select:',input$sample_data))
    #   file_path = file.path(config$sample_data_path,input$sample_data)
    #   rv_data$initialized<- FALSE
    #   load_ena_data(input,output,session,file_path,rv_data,state)
    # if(input$)
    if(length(state$ena_obj)==0){
      return("")
    }
    if(input$x == '' || input$y=='' || input$z ==''){
      return("")
    }
    axis_name = 'MR1'
    stat_box_id = 'stats_box_x_axis'
    render_stats_box(input$x,'stats_box_x_axis')
    render_stats_box(input$y,'stats_box_y_axis')
    render_stats_box(input$z,'stats_box_z_axis')
    
    
    
    
    #output$stats_box_x_axis-effect_size
    #output$stats_box_x_axis-p_value
    #output$main_app-stats_box_x_axis-test_type_value
  })
  render_stats_box <- function(axis_name,stat_box_id){
    if(length(state$ena_obj$points)==0){
      print('ena object not ready for test')
      return("")
    }
    
    dims <- colnames(as.matrix(state$ena_obj$points))
    axis_index = which(dims == axis_name)
    
    g1_data <- as.matrix(state$ena_obj$points[state$ena_obj$points$groupid==1])
    g2_data <- as.matrix(state$ena_obj$points[state$ena_obj$points$groupid==2])
    g1_dim1 <- g1_data[,axis_index]
    g2_dim1 <- g2_data[,axis_index]
    
    # browser()
    
    t_test_dim1 <- t.test(g1_dim1,g2_dim1)
    
    t_test_dim1_effect_size <- rENA::fun_cohens.d(g1_dim1,g2_dim1)
    t_test_dim1_p_value <- t_test_dim1$p.value
    t_test_dim1_statistic  <- t_test_dim1$statistic
    t_test_dim1_df <- t_test_dim1$parameter
    t_test_dim1_conf <-  t_test_dim1$conf.int
    t_test_dim1_conf_level <-  attr(t_test_dim1_conf,which = 'conf.level')
    
    t_test_dim1_effect_size <- round(t_test_dim1_effect_size,4)
    t_test_dim1_p_value <- round(t_test_dim1_p_value,4)
    t_test_dim1_statistic <- round(t_test_dim1_statistic,4)
    t_test_dim1_df <- round(t_test_dim1_df,4)
    
    output[[paste0(stat_box_id,"-",'effect_size')]] <- renderText(paste0(t_test_dim1_effect_size))
    output[[paste0(stat_box_id,"-",'p_value')]] <-  renderText(paste0(t_test_dim1_p_value))
    
    output[[paste0(stat_box_id,"-",'test_type')]] <- renderText(sprintf('%s (%1.4f):','t',t_test_dim1_df))
    output[[paste0(stat_box_id,"-",'test_type_value')]] <- renderText(paste0(t_test_dim1_statistic))
    output[[paste0(stat_box_id,"-",'df_value')]] <- renderText(paste0(t_test_dim1_df))
    output[[paste0(stat_box_id,"-",'conf')]] <- renderText(sprintf('%f %f',
                                                                   t_test_dim1_conf[1],
                                                                   t_test_dim1_conf[2]))
    
    output[[paste0(stat_box_id,"-",'conf_level')]] <- renderText(sprintf('%1.0f  percent confidence interval:',
                                                                                          t_test_dim1_conf_level*100))
    output[[paste0(stat_box_id,"-",'axis_name')]] <- renderText(axis_name)
    df <- data.frame(
      Statistic = c("Mean", "Std.", "N"),
      group1 = c(mean(g1_dim1), sd(g1_dim1), length(g1_dim1)),
      group2 = c(mean(g2_dim1), sd(g2_dim1), length(g2_dim1))
    )
    new_column_names <- c("", "1", "2")
    names(df) <- new_column_names
    print(df)
    output[[paste0(stat_box_id,"-",'data_table')]] <- renderTable(df)
  }
}