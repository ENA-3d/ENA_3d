source('./group_stats.R')
stats_module = function(input,output,session,rv_data,config,state){
  # sample_data_files = list.files(config$sample_data_path)
  # sample_data_files = c('Select a sample dataset',sample_data_files)
  # updateSelectInput(session, "sample_data", choices = sample_data_files,selected = sample_data_files[1])
  
  observeEvent({
    input$sample_data
    input$x
    input$y
    input$z
    input$stats_group1
    input$stats_group2
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
    
    if(length(state$ena_obj$points)==0 || length(rv_data$ena_groupVar)<=0){
      print('ena object not ready for test')
      return("")
    }
    # browser()

    if(length(rv_data$ena_groupVar)>1){
      gvar = rv_data$ena_groupVar[1]
    }else{
      gvar = rv_data$ena_groupVar
    }
    g1_data <- as.matrix(state$ena_obj$points[state$ena_obj$points[[gvar]]==input$stats_group1])
    g2_data <- as.matrix(state$ena_obj$points[state$ena_obj$points[[gvar]]==input$stats_group2])

    
    render_stats_box_t_test(input$x,'stats_box_x_axis',g1_data,g2_data)
    render_stats_box_t_test(input$y,'stats_box_y_axis',g1_data,g2_data)
    render_stats_box_t_test(input$z,'stats_box_z_axis',g1_data,g2_data)
    
    render_stats_box_wilcox(input$x,'stats_box_x_axis_wilcox_unpaired',g1_data,g2_data)
    render_stats_box_wilcox(input$y,'stats_box_y_axis_wilcox_unpaired',g1_data,g2_data)
    render_stats_box_wilcox(input$z,'stats_box_z_axis_wilcox_unpaired',g1_data,g2_data)
    
    render_stats_box_wilcox_paired(input$x,'stats_box_x_axis_wilcox_paired',g1_data,g2_data)
    render_stats_box_wilcox_paired(input$y,'stats_box_y_axis_wilcox_paired',g1_data,g2_data)
    render_stats_box_wilcox_paired(input$z,'stats_box_z_axis_wilcox_paired',g1_data,g2_data)
    
    # render_stats_box(input$x,'stats_box_x_axis')
    # render_stats_box(input$y,'stats_box_y_axis')
    # render_stats_box(input$z,'stats_box_z_axis')
    
    
    #output$stats_box_x_axis-effect_size
    #output$stats_box_x_axis-p_value
    #output$main_app-stats_box_x_axis-test_type_value
  })
  render_stats_box_t_test <- function(axis_name,stat_box_id,g1_data,g2_data){
    # browser()
    # if(length(state$ena_obj$points)==0 || length(rv_data$ena_groupVar)<=0){
    #   print('ena object not ready for test')
    #   return("")
    # }
    # 
    # dims <- colnames(as.matrix(state$ena_obj$points))
    # axis_index = which(dims == axis_name)
    # 
    # g1_data <- as.matrix(state$ena_obj$points[state$ena_obj$points[[rv_data$ena_groupVar]]==input$stats_group1])
    # g2_data <- as.matrix(state$ena_obj$points[state$ena_obj$points[[rv_data$ena_groupVar]]==input$stats_group2])
    # g1_dim <- g1_data[,axis_index]
    # g2_dim <- g2_data[,axis_index]
    
    # browser()
    dims <- colnames(as.matrix(state$ena_obj$points))
    axis_index = which(dims == axis_name)
    
    g1_dim <- g1_data[,axis_index]
    g2_dim <- g2_data[,axis_index]
    
    t_test_dim1 <- t.test(g1_dim,g2_dim)
    
    effect_size <- rENA::fun_cohens.d(g1_dim,g2_dim)
    p_value <- t_test_dim1$p.value
    t_statistic  <- t_test_dim1$statistic
    t_df <- t_test_dim1$parameter
    conf <-  t_test_dim1$conf.int
    conf_level <-  attr(conf,which = 'conf.level')
    
    effect_size <- round(effect_size,4)
    p_value <- round(p_value,4)
    t_statistic <- round(t_statistic,4)
    t_df <- round(t_df,4)
    
    test_stats_type = sprintf('%s (%1.4f)','t',t_df)
    
    df <- data.frame(
      Statistic = c("Mean", "Std.", "N"),
      group1 = c(mean(g1_dim), sd(g1_dim), length(g1_dim)),
      group2 = c(mean(g2_dim), sd(g2_dim), length(g2_dim))
    )
    new_column_names <- c("", input$stats_group1, input$stats_group2)
    names(df) <- new_column_names
    # print(df)
    # output[[paste0(stat_box_id,"-",'data_table')]] <- renderTable(df)
    
    render_stats_box(axis_name,stat_box_id,effect_size = effect_size,p_value = p_value,
                     statistic = t_statistic,df = t_df,conf = conf,conf_level = conf_level,
                     statistic_dataframe = df, test_stats_type =test_stats_type)
  }
  
  render_stats_box <-function(axis_name,stat_box_id,effect_size,p_value,statistic,df,conf,conf_level,statistic_dataframe,test_stats_type){
    output[[paste0(stat_box_id,"-",'effect_size')]] <- renderText(paste0(effect_size))
    output[[paste0(stat_box_id,"-",'p_value')]] <-  renderText(paste0(p_value))
    output[[paste0(stat_box_id,"-",'test_type')]] <- renderText(test_stats_type)
    output[[paste0(stat_box_id,"-",'test_type_value')]] <- renderText(paste0(statistic))
    
    if(!is.null(conf) &&  !is.null(conf_level)){
      output[[paste0(stat_box_id,"-",'conf')]] <- renderText(sprintf('%f %f',
                                                                     conf[1],
                                                                     conf[2]))
      output[[paste0(stat_box_id,"-",'conf_level')]] <- renderText(sprintf('%1.0f  percent confidence interval:',
                                                                           conf_level*100))
    }

    output[[paste0(stat_box_id,"-",'axis_name')]] <- renderText(axis_name)

    output[[paste0(stat_box_id,"-",'data_table')]] <- renderTable(statistic_dataframe)
  }
  
  render_stats_box_wilcox_paired <- function(axis_name,stat_box_id,g1_data,g2_data){
    # if(length(state$ena_obj$points)==0){
    #   print('ena object not ready for test')
    #   return("")
    # }
    # 
    # dims <- colnames(as.matrix(state$ena_obj$points))
    # axis_index = which(dims == axis_name)
    # 
    # g1_data <- as.matrix(state$ena_obj$points[state$ena_obj$points$groupid==1])
    # g2_data <- as.matrix(state$ena_obj$points[state$ena_obj$points$groupid==2])
    # g1_dim <- g1_data[,axis_index]
    # g2_dim <- g2_data[,axis_index]
    
    # browser()
    tryCatch({


      dims <- colnames(as.matrix(state$ena_obj$points))
      axis_index = which(dims == axis_name)
      
      g1_dim <- g1_data[,axis_index]
      g2_dim <- g2_data[,axis_index]
    test_result <- coin::wilcoxsign_test(g1_dim ~ g2_dim, distribution = "exact",
                                         alternative = "greater", zero.method = "Wilcoxon")
    
    
    
    

    
    p_value <- coin::pvalue(test_result)
    z_statistic  <- coin::statistic(test_result)
    
    N <- 2*length(g1_dim)
    effect_size <- z_statistic/sqrt(N)
    
    
    effect_size <- round(effect_size,4)
    p_value <- round(p_value,4)
    z_statistic <- round(z_statistic,4)
    
    test_stats_type = 'Z'
    
    df <- data.frame(
      Statistic = c("Median", "N"),
      group1 = c(median(g1_dim), length(g1_dim)),
      group2 = c(median(g2_dim), length(g2_dim))
    )
    new_column_names <- c("", input$stats_group1, input$stats_group2)
    names(df) <- new_column_names
    # print(df)
    # output[[paste0(stat_box_id,"-",'data_table')]] <- renderTable(df)
    
    render_stats_box(axis_name,stat_box_id,effect_size = effect_size,p_value = p_value,
                     statistic = z_statistic,df = '',conf = NULL,conf_level = NULL,
                     statistic_dataframe = df, test_stats_type =test_stats_type)
    }, error=function(e) {
      print(paste("in err handler2\n"))
      print(e)
    }, warning=function(w) {
      print(paste("in warn handler2\n"))
      print(w)
    })
  }
  
  render_stats_box_wilcox <- function(axis_name,stat_box_id,g1_data,g2_data){
    # if(length(state$ena_obj$points)==0){
    #   print('ena object not ready for test')
    #   return("")
    # }
    # 
    # dims <- colnames(as.matrix(state$ena_obj$points))
    # axis_index = which(dims == axis_name)
    # 
    # g1_data <- as.matrix(state$ena_obj$points[state$ena_obj$points$groupid==1])
    # g2_data <- as.matrix(state$ena_obj$points[state$ena_obj$points$groupid==2])
    # g1_dim <- g1_data[,axis_index]
    # g2_dim <- g2_data[,axis_index]
    
    # browser()
    dims <- colnames(as.matrix(state$ena_obj$points))
    axis_index = which(dims == axis_name)
    
    g1_dim <- g1_data[,axis_index]
    g2_dim <- g2_data[,axis_index]
    
    test_result <- wilcox.test(g1_dim,g2_dim)
    
    p_value <- test_result$p.value
    w_statistic  <- test_result$statistic
    effect_size <- nonparam.effect(w_statistic,length(g1_dim),length(g2_dim))
    
    effect_size <- round(effect_size,4)
    p_value <- round(p_value,4)
    w_statistic <- round(w_statistic,4)

    test_stats_type = 'W'
    
    df <- data.frame(
      Statistic = c("Median", "N"),
      group1 = c(median(g1_dim), length(g1_dim)),
      group2 = c(median(g2_dim), length(g2_dim))
    )
    new_column_names <- c("",input$stats_group1, input$stats_group2)
    names(df) <- new_column_names
    # print(df)
    # output[[paste0(stat_box_id,"-",'data_table')]] <- renderTable(df)
    
    render_stats_box(axis_name,stat_box_id,effect_size = effect_size,p_value = p_value,
                     statistic = w_statistic,df = '',conf = NULL,conf_level = NULL,
                     statistic_dataframe = df, test_stats_type =test_stats_type)
  }
  

  
}