source('./app_utils.R')
ena_network_plot_output <-  function(input, output, session,
                                        data,
                                        state,
                                        scaled_points,
                                        scaled_nodes,
                                        group_var=NULL,
                                        camera=NULL
) {
  # print('module server go with id')
  # print(id)
  x_axis <- reactive({
    tilde_var_or_null(input$x)
  })
  y_axis <- reactive({
    tilde_var_or_null(input$y)
  })
  z_axis <- reactive({
    tilde_var_or_null(input$z)
  })

  camera = reactive({
    pos = input$camera_position
    print(pos)
    if(pos =='default'){
      camera = list(eye=list(x=1.25, y=1.25, z=1.25))
    }else if(pos =='x_y'){
      camera = list(eye=list(x=0, y=0, z=2.5),up=list(x=0,y=1,z=0))
    }else if(pos == 'x_z'){
      camera = list(eye=list(x=0., y=-2.5, z=0.),up=list(x=0,y=0,z=1))
    }else if(pos =='y_z'){
      camera = list(eye=list(x=2.5, y=0., z=0.),up=list(x=0,y=0,z=1))
    }else if(pos =='y_x'){
      camera = list(eye=list(x=0, y=0, z=-2.5),up=list(x=1,y=0,z=0))
    }else if(pos =='z_x'){
      camera = list(eye=list(x=0, y=2.5, z=0),up=list(x=1,y=0,z=0))
    }else if(pos =='z_y'){
      camera = list(eye=list(x=-2.5, y=0, z=0),up=list(x=0,y=1,z=0))
    }
    # list(eye=camera_eye(),up=list(x=0,y=1,z=0))
  })
  tilde_group_var_or_null = reactive({
    if(grepl(' ', data$ena_groupVar[1])){
      as.formula(paste('~',sprintf("`%s`",data$ena_groupVar[1])))
    }else{
      tilde_var_or_null(data$ena_groupVar[1])
    }
    
  })
  
  get_select_group= reactive({
    if(data$model_tab_clicked ==TRUE){
      input$select_group
    }else{
      data$ena_groups
    }
  })%>% debounce(5000)
  
  # ena_lines_mean_in_groups = reactive({
  #   get_mean_group_lineweights_in_groups(state$ena_obj,data$ena_groupVar[1],get_select_group())
  # })
  
  get_colors = reactive({
    print(scaled_points())
    print(data$ena_obj)
    print(state$is_app_initialized)
    
    num_groups = length(unique(scaled_points()[,data$ena_groupVar[1]]))
    
    if(num_groups < length(state$color_list)){
      state$color_list[1:num_groups]
    }else{
      randomcoloR::distinctColorPalette(num_groups)
      
    }
  })
  get_secondary_groups = reactive({
    if(!is.na(data$ena_groupVar[2])){
      print(state$ena_obj$points[,data$ena_groupVar[2]])
      state$ena_obj$points[,data$ena_groupVar[2]]
    }else{
      c()
    }
  })
  add_3d_axis_based_on_user_selection = function(plot){
    if(input$show_x_axis_arrow){
      plot<-add_x_3d_axis(plot)
    }
    if(input$show_y_axis_arrow){
      plot<-add_y_3d_axis(plot)
    }
    if(input$show_z_axis_arrow){
      plot<-add_z_3d_axis(plot)
    }
    
    # plot <- layout(plot,title='X-Y',scene= list(camera=list(eye=list(x=0., y=0., z=-2.5))))
    plot
    
    
  }
  scale_to<- function(enaset,scale.to,axispadding=1.2){
    if (is.list(scale.to)) {
      max.axis = max(abs(as.matrix(enaset$points)))*axispadding
      if(is.null(scale.to$x)) {
        axis.range.x = c(-max.axis, max.axis)
      }
      else {
        axis.range.x = scale.to$x
      }
      if(is.null(scale.to$y)) {
        axis.range.y = c(-max.axis, max.axis)
      }
      else {
        axis.range.y = scale.to$y
      }
      if(is.null(scale.to$z)) {
        axis.range.z = c(-max.axis, max.axis)
      }
      else {
        axis.range.z = scale.to$z
      }
    }
    else {
      if(is.character(scale.to) && scale.to == "points") {
        max.axis = max(abs(as.matrix(enaset$points)))*axispadding
      }
      else if (is.numeric(scale.to)) {
        max.axis = tail(scale.to, 1)
      }
      else {
        max.axis = max(abs(as.matrix(enaset$rotation$nodes)))*axispadding;
      }
      axis.range.x = axis.range.y = axis.range.z= c(-max.axis, max.axis)
    }
    axis_range = list(axis.range.x=axis.range.x,
           axis.range.y= axis.range.y,
           axis.range.z=axis.range.z)
    return(axis_range)
  }
  add_mean_to_plot <- function(plot,
                               all_points,
                               group_name,
                               group_var,
                               show_mean,
                               show_conf_int,
                               color){
    #browser()
    conf <-'none'
    if(show_conf_int){
      conf <- 'box'
    }
    
    if(show_mean){
      points <- get_points_with_group(all_points,group_var,group_name)
      points <-remove_meta_data(points)
      
      means = sapply(points,'mean')
      #check confidence interval
      #browser()
      # if(sd(points[,input$x])==0 || sd(points[,input$y])==0||sd(points[,input$z])==0 ||
      #    is.na(sd(points[,input$x])) || is.na(sd(points[,input$y]))||is.na(sd(points[,input$z]))){
      #   conf <- 'none'
      # }
      if(!validate_confidence_interval(points,input$x,input$y,input$z)){
        conf <- 'none'
      }
      plot <- ena_plot_group_3d(plot,points = points,
                                colors=color,
                                confidence.interval=conf,
                                x_axis = input$x,
                                y_axis = input$y,
                                z_axis = input$z,
                                group_name=group_name)
    }
    
    return(plot)
  }
  validate_confidence_interval <- function(points,x,y,z){
    return(!(sd(points[,x])==0 || sd(points[,y])==0||sd(points[,z])==0 ||
               is.na(sd(points[,x])) || is.na(sd(points[,y]))||is.na(sd(points[,z]))))
  }
  generate_plot = reactive({
    #browser()
    # req(initialized(),cancelOutput = TRUE)
    main_plot <- plot_ly()
    print('generate plot')
    req(data$initialized,cancelOutput = TRUE)
    req(state$is_app_initialized,cancelOutput = TRUE)
    req(!is.null(data$ena_obj),cancelOutput = TRUE)
    validate(
      need(input$x != '',FALSE),
      need(input$y != '',FALSE),
      need(input$z != '',FALSE),
    )
    if(state$render_network_plot() == FALSE){
      return(NULL)
    }
    if(length(data$group_selectors)==0 || is.null(data$group_selectors)){
      return(NULL)
    }
    for(i in 1:length(data$group_selectors)){
      if(data$group_selectors[[i]][['ready']] == FALSE){
        return(NULL)
      }
    }
    print('render network plot')
    # Create an empty plot
    #browser()
    # Add the first trace (from points_plot)
    my_points <- scaled_points()
    colname = data$ena_groupVar[1]
    
    # Fix the bug of not showing edges when the dataset is loaded and the user hasn't open the model page      
    print(data$model_tab_clicked)
    print(input$select_group)
    
    selected_groups <- get_select_group()
    filter_points <- my_points[which(my_points[[colname]] %in% selected_groups)]
    filter_points[,colname]<-as.character(as.data.frame(filter_points)[,colname])
    
    print('filter_points')
    print(filter_points)
    
    n <- length(data$ena_groups)
    for(i in 1:n){
      
      
      selected_group<-data$ena_groups[i]
      #browser()
      group_selector = data$group_selectors[[selected_group]]
      
      
      #browser()
      color <- get_group_color(data$group_colors,'group',selected_group)
      
      points_toggle_id <- input[[group_selector['points_toggle_id']]]
      if(is.null(points_toggle_id)){
        points_toggle_id==FALSE
      }
      
      if(points_toggle_id){
        filter_points <- my_points[which(my_points[[colname]] %in% selected_group)]
        
        main_plot <- add_trace(main_plot,
                               data = filter_points,
                               
                               x = x_axis(),
                               y = y_axis(),
                               z = z_axis(),
                               #color = tilde_group_var_or_null(),
                               
                               #text=get_secondary_groups(),
                               # test=get_secondary_groups(),
                               type = 'scatter3d',
                               mode = "markers",
                               text=~ENA_UNIT,
                               # name = "Points",
                               hovertemplate = "X: %{x}<br>Y: %{y}<br>Z: %{z}<br>Unit : %{text}<br>",
                               name = selected_group,
                               marker = list(
                                 size = 5,
                                 line = list(
                                   width = 0
                                 ),
                                 color=color
                                 # ,name = labels[i] #rownames(nodes)[i]
                               ))
      }
      
      is_mean_shown <- input[[group_selector['show_mean_btn_id']]]
      if(is.null(is_mean_shown)){
        is_mean_shown==FALSE
      }
      is_conf_int_shown <- input[[group_selector['show_conf_int_btn_id']]]
      if(is.null(is_conf_int_shown)){
        is_conf_int_shown==FALSE
      }
      #browser()
      main_plot<-add_mean_to_plot(main_plot,
                                  all_points = my_points,
                                  group_name = selected_group,
                                  group_var = colname,
                                  show_mean = is_mean_shown,
                                  show_conf_int = is_conf_int_shown,
                                  color = color)
      
    }
    
    selected_group_for_networks <- input$network_selector
    if(selected_group_for_networks!='No Network'){
      my_nodes <- scaled_nodes()
      # Add the second trace (from nodes_plot)
      main_plot <- add_trace(main_plot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
                             type = 'scatter3d', mode = "markers", name = "Codes",
                             marker = list(
                               color ='rgb(77,77,77)',
                               size = abs(my_nodes$weight),
                               line = list(
                                 width = 0
                               )
                               #,name = labels[i] #rownames(nodes)[i]
                             ))
      t <- list(
        family = "sans serif",
        size = 14,
        color = toRGB("grey50"))
      
      main_plot <-  add_text(main_plot,data=my_nodes,x = x_axis(), y = y_axis(), z = z_axis(),
                             text = ~code,
                             textfont=t,
                             textposition = "top right")

      # Customize the layout and appearance of the combined plot
      if(selected_group_for_networks %in% data$ena_groups){
        lineweights<-get_mean_group_lineweights_in_groups(state$ena_obj,data$ena_groupVar[1],selected_group_for_networks)
        color <- get_group_color(data$group_colors,'group',selected_group_for_networks)
      }else{
        #browser()
        lineweights<-get_mean_group_lineweights_in_groups(state$ena_obj,"ENA_UNIT",selected_group_for_networks)
        DT <- state$ena_obj$points[which(state$ena_obj$points$ENA_UNIT==selected_group_for_networks)]
        group <- as.data.frame(DT)[,data$ena_groupVar[1]]
        color <- get_group_color(data$group_colors,'group',group)
      }
      
      #browser()
      network <- build_network(scaled_nodes(),
                               network=lineweights,
                               adjacency.key=state$ena_obj$rotation$adjacency.key,
                               colors=c(color,color))

      main_plot <- plot_network(main_plot,
                                network,
                                legend.include.edges = F,
                                x_axis=input$x,
                                y_axis=input$y,
                                z_axis=input$z,
                                line_width = input$line_width
                                )
    }
    
    # my_nodes <- scaled_nodes()
    # # Add the second trace (from nodes_plot)
    # main_plot <- add_trace(main_plot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
    #                        type = 'scatter3d', mode = "markers", name = "Codes",
    #                        marker = list(
    #                          color ='rgb(77,77,77)',
    #                          size = abs(my_nodes$weight),
    #                          line = list(
    #                            width = 0
    #                          )
    #                          #,name = labels[i] #rownames(nodes)[i]
    #                        ))
    # t <- list(
    #   family = "sans serif",
    #   size = 14,
    #   color = toRGB("grey50"))
    # 
    # main_plot <-  add_text(main_plot,data=my_nodes,x = x_axis(), y = y_axis(), z = z_axis(),
    #                        text = ~code,
    #                        textfont=t,
    #                        textposition = "top right")
    # 
    # # Customize the layout and appearance of the combined plot
    # main_plot <- layout(main_plot,
    #                     scene = list(xaxis = list(title = input$x,showgrid=input$show_grid,zeroline=input$show_zeroline),
    #                                  yaxis = list(title = input$y,showgrid=input$show_grid,zeroline=input$show_zeroline),
    #                                  zaxis = list(title = input$z,showgrid=input$show_grid,zeroline=input$show_zeroline)),
    #                     showlegend = TRUE)
    # if(length(selected_groups) == 0){
    #   return(main_plot)
    # }
    # browser()
    # Generate Edges
    # mean_in_groups<-get_mean_group_lineweights_in_groups(state$ena_obj,data$ena_groupVar[1],selected_groups)
    # network <- build_network(scaled_nodes(),
    #                          network=mean_in_groups,
    #                          adjacency.key=state$ena_obj$rotation$adjacency.key)
    # 
    # main_plot <- plot_network(main_plot,
    #                           network,
    #                           legend.include.edges = F,
    #                           x_axis=input$x,
    #                           y_axis=input$y,
    #                           z_axis=input$z,
    #                           line_width = input$line_width)
    
    # if(!is.null(camera)){
    #   print('set cam')
    #   main_plot %>% layout(scene= list(camera=camera))
    # }
    # camera = list(
    #   eye=list(x=0., y=0., z=2.5)
    # )
    #print(camera())
    #browser()
    axis_range <- scale_to(state$ena_obj,scale.to = 'network',axispadding=1.2)
    axx <- list(
      nticks = 4,
      range = c(-10,10)
    )
    
    axy <- list(
      nticks = 4,
      range = c(-10,10)
    )
    
    axz <- list(
      nticks = 4,
      range = c(-10,10)
    )
    #browser()
    main_plot <- layout(main_plot,
                        title=input$camera_position,
                        scene= list(camera=camera(),xaxis=axx,yaxis=axy,zaxis=axz
                                    )
                                    
                        )
    
    main_plot
  })
  
  # reactive(generate_plot())
  # output$ena_points_plot <- renderPlotly({
  #   print('generate plot ena_points_plot')
  #   # generate_plot()
  #   plot_ly(data.frame(x=c(1,2,3),y=c(1,2,3)))
  # })
  output$ena_network_plot <- renderPlotly({
    comparison_plot <- generate_plot()

    comparison_plot <- add_3d_axis_based_on_user_selection(comparison_plot)

    print('new plot')
    event_register(comparison_plot, 'plotly_relayout')
    # click_data <- event_data("plotly_click", source = "ena_points_plot")
    #
    # if (!is.null(click_data)) {
    #   print(str(click_data))
    #   # idx <- click_data$pointNumber + 1
    #   # data[idx, "col"] <- "red"
    # }

    
    comparison_plot
   
  })
  
  # observeEvent(event_data(event = "plotly_relayout",source='plot_correlation'),{
  #   clicked <- event_data(event = "plotly_relayout",
  #                         source = "plot_correlation")
  #   if (!is.null(clicked)) {
  #     print(clicked)
  #   }
  # })
  
  observeEvent({data$group_selectors},{
    if(length(data$group_selectors)!=0){
      
      list_of_selectors<-list()
      n<-length(data$group_selectors)
      for(ii in 1:n) {
        
        local({
          i <- ii
          group_selector <- data$group_selectors[[i]]
          group_name <- group_selector[['group_name']]
          
          observeEvent(eventExpr = {
                        input[[group_selector[['color_selector_id']]]]
                        input[[group_selector[['points_toggle_id']]]]
                        input[[group_selector[['show_mean_btn_id']]]]
                        input[[group_selector[['show_conf_int_btn_id']]]]
            
                        },
                       handlerExpr = {
                         print(sprintf("You clicked btn number %s",group_name))
                         print(group_name)
                         # if(!is.null(input[[color_selector_id]])){
                         #   data$group_colors[which(data$group_colors[,'group']==group_name)]<-input[[color_selector_id]]
                         #   print(data$group_colors)
                         # }
                         # data$group_options[[group_name]][['color_selector_id']] <- input[[group_selector[['color_selector_id']]]]
                         # data$group_options[[group_name]][['points_toggle_id']] <-input[[group_selector[['points_toggle_id']]]]
                         # data$group_options[[group_name]][['show_mean_btn_id']] <-input[[group_selector[['show_mean_btn_id']]]]
                         # data$group_options[[group_name]][['show_conf_int_btn_id']] <-input[[group_selector[['show_conf_int_btn_id']]]]
                         #rv$group_colors[which(group_colors[,group_col]==group_name)]
                         #input[[]]
                         
                         if(!is.null(input[[group_selector[['color_selector_id']]]])&&
                            !is.null(input[[group_selector[['points_toggle_id']]]])&&
                            !is.null(input[[group_selector[['show_mean_btn_id']]]])&&
                            !is.null(input[[group_selector[['show_conf_int_btn_id']]]])){
                           
                           data$group_selectors[[i]][['ready']] <- TRUE
                         }
                       })
        })
      }
    }
  })
}