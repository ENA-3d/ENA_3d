source('./app_utils.R')
ena_comparison_plot_output <-  function(input, output, session,
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
    camera_eye<- reactive({
      pos = input$camera_position
      print(pos)
      if(pos =='default'){
        list(x=1.25, y=1.25, z=1.25)
      }else if(pos =='x_y'){
        list(x=0, y=0, z=2.5)
      }else if(pos == 'x_z'){
        list(x=0., y=-2.5, z=0.)
      }else if(pos =='y_z'){
        list(x=-2.5, y=0., z=0.)
      }
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
    
    # get_select_group= reactive({
    #   if(data$model_tab_clicked ==TRUE){
    #     input$select_group
    #   }else{
    #     data$ena_groups
    #   }
    # })%>% debounce(5000)
    get_groups = reactive({
      data$ena_groups
    })
    ena_lines_mean_in_groups = reactive({
      get_mean_group_lineweights_in_groups(state$ena_obj,data$ena_groupVar[1],get_groups())
    })
    
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
    
    generate_plot = reactive({
      # req(initialized(),cancelOutput = TRUE)
      main_plot <- plot_ly(source='plot_correlation')
      print('generate plot')
      req(data$initialized,cancelOutput = TRUE)
      req(state$render_comparison(),cancelOutput = TRUE)
      req(state$is_app_initialized,cancelOutput = TRUE)
      req(!is.null(data$ena_obj),cancelOutput = TRUE)
      validate(
        need(input$x != '',FALSE),
        need(input$y != '',FALSE),
        need(input$z != '',FALSE),
      )
      if(state$render_comparison() == FALSE){
        return(NULL)
      }

      print('render comparsion')
      # Create an empty plot

      # Add the first trace (from points_plot)
      my_points <- as.data.table(scaled_points())
      colname = data$ena_groupVar[1]

      # Fix the bug of not showing edges when the dataset is loaded and the user hasn't open the model page      
      print(data$model_tab_clicked)
      print(input$select_group)
      

      filter_points <- my_points[which(my_points[[colname]] %in% get_groups())]

      
      print('filter_points')
      print(filter_points)
      
      
      # main_plot = add_trace(main_plot,
      #                         data = filter_points,
      #                         x = x_axis(),
      #                         y = y_axis(),
      #                         z = z_axis(),
      #                         color = tilde_group_var_or_null(),
      #                         colors = get_colors(),
      #                         text=get_secondary_groups(),
      #                         # test=get_secondary_groups(),
      #                         type = 'scatter3d',
      #                         mode = "markers",
      #                         # name = "Points",
      #                         hovertemplate = "X: %{x}<br>Y: %{y}<br>Z: %{z}<br>Group : %{text}<br>%{test}",
      #                         marker = list(
      #                           size = 5,
      #                           line = list(
      #                             width = 0
      #                           )
      #                           # ,name = labels[i] #rownames(nodes)[i]
      #                         ))


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
      main_plot <- layout(main_plot,
                          scene = list(xaxis = list(title = input$x,showgrid=input$show_grid,zeroline=input$show_zeroline),
                                       yaxis = list(title = input$y,showgrid=input$show_grid,zeroline=input$show_zeroline),
                                       zaxis = list(title = input$z,showgrid=input$show_grid,zeroline=input$show_zeroline)),
                          showlegend = TRUE)
      # if(length(get_select_group()) == 0){
      #   return(main_plot)
      # }
      # browser()
      
      if(input$compare_group_1 == input$compare_group_2){
        network <- get_mean_group_lineweights(state$ena_obj,data$ena_groupVar[1],input$compare_group_1)
      }else{
        g1.mean=get_mean_group_lineweights(state$ena_obj,data$ena_groupVar[1],input$compare_group_1)
        g2.mean=get_mean_group_lineweights(state$ena_obj,data$ena_groupVar[1],input$compare_group_2)
        
        subtracted.network <- g1.mean - g2.mean
        
        network<- subtracted.network
      }

      
      # network <- build_network(scaled_nodes(),
      #                          network=subtracted.network,
      #                          adjacency.key=state$ena_obj$rotation$adjacency.key)
      # main_plot<-plot_ly()
      # main_plot <- plot_network(main_plot,
      #                           network,
      #                           legend.include.edges = F,
      #                           x_axis=input$x,
      #                           y_axis=input$y,
      #                           z_axis=input$z,
      #                           line_width = input$line_width)
      # main_plot

      # colors = c(pos=input$comparison_group_1_color,
      #            input$comparison_group_2_color)
      # Generate Edges
      network <- build_network(scaled_nodes(),
                               network=network,
                               adjacency.key=state$ena_obj$rotation$adjacency.key,
                               colors=c(pos=input$comparison_group_1_color,
                                        input$comparison_group_2_color))

      main_plot <- plot_network(main_plot,
                                network,
                                legend.include.edges = T,
                                x_axis=input$x,
                                y_axis=input$y,
                                z_axis=input$z,
                                line_width = input$line_width)

      # if(!is.null(camera)){
      #   print('set cam')
      #   main_plot %>% layout(scene= list(camera=camera))
      # }
      # camera = list(
      #   eye=list(x=0., y=0., z=2.5)
      # )
      print(camera())
      main_plot <- layout(main_plot,title=input$camera_position,scene= list(camera=camera()))
      main_plot
    })
    
    # reactive(generate_plot())
    # output$ena_points_plot <- renderPlotly({
    #   print('generate plot ena_points_plot')
    #   # generate_plot()
    #   plot_ly(data.frame(x=c(1,2,3),y=c(1,2,3)))
    # })
    output$ena_points_plot <- renderPlotly({
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
    
    observeEvent(event_data(event = "plotly_relayout",source='plot_correlation'),{
      clicked <- event_data(event = "plotly_relayout",
                            source = "plot_correlation")
      if (!is.null(clicked)) {
        print(clicked)
      }
    })

}