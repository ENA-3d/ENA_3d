ena_unit_group_change_plot_output <- function(input,output,session,
                                              rv_data,
                                              state,
                                              scaled_points,
                                              scaled_nodes){
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
      list(x=0., y=0., z=-2.5)
    }else if(pos == 'x_z'){
      list(x=0., y=2.5, z=0.)
    }else if(pos =='y_z'){
      list(x=2.5, y=0., z=0.)
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
  
  # make plots for all the groups and save it inside a list
  make_unit_group_change_plots <- reactive({
    print('make_unit_group_plots')
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      # tm[input$group_change,]
      # axx <- list(
      #   nticks = 4,
      #   range = c(-3,3)
      # )
      # axy <- list(
      #   nticks = 4,
      #   range = c(-3,3)
      # )
      # axz <- list(
      #   nticks = 4,
      #   range = c(-3,3)
      # )
      camera = list(
        eye=list(x=1.5, y=1.5, z=1.5)
      )
      n=length(rv_data$ena_groups)
      my_nodes <- scaled_nodes
      
      t <- list(
        family = "sans serif",
        size = 14,
        color = toRGB("grey50"))
      
      
      
      for (i in 1:n) {
        current_group = rv_data$ena_groups[i]
        print(paste0('current group:',current_group))
        
        mplot <- plot_ly()
        
        mplot <- add_trace(mplot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
                           type = 'scatter3d', mode = "markers", name = "Codes",
                           marker = list(
                             color ='rgb(77,77,77)',
                             size = abs(my_nodes$weight),
                             line = list(
                               width = 0
                             )
                             #,name = labels[i] #rownames(nodes)[i]
                           ))
        mplot <-  add_text(mplot,data=my_nodes,x = x_axis(), y = y_axis(), z = z_axis(),
                           text = ~code,
                           textfont=t,
                           textposition = "top right")
        
        c_network <- build_network(scaled_nodes,
                                   network=get_mean_group_lineweights(state$ena_obj,rv_data$ena_groupVar[1],current_group),
                                   adjacency.key=state$ena_obj$rotation$adjacency.key)
        # print('start plotting network')
        mplot <- plot_network(mplot,
                              c_network,
                              legend.include.edges = FALSE,
                              x_axis=input$x,
                              y_axis=input$y,
                              z_axis=input$z,
                              line_width = input$line_width)
        
        mtitle = list(
          text=paste0(rv_data$ena_groupVar[1],' ',current_group),
          pad = list(t=50,b = 10, l = 10, r = 10)
        )
        
        mplot <- mplot %>% layout(title = mtitle,
                                  scene = list(camera=camera)
        )
        
        mplot <- layout(mplot,
                            scene = list(xaxis = list(title = input$x,showgrid=input$show_grid,zeroline=input$show_zeroline),
                                         yaxis = list(title = input$y,showgrid=input$show_grid,zeroline=input$show_zeroline),
                                         zaxis = list(title = input$z,showgrid=input$show_grid,zeroline=input$show_zeroline)),
                            showlegend = TRUE)
        
        mplot<-add_3d_axis_based_on_user_selection(mplot)
        
        mplot  %>% toWebGL()
        rv_data$unit_group_change_plots[[as.character(current_group)]] <- mplot
        incProgress(1/n, detail = paste("Doing part: ", i))
        
      }
    })
    rv_data$unit_group_change_plots
  })
  
  "
      When the user change the axies or line width or scale factor,
      redraw the unit group change plot
      "
  observeEvent({input$x
    input$y
    input$z
    input$line_width
    input$scale_factor
    input$show_grid
    input$show_zeroline
    input$show_x_axis_arrow
    input$show_y_axis_arrow
    input$show_z_axis_arrow},
    
    {
      if(rv_data$initialized && length(rv_data$unit_group_change_plots)>0){
        rv_data$unit_group_change_plots <- make_unit_group_change_plots()
        
      }
    })
  
  
  
  "
        The plot in the model->change tab
  "
  output$ena_unit_group_change_plot <- renderPlotly({
    req(rv_data$initialized,cancelOutput = TRUE)
    validate(
      need(input$x != '',FALSE),
      need(input$y != '',FALSE),
      need(input$z != '',FALSE),
      need(input$group_change_var != '',FALSE),
    )
    
    # We first make all the plots among groups and cache the result
    # in order to increase performance
    if(length(rv_data$unit_group_change_plots) == 0){
      print('length 0')
      rv_data$unit_group_change_plots=make_unit_group_change_plots()
      p<-rv_data$unit_group_change_plots[[as.character(input$unit_change)]]
    }else{
      print('not length 0')
      p<- rv_data$unit_group_change_plots[[as.character(input$unit_change)]]

    }
    p <- layout(p,title=input$camera_position,scene= list(camera=camera()))
    p
    
  })
}
