source('./app_utils.R')
ena_comparison_plot_output <-  function(input, output, session,
                                        data,
                                        state,
                                        scaled_points,
                                        scaled_nodes,
                                        group_var=NULL
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
    
    tilde_group_var_or_null = reactive({
      tilde_var_or_null(data$env_groupVar[1])
    })

    ena_lines_mean_in_groups = reactive({
      get_mean_group_lineweights_in_groups(data$ena_obj,data$ena_groupVar[1],input$select_group)
    })
    generate_plot = reactive({
      # req(initialized(),cancelOutput = TRUE)
      main_plot <- plot_ly()
      print('generate plot')
      req(data$initialized,cancelOutput = TRUE)
      req(state$render_comparison(),cancelOutput = TRUE)
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
      my_points <- as.data.table(scaled_points)
      colname = data$ena_groupVar[1]
      # print('selected_group')
      # print(input$select_group)
      # print(colname)
      # print(colname %in% colnames(my_points))
      filter_points = my_points[which(my_points[[colname]] %in% c(input$select_group))]

      print('filter_points')
      print(filter_points)

      main_plot = add_trace(main_plot,
                              data = filter_points,
                              x = x_axis(),
                              y = y_axis(),
                              z = z_axis(),
                              color = tilde_group_var_or_null(),
                              colors = c('#BF382A', '#0C4B8E'),
                              type = 'scatter3d',
                              mode = "markers",
                              # name = "Points",
                              hovertemplate = "X: %{x}<br>Y: %{y}<br>Z: %{z}<br>Group ID: %{username}",
                              marker = list(
                                size = 5,
                                line = list(
                                  width = 0
                                )
                                #,name = labels[i] #rownames(nodes)[i]
                              ))


      my_nodes <- scaled_nodes
      # Add the second trace (from nodes_plot)
      main_plot <- add_trace(main_plot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
                             type = 'scatter3d', mode = "markers", name = "Codes",
                             marker = list(
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

      # main_plot <-  add_text(main_plot,data=my_nodes,x = x_axis(), y = y_axis(), z = z_axis(),
      #                        text = ~code,
      #                        textfont=t,
      #                        textposition = "top right")

      # Customize the layout and appearance of the combined plot
      main_plot <- layout(main_plot,
                          scene = list(xaxis = list(title = input$x),
                                       yaxis = list(title = input$y),
                                       zaxis = list(title = input$z)),
                          showlegend = FALSE)
      if(length(input$select_group) == 0){
        return(main_plot)
      }

      # Generate Edges
      network <- build_network(scaled_nodes,
                               network=ena_lines_mean_in_groups(),
                               adjacency.key=data$ena_obj$rotation$adjacency.key)

      main_plot <- plot_network(main_plot,
                                network,
                                legend.include.edges = F,
                                x_axis=input$x,
                                y_axis=input$y,
                                z_axis=input$z,
                                line_width = input$line_width)
      main_plot
    })
    
    # reactive(generate_plot())
    # output$ena_points_plot <- renderPlotly({
    #   print('generate plot ena_points_plot')
    #   # generate_plot()
    #   plot_ly(data.frame(x=c(1,2,3),y=c(1,2,3)))
    # })
    output$ena_points_plot <- renderPlotly({
      generate_plot()
    })
}