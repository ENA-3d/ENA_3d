source('build_network.R')
source('transition.R')
source('app_utils.R')
library(shinyWidgets)

ena_app_server <- function(id,state) {
  # Calling the moduleServer function
  moduleServer(
    # Setting the id
    id,
    # Defining the module core mechanism
    function(input, output, session) {
      shinyjs::useShinyjs()
      ns = NS(id)
      initialized = reactiveVal(FALSE) # flag for successful initialization

      main_plot <- plot_ly()
      change_plot <- plot_ly()
      change_network <- NULL
      change_plot_list <- list()

      # reactive values, served as global variable inside the server
      rv <- reactiveValues(myList = list(),
                           unit_group_change_plots=list(),
                           current_unit_change_plot_camera=list(),
                           ena_groups=list(),
                           ena_groupVar=list(),
                           ena_obj=list())



      # Obtain the tilde formula for x axis, used in making plot
      x_axis = reactive({
        as.formula(paste("~", input$x))
      })

      # Obtain the tilde formula for y axis, used in making plot
      y_axis = reactive({
        as.formula(paste("~", input$y))
      })

      # Obtain the tilde formula for z axis, used in making plot
      z_axis = reactive({
        as.formula(paste("~", input$z))
      })

      # Obtain the tilde formula for group variable, used in making plot
      tilde_group_var_or_null = reactive({
        tilde_var_or_null(rv$env_groupVar[1])
      })

      ena_points <- reactive({
        as.data.frame(rv$ena_obj$points)
      })

      ena_nodes <- reactive({
        rv$ena_obj$rotation$nodes
      })

      # Obtain the points position after scaling
      scaled_points <- reactive({
        if(is.null(rv$ena_groupVar)){
          ena_points()
        }
        print(paste0('rv$ena_groupVar:',rv$ena_groupVar[1]))
        print(paste0('rv$ena_groups:',rv$ena_groups))

        my_points = ena_points()
        print(my_points)

        my_points[,rv$ena_groupVar[1]] <- as.character(my_points[,rv$ena_groupVar[1]])

        for(i in colnames(rv$ena_obj$points)){
          if(i %!in% colnames(rv$ena_obj$meta.data)){
            my_points[[i]] <- my_points[[i]]*scale_factor()
          }
        }

        my_points
        # ena_points()
      })

      # Obtain the codes position after scaling
      scaled_nodes <- reactive({
        node_points = ena_nodes()

        for(i in colnames(rv$ena_obj$points)){
          if(i %!in% colnames(rv$ena_obj$meta.data)){
            node_points[[i]] <- node_points[[i]]*scale_factor()
          }
        }

        node_size_range = c(3,10)
        node_points$weight = rep(0, nrow(node_points))
        if( any(node_points$weight > 0)) {
          node_points$weight = scales::rescale((node_points$weight * (1 / max(abs(node_points$weight)))), node_size_range) # * enaplot$get("multiplier"));
        }
        else {
          node_points$weight = node_size_range[2]
        }

        node_points
      })

      scale_factor<- reactive({
        input$scale_factor
      })

      display_labels <- reactive({
        # Create labels for the points based on the data (you can customize this)
        my_labels <- paste("Label:", 1:nrow(scaled_points()))
        my_labels
      })

      # Calculate the edges
      ena_lines_mean = reactive({
        lineweights = as.matrix(rv$ena_obj$line.weights)
        linesmean = as.vector(colMeans(lineweights))
        linesmean
      })

      ena_lines_mean_in_groups = reactive({
        get_mean_group_lineweights_in_groups(rv$ena_obj,rv$ena_groupVar[1],input$select_group)
      })

      current_unit_slider_choice_sorted = reactive({
        sort(unique(rv$ena_obj$points[,get(input$group_change_var)]))

      })

      ena_plot_points = reactive({
        my_points <- scaled_points()
        my_points <- as.data.table(my_points)
        colname = rv$ena_groupVar[1]
        print('selected_group')
        print(input$select_group)
        print(colname)
        print(colname %in% colnames(my_points))
        filter_points = my_points[which(my_points[[colname]] %in% c(input$select_group))]

        print('filter_points')
        print(filter_points)

        points_plot = add_trace(main_plot,
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
        points_plot
      })

      "
        The plot in the model -> comparsion tab
      "
      output$ena_points_plot <- renderPlotly({
        # req(initialized(),cancelOutput = TRUE)
        req(initialized(),cancelOutput = TRUE)

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
        main_plot <- ena_plot_points()

        my_nodes <- scaled_nodes()
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
        network <- build_network(scaled_nodes(),
                                 network=ena_lines_mean_in_groups(),
                                 adjacency.key=rv$ena_obj$rotation$adjacency.key)

        main_plot <- plot_network(main_plot,
                                  network,
                                  legend.include.edges = F,
                                  x_axis=input$x,
                                  y_axis=input$y,
                                  z_axis=input$z,
                                  line_width = input$line_width)
        main_plot
      })

      "
        The plot in the model->change tab
      "
      output$ena_unit_group_change_plot <- renderPlotly({
        req(initialized(),cancelOutput = TRUE)
        validate(
          need(input$x != '',FALSE),
          need(input$y != '',FALSE),
          need(input$z != '',FALSE),
          need(input$group_change_var != '','No group change var'),
        )

        # We first make all the plots among groups and cache the result
        # in order to increase performance
        if(length(rv$unit_group_change_plots) == 0){
          print('length 0')
          rv$unit_group_change_plots=make_unit_group_change_plots()
          rv$unit_group_change_plots[[as.character(input$unit_change)]]
        }else{
          print('not length 0')
          rv$unit_group_change_plots[[as.character(input$unit_change)]]
        }


      })

      # Hide or show the corresponding plot, depending on which tab is active
      observeEvent(state$active_tab(),{

        if(state$render_comparison()){
          print('show comparison plot')
          shinyjs::show("ena_points_plot")
          shinyjs::hide("ena_unit_group_change_plot")

        }
        if(state$render_unit_group_change_plot()){
          print('show unit group change plot')
          shinyjs::show("ena_unit_group_change_plot")
          shinyjs::hide("ena_points_plot")
        }
      })

      # Upload file and load the ena obj in the environment
      observeEvent(input$ena_data_file,{
        print(paste0('Receive input file',input$ena_data_file))
        file <- input$ena_data_file
        ext <- tools::file_ext(file$datapath)

        req(file)
        validate(
          need(ext == "Rdata" || ext == "RData", "Please upload a Rdata file")
        )

        print('load ena data')
        env_ena_data <- load(file=file$datapath)
        rv$ena_obj = get(env_ena_data)

        # Find all Dimensions (MR1, SVD2, SVD3 ...)
        dims <- list()
        for(i in colnames(rv$ena_obj$points)){
          if(i %!in% colnames(rv$ena_obj$meta.data)){
            dims=append(dims,i)
          }
        }
        print('update group var')
        rv$ena_groupVar <- get_ena_group_var(rv$ena_obj)
        # rv$ena_groups <- unique(as.data.frame(rv$ena_obj$points)[rv$ena_groupVar[1]])
        rv$ena_groups <- unique(rv$ena_obj$points[,get(rv$ena_groupVar[1])])

        print(paste0('rv$ena_groupVar:',typeof(rv$ena_groupVar)))
        print(paste0('rv$ena_groupsL',typeof(rv$ena_groups)))

        dim_choices <- unique(dims)
        updateSelectInput(session, "x", choices = dim_choices, selected = dim_choices[1])
        updateSelectInput(session, "y", choices = dim_choices, selected = dim_choices[2])
        updateSelectInput(session, "z", choices = dim_choices, selected = dim_choices[3])

        # update groups
        updateSelectInput(session, "change_group_1", choices = rv$ena_groups, selected = rv$ena_groups[1])
        updateSelectInput(session, "change_group_2", choices = rv$ena_groups, selected = rv$ena_groups[1])
        # update unit selection
        updateSelectInput(session, "group_change_var", choices = rv$ena_groupVar, selected = rv$ena_groupVar[1])


        unit_slider_choices=sort(unique(rv$ena_obj$points[,get(rv$ena_groupVar[1])]))
        updateSliderTextInput(session=session,inputId='unit_change',choices = unit_slider_choices)

        print(paste0('choices:',rv$ena_groupVar))
        # print(paste0(' updateSlider choices:',a))

        shinyjs::show("ena_points_plot")

        initialized(TRUE)
      })


      # create checkbox for select group in the model->comparison tab
      output$group_colors_container <- renderUI({
        n = length(rv$ena_groups)
        checkboxGroupInput(ns("select_group"), "Choose Unit:",
                           choiceNames = rv$ena_groups,
                           choiceValues = rv$ena_groups,
                           selected=rv$ena_groups
        )
      })

      "
      When the user change the axies or line width or scale factor,
      redraw the unit group change plot
      "
      observeEvent({input$x
                    input$y
                    input$z
                    input$line_width
                    input$scale_factor},

        {
        if(initialized() && length(rv$unit_group_change_plots)>0){
          rv$unit_group_change_plots <- make_unit_group_change_plots()
        }
      })

      # make plots for all the groups and save it inside a list
      make_unit_group_change_plots <- reactive({
        print('make_unit_group_plots')
        withProgress(message = 'Making plot', value = 0, {
          # Number of times we'll go through the loop
          # tm[input$group_change,]
          axx <- list(
            nticks = 4,
            range = c(-3,3)
          )
          axy <- list(
            nticks = 4,
            range = c(-3,3)
          )
          axz <- list(
            nticks = 4,
            range = c(-3,3)
          )
          camera = list(
            eye=list(x=1.5, y=1.5, z=1.5)
          )
          n=length(rv$ena_groups)
          my_nodes <- scaled_nodes()

          t <- list(
            family = "sans serif",
            size = 14,
            color = toRGB("grey50"))



          for (i in 1:n) {
            current_group = rv$ena_groups[i]
            print(paste0('current group:',current_group))

            mplot <- plot_ly()

            mplot <- add_trace(mplot, data = my_nodes, x = x_axis(), y = y_axis(), z = z_axis(),
                               type = 'scatter3d', mode = "markers", name = "Codes",
                               marker = list(
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

            c_network <- build_network(scaled_nodes(),
                                       network=get_mean_group_lineweights(rv$ena_obj,rv$ena_groupVar[1],current_group),
                                       adjacency.key=rv$ena_obj$rotation$adjacency.key)
            # print('start plotting network')
            mplot <- plot_network(mplot,
                                  c_network,
                                  legend.include.edges = FALSE,
                                  x_axis=input$x,
                                  y_axis=input$y,
                                  z_axis=input$z,
                                  line_width = input$line_width)

            mtitle = list(
              text=paste0(rv$ena_groupVar[1],' ',current_group),
              pad = list(t=50,b = 10, l = 10, r = 10)
            )

            mplot <- mplot %>% layout(title = mtitle,
                                      scene = list(xaxis=axx,yaxis=axy,zaxis=axz,camera=camera)
                                      )
            mplot  %>% toWebGL()
            rv$unit_group_change_plots[[as.character(current_group)]] <- mplot
            incProgress(1/n, detail = paste("Doing part: ", i))

          }
        })
        rv$unit_group_change_plots
      })
    }
  )
}
