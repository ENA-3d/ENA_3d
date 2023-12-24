"
This module is the main app. It handles the loading of the ENA data, and prepares data of nodes and points for rendering.
The logic for rendering the plots is handled in the sub-modules.
"
source('build_network.R')
source('transition.R')
source('app_utils.R')
source('app_module_ena_comparison_plot.R')
source('app_module_ena_unit_group_change_plot.R')
source('app_module_load_dataset.R')
source('app_module_upload_data.R')
source('app_module_sample_data.R')

library(shinyWidgets)

ena_app_server <- function(id,state,config) {
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
                           ena_obj=list(),
                           ena_points_plot_ready=FALSE,
                           initialized=FALSE,
                           model_tab_clicked=FALSE)



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

      ena_points <- function(){
        as.data.frame(state$ena_obj$points)
      }

      ena_nodes <- function(){
        state$ena_obj$rotation$nodes
      }

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

        for(i in colnames(state$ena_obj$points)){
          if(i %!in% colnames(state$ena_obj$meta.data)){
            my_points[[i]] <- my_points[[i]]*scale_factor()
          }
        }

        my_points
        # ena_points()
      })

      # Obtain the codes position after scaling
      scaled_nodes <- reactive({
        node_points = ena_nodes()
        test = rv$ena_groups
        for(i in colnames(state$ena_obj$points)){
          if(i %!in% colnames(state$ena_obj$meta.data)){
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


      ena_lines_mean_in_groups = reactive({
        get_mean_group_lineweights_in_groups(state$ena_obj,rv$ena_groupVar[1],input$select_group)
      })

      current_unit_slider_choice_sorted = reactive({
        sort(unique(state$ena_obj$points[,get(input$group_change_var)]))

      })


      "
        The plot in the model -> comparsion tab
      "
      ena_comparison_plot_output(input, output, session,
                                 rv,
                                 state,
                                 scaled_points,
                                 scaled_nodes,
                                 )
     
      "
        The plot in the model->change tab
      "
      ena_unit_group_change_plot_output(input,output,session,
                                        rv,
                                        state,
                                        scaled_points(),
                                        scaled_nodes()
                                        )

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
      
      upload_data(input,output,session,rv,state)
      sample_data_load_and_select(input,output,session,rv,config,state)

      # execute_at_next_input <- function(expr, session = getDefaultReactiveDomain()) {
      #   observeEvent(once = TRUE, reactiveValuesToList(session$input), {
      #     print(reactiveValuesToList(session$input))
      #     force(expr)
      #   }, ignoreInit = TRUE)
      # }
      
      # create checkbox for select group in the model->comparison tab
      # output$group_colors_container <- renderUI({
      #   n = length(rv$ena_groups)
      #   checkboxGroupInput(ns("select_group"), "Choose Group:",
      #                      choiceNames = rv$ena_groups,
      #                      choiceValues = rv$ena_groups,
      #                      selected=rv$ena_groups
      #   )
      # })
      
      
      observeEvent(input$select_group,{
        if(!is.null(input$select_group)){
          rv$model_tab_clicked<-TRUE
        }
      })

      
    }
  )
}
