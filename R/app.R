library(plotly)
library(data.table)
library(collapse)
library(shiny)
source('app_ui_plot_settings.R')
source('app_ui_main_plot.R')
source('app_ui_model_tab.R')
source('app_ui_data_upload_tab.R')
library(shinyWidgets)
source('app_server.R')
source('app_module_ena_comparison_plot.R')
source('color_list.R')
source('app_ui_camera_position_panel.R')

library(shinyjs)
library(R6)

config = list()
config$sample_data_path = normalizePath("../sample_data")
"
R6 class.
It is an object used to communicate data between modules.
"
ENA_3D_Server <- R6Class("ENA_3D_Server",
                  public = list(
                    active_tab = NULL,
                    render_comparison = FALSE,
                    render_group_change = FALSE,
                    render_unit_group_change_plot=FALSE,
                    ena_obj=NULL,
                    color_list = color_list,
                    is_app_initialized = FALSE,
                    initialize = function() {}
                  )
)

app_ui <- function(){

    fluidPage(
      shinyjs::useShinyjs(),
      tags$style(type="text/css",
                 ".recalculating {opacity: 1.0;}
                  .mysidebar .col-sm-2 .nav {--bs-nav-link-padding-x:0.2rem;font-size:13px}
                  .mysidebar .col-sm-2 {padding:3px}
                  .mysidebar .col-sm-2 .nav a {text-align:center}
                  .mysidebar {height:100%}
                  .mysidebar .col-sm-2 {height:100%}
                  .mysidebar .col-sm-2 .nav {     align-items: center;
                                                    justify-content: space-around;
                                                    display: flex;
                                                    height: 100%;}
                 "
      ),
      titlePanel("ENA 3D"),
      theme = bslib::bs_theme(bootswatch = "darkly"),
      sidebarLayout(
        sidebarPanel(
          style = "min-height:80vh;height:100%",
          navlistPanel(
            widths = c(2, 10),
            tabPanel("Data",
                     data_upload_ui(id = "main_app")
            ),
            tabPanel("Model",
                     
                      model_ui(id = "main_app"),
                     ),
            tabPanel("Plot Tools",
                      plot_settings_ui(id = "main_app")
            ),
            tabPanel("Stats",
                     plot_settings_ui(id = "main_app")
            ),
          )%>% 
            tagAppendAttributes(class= 'mysidebar'),
          width = 3),
        mainPanel(
          camera_position_panel_ui(id = "main_app"),
          plot_ui(id = "main_app"),
          width = 9
        )
      )

    )

}

"
 Server wrapper, used to passing variables (state) between UI and the
"
app_server <- function(input, output, session) {
  # Use ena_server_state to communicate between the UI and ena_app_server module
  ena_server_state <- ENA_3D_Server$new()
  ena_server_state$active_tab <- reactive({
    input$'main_app-mytabs'
  })
  
  # The server needs to know which tab is currently active in order to show the corresponding data
  ena_server_state$render_comparison <- reactive({
    ena_server_state$active_tab() == 'comparison_plot'
  })
  ena_server_state$render_group_change <- reactive({
    ena_server_state$active_tab() == 'two_group'
  })
  ena_server_state$render_unit_group_change_plot <-reactive({
    ena_server_state$active_tab() == 'group_change'
  })
  
  ena_app_server(id = "main_app",state=ena_server_state,config)
  # ena_comparison_plot_server( "main_app")
}
options(shiny.maxRequestSize = 50*1024^2)
shinyApp(app_ui, app_server)
