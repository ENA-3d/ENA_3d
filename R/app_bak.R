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
source('app_ui_stats.R')
library(shiny.fluent)
library(coin)
library(shinyjs)
library(R6)
library(shinyfullscreen)

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
                    render_overall = FALSE,
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
                 "html{ font-size:0.9rem;}
                  .recalculating {opacity: 1.0;}
                  .mysidebar .left-side .nav {--bs-nav-link-padding-x:0.2rem;font-size:13px}
                  .mysidebar .left-side {padding:3px}
                  .mysidebar .left-side .nav a {text-align:center}
                  .mysidebar {height:95%}
                  .mysidebar .left-side {height:100%}
                  .mysidebar .left-side .nav {     
                        align-items: center;
                        justify-content: space-around;
                        display: flex;
                        height: 100%;
                        max-height:60vh;
                   }
                   .mysidebar .col-sm-10 {
                        overflow: scroll;
                        height: 100%;
                   }
                   .hide {
                        display:none !important;
                   }
                 "
      ),
      # titlePanel("ENA 3D"),
      theme = bslib::bs_theme(bootswatch = "darkly"),
      sidebarLayout(
        
        sidebarPanel(
          style = "height:90vh;",
          # h2('ENA 3D',),
          actionButton('toggle_sidebar_btn','Hide Sidebar',class='toggle-sidebar-btn'),
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
                     stats_ui(id = "main_app")
            ),
          )%>% 
            tagAppendAttributes(class= 'mysidebar'),
          
          width = 3),
        mainPanel(
          fluidRow(
            column(10,camera_position_panel_ui(id = "main_app")),
            column(2,actionButton(NS("main_app",'fullscreen_btn'),'Full Screen'))
            
          ),
          
          plot_ui(id = "main_app"),
          width = 9
        )%>%tagAppendAttributes(class= 'plot-container')
      ),   tags$script(
        type = "text/javascript",
        "
          const sidebar = document.getElementsByClassName('mysidebar')
  
          ch = sidebar[0].children
          left_side = ch[0]
          ch[0].classList.add('left-side')
          right_side = ch[1]
          ch[1].classList.add('right-side')
          
          var div = document.getElementsByClassName('mysidebar')[0];
          k = div.closest('.col-sm-3');
          $(k).addClass('big-sidebar')
          
          const plot_container = document.getElementsByClassName('plot-container')[0];
          
          tbtn = document.getElementsByClassName('toggle-sidebar-btn')[0];
          $(tbtn).on('click',function(e){
            $(left_side).toggleClass('col-sm-12')
            $(left_side).toggleClass('col-sm-2')
            $(right_side).toggleClass('hide')
            $(k).toggleClass('col-sm-1')
            $(k).toggleClass('col-sm-3')
            console.log('click')
            
            $(plot_container).toggleClass('col-sm-11')
            $(plot_container).toggleClass('col-sm-9')

            
            sidebar_text = document.getElementsByClassName('toggle-sidebar-btn')[0].getInnerHTML()
            
            if(sidebar_text == 'Show'){
              document.getElementsByClassName('toggle-sidebar-btn')[0].setHTML('Hide Sidebar')
            }else{
              document.getElementsByClassName('toggle-sidebar-btn')[0].setHTML('Show')

            }
          })

        "
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
  ena_server_state$render_overall <- reactive({
    ena_server_state$active_tab() == 'overall_model'
  })
  ena_server_state$render_unit_group_change_plot <-reactive({
    ena_server_state$active_tab() == 'group_change'
  })
  
  ena_app_server(id = "main_app",state=ena_server_state,config)
  # ena_comparison_plot_server( "main_app")
}
options(shiny.maxRequestSize = 50*1024^2)
shinyApp(app_ui, app_server)
