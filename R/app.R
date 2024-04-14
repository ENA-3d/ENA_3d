source('install_dependencies.R')

library(plotly)
library(data.table)
library(collapse)
library(shiny)
library(shinyWidgets)
library(shiny.fluent)
library(coin)
library(shinyjs)
library(R6)
library(shinyfullscreen)
library(rENA)
library(colourpicker)
source('app_ui_plot_settings.R')
source('app_ui_main_plot.R')
source('app_ui_model_tab.R')
source('app_ui_data_upload_tab.R')
source('app_server.R')
source('app_module_ena_comparison_plot.R')
source('color_list.R')
source('app_ui_camera_position_panel.R')
source('app_ui_stats.R')


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
                           render_network_plot=FALSE,
                           ena_obj=NULL,
                           color_list = color_list,
                           is_app_initialized = FALSE,
                           initialize = function() {}
                         )
)

app_ui <- function(){

  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(
        "$(document).on('shiny:inputchanged', function(event) {
          if (event.name != 'changed') {
            Shiny.setInputValue('changed', event.name);
          }
        });"
      )
    ),
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
                   .toggle-sidebar-btn{
                        position:absolute;
                        transform:translate(5px,-100px);
                        width:45px;
                        --bs-btn-padding-x:0.1rem;
                   }
                   .camera-position-panel .form-group{
                        display:flex;
                        flex-direction:row;
                        justify-content:center;
                        margin-bottom:0px;
                        align-items: center;
                   }
                  .camera-position-panel{
                        display:flex;
                        justify-content:center;
                  }
                  .plot-tool-bar{
                        padding: 10px 10px 5px 10px;
                  }
                 "
    ),
    # titlePanel("ENA 3D"),
    theme = bslib::bs_theme(bootswatch = "darkly"),
    fluidRow(
      
      column(3,
       
        style = "height:93vh;",

        fluidRow(
          h2('ENA 3D',id='ena_3d_h2'),
        ),
        
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
        fluidRow(
          actionButton('toggle_sidebar_btn','Hide',class='toggle-sidebar-btn'),
          
        )
        
        ),
      column(9,
        
        fluidRow(
          column(10,camera_position_panel_ui(id = "main_app"))%>% 
            tagAppendAttributes(class= 'camera-position-panel'),
          column(2,actionButton(NS("main_app",'fullscreen_btn'),'Full Screen'))
          
        )%>%tagAppendAttributes(class= 'plot-tool-bar'),
        
        plot_ui(id = "main_app"),
        
      )%>%tagAppendAttributes(class= 'plot-container')
    ),
    
    tags$script(
      type = "text/javascript",
      "   
          const sidebar_hide_text = 'Hide'
          const sidebar_show_text = 'Show'
          const sidebar = document.getElementsByClassName('mysidebar')
          
          ch = sidebar[0].children
          left_side = ch[0]
          ch[0].classList.add('left-side')
          right_side = ch[1]
          ch[1].classList.add('right-side')
          ch[1].classList.add('well')

          var div = document.getElementsByClassName('mysidebar')[0];
          k = div.closest('.col-sm-3');
          $(k).addClass('big-sidebar')
          
          const plot_container = document.getElementsByClassName('plot-container')[0];
          
          const tbtn = document.getElementsByClassName('toggle-sidebar-btn')[0];
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
            
            if(sidebar_text == sidebar_show_text){
              //document.getElementsByClassName('toggle-sidebar-btn')[0].setHTML(sidebar_hide_text)
              $(document.getElementsByClassName('toggle-sidebar-btn')[0]).html(sidebar_hide_text)
              $('#ena_3d_h2').html('ENA 3D')
              
            }else{
              //document.getElementsByClassName('toggle-sidebar-btn')[0].setHTML(sidebar_show_text)
              $(document.getElementsByClassName('toggle-sidebar-btn')[0]).html(sidebar_show_text)
              $('#ena_3d_h2').html('ENA')
              
            }
            
            trans_x = $('.left-side').width() / 2 - 20;
            
            $(tbtn).css('transform',`translate(${trans_x}px,-100px)`)
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
  ena_server_state$render_network_plot <-reactive({
    ena_server_state$active_tab() == 'network'
  })
  
  ena_app_server(id = "main_app",state=ena_server_state,config)
  # ena_comparison_plot_server( "main_app")
}
options(shiny.maxRequestSize = 50*1024^2)
shinyApp(app_ui, app_server)
