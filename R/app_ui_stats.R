stats_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  fluidPage(
    fluidRow(
      selectInput('stats_group1','Group 1',choices = c()),
      selectInput('stats_group2','Group 2',choices = c())
    ),
    fluidRow(
      stats_box(ns('stats_box_x_axis'),'X-axis','t'),
      hr(),
      stats_box(ns('stats_box_y_axis'),'Y-axis','t'),
      hr(),
      stats_box(ns('stats_box_z_axis'),'Z-axis','t'),
      
    # stats_box(ns('stats_box_y_axis')),
    # stats_box(ns('stats_box_z_axis')),
    )
  )
}
stats_box <- function(id,axis,test_type_value){
  ns <- NS(id)
  fixedPage(
    tags$style(type="text/css","
    .container:has(.stats-box-row) {
      background:white;
      color:black;
      border-radius:0.325rem;
      padding:15px
    }
    .container:has(.stats-box-row) table {
      color:black;
    }
    .stats-box{
      border: solid 1px;
      border-width: 0px 0px 1px 0px;
    }
    .stats-box-data-table{
      color:black;
    }
               "),
    
    fixedRow(
      column(6,h5(paste0(axis,':'))),
      column(6,textOutput(ns('axis_name')))
    )%>% tagAppendAttributes(class="stats-box-row") ,
    fluidRow(
      tableOutput(ns('data_table'))%>% tagAppendAttributes(class="stats-box-data-table")
    ),
    # tableOutput(ns('data_table')),
    fixedRow(
      column(6,"Effect Size:"),
      column(6,textOutput(ns('effect_size')))
    )%>% tagAppendAttributes(class="stats-box"),
    fixedRow(
      column(6,"P-value:"),
      column(6,textOutput(ns('p_value')))
    )%>% tagAppendAttributes(class="stats-box"),
    fluidRow(
      column(6,test_type_value),
      column(6,textOutput(ns('test_type_value')))
    )%>% tagAppendAttributes(class="stats-box"),
    fluidRow(
      column(6,'df'),
      column(6,textOutput(ns('df_value')))
    )%>% tagAppendAttributes(class="stats-box"),
    fluidRow(
      column(6,textOutput(ns('conf_level'))),
      column(6,textOutput(ns('conf')))
    )%>% tagAppendAttributes(class="stats-box")
  )
}