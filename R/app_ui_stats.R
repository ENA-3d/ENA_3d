stats_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(6,selectInput(ns('stats_group1'),'Group 1',choices = c())),
      column(6,selectInput(ns('stats_group2'),'Group 2',choices = c()))
      
    ),
    # fluidRow(
    #   stats_box(ns('stats_box_x_axis'),'X-axis','t'),
    #   hr(),
    #   stats_box(ns('stats_box_y_axis'),'Y-axis','t'),
    #   hr(),
    #   stats_box(ns('stats_box_z_axis'),'Z-axis','t'),
    #   
    # # stats_box(ns('stats_box_y_axis')),
    # # stats_box(ns('stats_box_z_axis')),
    # )
    tabsetPanel(
      tabPanel("T test", 
                 stats_box(ns('stats_box_x_axis'),'X-axis'),
                 hr(),
                 stats_box(ns('stats_box_y_axis'),'Y-axis'),
                 hr(),
                 stats_box(ns('stats_box_z_axis'),'Z-axis'),
      ),
      tabPanel("Wilcoxon",
               stats_box(ns('stats_box_x_axis_wilcox_unpaired'),'X-axis'),
               hr(),
               stats_box(ns('stats_box_y_axis_wilcox_unpaired'),'Y-axis'),
               hr(),
               stats_box(ns('stats_box_z_axis_wilcox_unpaired'),'Z-axis'),
      ),
      tabPanel("Wilcoxon Paired",
               stats_box(ns('stats_box_x_axis_wilcox_paired'),'X-axis'),
               hr(),
               stats_box(ns('stats_box_y_axis_wilcox_paired'),'Y-axis'),
               hr(),
               stats_box(ns('stats_box_z_axis_wilcox_paired'),'Z-axis'),
               
               )
    )
  )
}
stats_box <- function(id,axis){
  ns <- NS(id)
  fixedPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app_ui_stats.css")
    ),
    
    fixedRow(
      column(6,h5(paste0(axis,':'))),
      column(6,textOutput(ns('axis_name')))
    )%>% tagAppendAttributes(class="stats-box-row") ,
    fluidRow(
      tableOutput(ns('data_table'))
    )%>% tagAppendAttributes(class="stats-box-data-table"),
    # tableOutput(ns('data_table')),
    fixedRow(
      column(6,"Effect Size:"),
      column(6,textOutput(ns('effect_size')))
    )%>% tagAppendAttributes(class="stats-box-row"),
    fixedRow(
      column(6,"P-value:"),
      column(6,textOutput(ns('p_value')))
    )%>% tagAppendAttributes(class="stats-box-row"),
    fluidRow(
      column(6,textOutput(ns('test_type'))),
      column(6,textOutput(ns('test_type_value')))
    )%>% tagAppendAttributes(class="stats-box-row"),
    fluidRow(
      column(6,textOutput(ns('conf_level'))),
      column(6,textOutput(ns('conf')))
    )%>% tagAppendAttributes(class="stats-box-row")
  )
}