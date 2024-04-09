model_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(

    tabsetPanel(
      id = ns("mytabs"),
      tabPanel("Overall",value = "overall_model", model_overall_model_ui(id)),
      tabPanel("Networks",value = "network", model_network_ui(id)),
      tabPanel("Comparison",value = "comparison_plot", model_two_group_comparison_ui(id)),
      tabPanel("Change",value = "group_change", model_group_change_ui(id)),
      # tabPanel("Two Group",value = "two_group",model_two_group_change_ui(id))

    )
  )

}
model_two_group_change_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("change_group_1"), "Group 1",choices=list()),
    selectInput(ns("change_group_2"), "Group 2", choices=list()),
    sliderInput(ns("group_change"), "Group Change", value = 1, min = 1, max = 10)
  )
}
model_two_group_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("compare_group_1"), "Group 1",choices=list()),
    colourpicker::colourInput(ns("comparison_group_1_color"), "Group 1 color", "#BF382A"),
    #Toggle.shinyInput(ns("compare_group_1_show_mean"),label="Mean",onText='On',inlineLabel=TRUE,value = TRUE),
    fixedRow(
      column(9,"Show Mean:"),
      column(3,Toggle.shinyInput(ns("compare_group_1_show_mean"),value = FALSE))
    ),
    fixedRow(
      column(9,"Show Confidence Interval:"),
      column(3,Toggle.shinyInput(ns("compare_group_1_show_confidence_interval"),value = FALSE))
    ),
    hr(),
    selectInput(ns("compare_group_2"), "Group 2", choices=list()),
    colourpicker::colourInput(ns("comparison_group_2_color"), "Group 2 color", "#0C4B8E"),
    #Toggle.shinyInput(ns("compare_group_2_show_mean"),value = TRUE),
    fixedRow(
      column(9,"Show Mean:"),
      column(3,Toggle.shinyInput(ns("compare_group_2_show_mean"),value = FALSE))
    ),
    fixedRow(
      column(9,"Show Confidence Interval:"),
      column(3,Toggle.shinyInput(ns("compare_group_2_show_confidence_interval"),value = FALSE))
    ),
  )
}

model_overall_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionButton(ns('g1'),'Group 1'),
    # actionButton(ns('g2'),'Group 1')
    # virtualSelectInput(
    #   inputId = "id",
    #   label = "Select:",
    #   choices = list(
    #     "Spring" = c("March", "April", "May"),
    #     "Summer" = c("June", "July", "August"),
    #     "Autumn" = c("September", "October", "November"),
    #     "Winter" = c("December", "January", "February")
    #   ),
    #   showValueAsTags = TRUE,
    #   search = TRUE,
    #   multiple = TRUE
    # ),
    
    uiOutput(ns('group_colors_container'))
  )
}

model_network_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionButton(ns('g1'),'Group 1'),
    # actionButton(ns('g2'),'Group 1')
    # virtualSelectInput(
    #   inputId = "id",
    #   label = "Select:",
    #   choices = list(
    #     "Spring" = c("March", "April", "May"),
    #     "Summer" = c("June", "July", "August"),
    #     "Autumn" = c("September", "October", "November"),
    #     "Winter" = c("December", "January", "February")
    #   ),
    #   showValueAsTags = TRUE,
    #   search = TRUE,
    #   multiple = TRUE
    # ),
    uiOutput(ns('network_groups_container')),
  )
}

model_group_change_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("group_change_var"), "Select Group Variable",choices=list()),
    # sliderInput(ns("main_group_change"), "Unit Change", value = 1, min = 1, max = 10)
    sliderTextInput(inputId = ns("unit_change"),
                    label = "Units",
                    choices = c(1,5,10,15,20,25,30)),
    hr(),
    fixedRow(
      column(9,"Show Mean:"),
      column(3,Toggle.shinyInput(ns("group_change_show_mean"),value = TRUE))
    ),
    fixedRow(
      column(9,"Show Confidence Interval:"),
      column(3,Toggle.shinyInput(ns("group_change_show_confidence_interval"),value = TRUE))
    ),
  )
}

group_selector_ui <- function(button_id,
                              points_toggle_id='1',
                              color_selector_id='1',
                              show_mean_btn_id='1',
                              show_conf_int_btn_id='1',
                              group_name='1',
                              group_color='grey'){
  #ns <- NS(id)
  # button_id <- ns(paste0("button"))
  # points_toggle_id <- ns(paste0("points_toggle"))
  # color_selector_id <- ns(paste0("color_selector"))
  # show_mean_btn_id <- ns(paste0("show_mean_btn"))
  # show_conf_int_btn_id <- ns(paste0("show_conf_int_btn"))
  if(is.null(button_id)){
    return(tagList())
  }
  tagList(
    # actionButton(ns('g1'),'Group 1'),
    # actionButton(ns('g2'),'Group 1')
    # virtualSelectInput(
    #   inputId = "id",
    #   label = "Select:",
    #   choices = list(
    #     "Spring" = c("March", "April", "May"),
    #     "Summer" = c("June", "July", "August"),
    #     "Autumn" = c("September", "October", "November"),
    #     "Winter" = c("December", "January", "February")
    #   ),
    #   showValueAsTags = TRUE,
    #   search = TRUE,
    #   multiple = TRUE
    # ),
    
    #uiOutput(ns('group_colors_container'))
    fixedRow(
      #column(6,actionButton(inputId = button_id,label = group_name, style=sprintf("color: white;background:%s",group_color))),
      #column(3,colourpicker::colourInput(ns("sadf"), "", "#0C4B8E"),),
      #column(3,switchInput(inputId = "Id018",label = "Points",size='mini')
      #       %>% tagAppendAttributes(class= 'group_show_points_button')),
      column(1,colourInput(color_selector_id,label = NULL,showColour = 'background',width='1px',value = group_color)),
      column(7,paste0("  ",group_name)),
      column(3,prettyToggle(
        inputId = points_toggle_id,
        label_on = "Points", 
        label_off = "Points",
        outline = TRUE,
        plain = TRUE,
        icon_on = icon("eye"), 
        icon_off = icon("eye-slash"),
        value = TRUE
      )),
      column(1,
             dropdownButton(
               
               tags$h4("Group Settings"),
               #hr(),
               #colourpicker::colourInput(color_selector_id, sprintf("Group %s color",group_name), group_color),
               hr(),
               tags$h5("Mean Options"),
               fixedRow(
                 column(9,"Show Mean:"),
                 column(3,Toggle.shinyInput(show_mean_btn_id,value = TRUE))
               ),
               fixedRow(
                 column(9,"Show Confidence Interval:"),
                 column(3,Toggle.shinyInput(show_conf_int_btn_id,value = TRUE))
               ),
               
               circle = TRUE, status = "info",
               icon = icon("gear"), width = "300px",size='xs',
               tooltip = tooltipOptions(title = "Group Option")
             )
          )
    )
    
  )
}



