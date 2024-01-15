model_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(

    tabsetPanel(
      id = ns("mytabs"),
      tabPanel("Model",value = "overall_model", model_overall_model_ui(id)),
      
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
    selectInput(ns("compare_group_2"), "Group 2", choices=list()),
  )
}

model_overall_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    # actionButton(ns('g1'),'Group 1'),
    # actionButton(ns('g2'),'Group 1')
    virtualSelectInput(
      inputId = "id",
      label = "Select:",
      choices = list(
        "Spring" = c("March", "April", "May"),
        "Summer" = c("June", "July", "August"),
        "Autumn" = c("September", "October", "November"),
        "Winter" = c("December", "January", "February")
      ),
      showValueAsTags = TRUE,
      search = TRUE,
      multiple = TRUE
    ),
    
    uiOutput(ns('group_colors_container'))
  )
}

model_group_change_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("group_change_var"), "Select Unit",choices=list()),
    # sliderInput(ns("main_group_change"), "Unit Change", value = 1, min = 1, max = 10)
    sliderTextInput(inputId = ns("unit_change"),
                    label = "Units",
                    choices = c(1,5,10,15,20,25,30))
  )
}
