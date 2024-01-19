plot_settings_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
        selectInput(ns("x"), "X axis",choices=list()),
        selectInput(ns("y"), "Y axis",choices=list()),
        selectInput(ns("z"), "Z axis",choices=list()),
        hr(),
        sliderInput(ns("scale_factor"), "Scale Factor", value = 1, min = 1, max = 10),
        sliderInput(ns("line_width"), "Edge Width Factor", value = 3, min = 1, max = 10),
        hr(),
        fluidRow(
          column(6,p('Show Grid')),
          column(3,Toggle.shinyInput(ns("show_grid"), value = TRUE))
        ),
        fluidRow(
          column(6,p('Show Zero Lines')),
          column(3,Toggle.shinyInput(ns("show_zeroline"), value = TRUE))
        ),
        hr(),
        fluidRow(
          column(6,p('Show X-axis Arrow')),
          column(3,Toggle.shinyInput(ns("show_x_axis_arrow"), value = TRUE))
        ),
        fluidRow(
          column(6,p('Show Y-axis Arrow')),
          column(3,Toggle.shinyInput(ns("show_y_axis_arrow"), value = TRUE))
        ),
        fluidRow(
          column(6,p('Show Z-axis Arrow')),
          column(3,Toggle.shinyInput(ns("show_z_axis_arrow"), value = TRUE))
        ),

      )
}
