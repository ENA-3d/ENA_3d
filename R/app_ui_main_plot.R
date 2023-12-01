
plot_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
    plotlyOutput(NS(id,"ena_points_plot"),height = "100vh"),
    plotlyOutput(NS(id,"ena_unit_group_change_plot"),height = "100ch")

  )
}
