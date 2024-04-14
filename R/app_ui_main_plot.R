
plot_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
    plotlyOutput(NS(id,"ena_points_plot"),height = "90vh") %>%fullscreen_this(click_id = ns('fullscreen_btn_')) ,
    plotlyOutput(NS(id,"ena_unit_group_change_plot"),height = "90vh") %>%fullscreen_this(click_id = ns('fullscreen_btn_2')) ,
    plotlyOutput(NS(id,"ena_overall_plot"),height = "90vh")%>%fullscreen_this(click_id = ns('fullscreen_btn')),
    plotlyOutput(NS(id,"ena_network_plot"),height = "90vh")%>%fullscreen_this(click_id = ns('fullscreen_btn'))
    
  )
}
