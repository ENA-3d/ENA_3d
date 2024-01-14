camera_position_panel_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
    # actionButton(ns("default_cam_btn"), "Default Camera"),
    # actionButton(ns("x_y_btn"), "X-Y plane"),
    # actionButton(ns("x_z_btn"), "X-Z plane"),
    # actionButton(ns("y_z_btn"), "Y-Z plane")
    radioButtons(ns("camera_position"), "Camera Position:",
                c("Default 3D Camera" = "default",
                "X-Y Plane" = "x_y",
                "X-Z Plane" = "x_z",
                "Y-Z Plane" = "y_z",
                "Y-X Plane" = "y_x",
                "Z-X Plane" = "z_x",
                "Z-Y Plane" = "z_y"),
                selected = 'default',
                inline=TRUE)
  )
}