library(shinytest2)

test_that("{shinytest2} recording: R", {
  app <- AppDriver$new(variant = platform_variant(), name = "R", height = 670, width = 1121)
  app$set_inputs(`main_app-show_grid` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-show_x_axis_arrow` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-show_z_axis_arrow` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-show_zeroline` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-show_y_axis_arrow` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-sample_data` = "sample_enaset.Rdata")
  app$expect_screenshot()
})


test_that("{shinytest2} recording: t_test_two_group", {
  app <- AppDriver$new(variant = platform_variant(), name = "t_test_two_group", height = 670, 
      width = 1121)
  app$set_inputs(`main_app-show_grid` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-show_x_axis_arrow` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-show_z_axis_arrow` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-show_zeroline` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-show_y_axis_arrow` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`main_app-sample_data` = "sample_enaset.Rdata")
  app$set_inputs(`main_app-stats_group2` = "2")
  app$expect_screenshot()

})
