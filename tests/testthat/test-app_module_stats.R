library(shiny)
library(shinytest)
ns<-NS('main_app')

testing_data_path<-'./test_data/testing_data.Rdata'

test_that("reactives and output updates", {
  # testServer(expr =  {
  #     session$setInputs(ena_data_file=list(datapath = testing_data_path))
  #     # expect_equal(xy(), 0)
  #     # expect_equal(yz(), 2)
  #     # expect_equal(output$out, "Result: 0")
  # 
  #   })
  set_input = function(inputArg, inputVal){
    args <- list(inputVal)
    names(args) <- c( inputArg)
    do.call(app$setInputs, args)
  }
  upload_file<-  function(inputArg, inputVal){
    args <- list(inputVal)
    names(args) <- c( inputArg)
    do.call(app$uploadFile, args)
  }
  
  app <- ShinyDriver$new("../../R")
  # app$snapshotInit("")
  # # set_input(ns('ena_data_file'),list(datapath = testing_data_path))
  # upload_file(ns('ena_data_file'),testing_data_path)
  # set_input(ns('x'),'MR1')
  # set_input(ns('y'),'SVD2')
  # set_input(ns('z'),'SVD3')
  # print(app$getAllValues())
  # app$setInputs(`main_app-x`='MR1')
  # app$setInputs(greet = "click")
  # app$snapshot()
  # app$snapshot(list(output = "greeting"))
  app$recordTest({
    # Simulate file upload
    app$setInputs(file = list(name = "test.csv", type = "text/csv", datapath = "path/to/test.csv"))
    
    # Add more interactions if needed
    
    # Validate the output or behavior
    # For example, check if the uploaded file is processed correctly
    output <- app$getOutputValue("output_table_id")
    expect_equal(nrow(output), expected_row_count)
  })
})

