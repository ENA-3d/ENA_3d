data_upload_ui <- function(id) {
  # This ns <- NS structure creates a
  # "namespacing" function, that will
  # prefix all ids with a string
  ns <- NS(id)
  tagList(
    fileInput(NS(id,"ena_data_file"), "Choose Rdata File", accept = c(".Rdata",".RData"))
  )
}
