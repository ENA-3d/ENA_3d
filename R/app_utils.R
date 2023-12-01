library(shinyjs)
hide_element <- function(element_id){
  print(paste0('add dnone for',element_id))
  # shinyjs::addCssClass(element_id,'d-none')
  shinyjs::runjs(sprintf("$('#%s').addClass('d-none');", element_id))
  shinyjs::runjs(sprintf("console.log('add class %s')", element_id))

}
show_element <- function(element_id){
  print(paste0('remove dnone for',element_id))

  shinyjs::runjs(sprintf("$('#%s').removeClass('d-none');", element_id))
  shinyjs::runjs(sprintf("console.log('remove class %s')", element_id))
}
get_ena_group<- function(ena_obj){
  if(is.null(ena_obj$`_function.params`$groups) && is.null(ena_obj$`_function.params`$unit.groups)){
    stop('No group specified in the ena_obj! Either ena_obj$`_function.params`$groups or ena_obj$`_function.params`$unit.groups is null.')
  }
  if(!is.null(ena_obj$`_function.params`$groups)){
    return(ena_obj$`_function.params`$groups)
  }
  return(ena_obj$`_function.params`$unit.groups)
}
get_ena_group_var<- function(ena_obj){
  if(is.null(ena_obj$`_function.params`$groupVar) && is.null(ena_obj$`_function.params`$units.by)){
    stop('No group specified in the ena_obj! Either ena_obj$`_function.params`$groupVar or ena_obj$`_function.params`$unit.by is null.')
  }
  if(!is.null(ena_obj$`_function.params`$groupVar)){
    return(ena_obj$`_function.params`$groupVar)
  }
  return(ena_obj$`_function.params`$units.by)
}

get_camera_position <- function(plot) {
	  # This function takes a plotly 3d plot object and returns its current camera position.
	  #
	  # Parameters:
	  # plot (plotly object): A plotly 3d plot object
	  #
	  # Returns:
	  # list: A list containing the current camera position of the plot
  tryCatch({
    # Get the current camera position of the plot
    camera_position <- plotly:::plotly_build(plot)$scene$camera$eye

    # Return the camera position as a list
    return(as.list(camera_position))
  }, error = function(e) {
    # Log the error
    cat("Error: ", e$message, "\n")
    return(NULL)
  })
}


