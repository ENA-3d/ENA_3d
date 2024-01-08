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
tilde_var_or_null = function(var_name){
  result <- NULL
  if(is.null(var_name)){
    result <- NULL
  }else{
    result <- as.formula(paste("~",var_name))
  }
  result
}
add_3d_axis = function(plot){
  # Create a 3D plot with scatter3d trace for lines
  plot <- plot %>%
    add_trace(
      type = "scatter3d",
      mode = "lines+markers",
      x = c(0,1),
      y = c(0,0),
      z = c(0,0),
      line = list(color = "red", width = 2),
      marker = list(size = 1, color = "red")
    )
  
  cone_base_radius <- 1
  cone_height <- 1
  cone_center <- c(1, 0, 0)
  
  # Create a 3D plot with cone trace
  plot <- plot %>%
    add_trace(
      type = "cone",
      x = cone_center[1],
      y = cone_center[2],
      z = cone_center[3],
      u = list(cone_height),
      v = list(0),
      w = list(0),
      sizemode = "absolute",
      sizeref = 0.2,
      showscale = FALSE,
      colorscale = list(c(0, 'red'), c(1, 'red')),
      anchor = "tail"
    )
  
  plot <- plot %>%
    add_trace(
      type = "scatter3d",
      mode = "lines+markers",
      x = c(0,0),
      y = c(0,1),
      z = c(0,0),
      line = list(color = "blue", width = 2),
      marker = list(size = 1, color = "blue")
    )
  plot <- plot %>% add_text(
    x = cone_center[1],
    y = cone_center[2],
    z = cone_center[3]  + 0.1, # Adjust the height of the text above the cone
    text = "X axis",
    textfont=list(size = 12, color = "red")
  )
  
  
  cone_center <- c(0, 1, 0)
  
  # Create a 3D plot with cone trace
  plot <- plot %>%
    add_trace(
      type = "cone",
      x = cone_center[1],
      y = cone_center[2],
      z = cone_center[3],
      u = list(0),
      v = list(cone_height),
      w = list(0),
      sizemode = "absolute",
      sizeref = 0.2,
      showscale = FALSE,
      colorscale = list(c(0, 'blue'), c(1, 'blue')),
      anchor = "tail"
    ) 
  plot <- plot %>% add_text(
    x = cone_center[1],
    y = cone_center[2],
    z = cone_center[3]  + 0.1, # Adjust the height of the text above the cone
    text = "Y axis",
    textfont=list(size = 12, color = "blue")
  )
  
  
  plot <- plot %>%
    add_trace(
      type = "scatter3d",
      mode = "lines+markers",
      x = c(0,0),
      y = c(0,0),
      z = c(0,1),
      line = list(color = "green", width = 2),
      marker = list(size = 1, color = "green")
    )
  
  cone_center <- c(0, 0, 1)
  
  # Create a 3D plot with cone trace
  plot <- plot %>%
    add_trace(
      type = "cone",
      x = cone_center[1],
      y = cone_center[2],
      z = cone_center[3],
      u = list(0),
      v = list(0),
      w = list(cone_height),
      sizemode = "absolute",
      sizeref = 0.2,
      showscale = FALSE,
      colorscale = list(c(0, 'green'), c(1, 'green')),
      anchor = "tail"
    ) 
  plot <- plot %>% add_text(
    x = cone_center[1],
    y = cone_center[2],
    z = cone_center[3]  + 0.1, # Adjust the height of the text above the cone
    text = "Z axis",
    textfont=list(size = 12, color = "green")
  )
  
  plot <- layout(plot,title='X-Y',scene= list(camera=list(eye=list(x=0., y=0., z=-2.5))))
  plot
  
  
}
