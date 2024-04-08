ena_plot_group_3d <- function(
    ena_plot,
    points = NULL,
    method = "mean",
    labels = NULL,
    colors = 'red',
    shape = c("square", "triangle-up", "diamond", "circle"),
    confidence.interval = c("none", "crosshairs", "box"),
    outlier.interval = c("none", "crosshairs", "box"),
    label.offset = "bottom right",
    label.font.size = 10,
    label.font.color = '#000000',
    label.font.family = 'Arial',
    show.legend = T,
    legend.name = NULL,
    x_axis='MR1',
    y_axis='SVD2',
    z_axis='SVD3',
    group_name=NULL,
    ...
) {
  shape = match.arg(shape);
  confidence.interval = match.arg(confidence.interval);
  outlier.interval = match.arg(outlier.interval);
  
  if(is.null(points)) {
    stop("Points must be provided.");
  }
  else if(is(points, "ena.points")) {
    points = rENA::remove_meta_data(points)
  }
  
  ### problem if outlier and confidence intervals selected for crosshair
  if(confidence.interval == "crosshairs" && outlier.interval == "crosshairs") {
    message("Confidence Interval and Outlier Interval cannot both be crosshair. Plotting Outlier Interval as box");
    outlier.interval = "box";
  }
  
  ### if group more than one row, combine to mean
  confidence.interval.values = NULL;
  outlier.interval.values = NULL;
  if(
    (is(points, "data.frame") || is(points, "matrix")) &&
    nrow(points) > 1
  ) {
    if(is.null(method) || method == "mean") {
      if(confidence.interval != "none") {
        confidence.interval.values = matrix(
          c(as.vector(t.test(points[,x_axis], conf.level = 0.95)$conf.int), as.vector(t.test(points[,y_axis], conf.level = 0.95)$conf.int),as.vector(t.test(points[,z_axis], conf.level = 0.95)$conf.int)),
          ncol=3
        );
      }
      if(outlier.interval != "none") {
        outlier.interval.values = c(IQR(points[,x_axis]), IQR(points[,y_axis]),IQR(points[,z_axis])) * 1.5;
        outlier.interval.values = matrix(rep(outlier.interval.values, 3), ncol = 3, byrow = T) * c(-1, 1)
      }
      
      if(length(unique(colors)) > 1) {
        points = t(sapply(unique(colors), function(color) colMeans(points[color == colors,]), simplify = T))
        colors = unique(colors)
        attr(ena_plot, "means") <- length(attr(ena_plot, "means")) + length(colors)
      } else {
        points = colMeans(points);
        attr(ena_plot, "means") <- length(attr(ena_plot, "means")) + 1
      }
    }
    else {
      if(confidence.interval != "none") warning("Confidence Intervals can only be used when method=`mean`")
      if(outlier.interval != "none") warning("Outlier Intervals can only be used when method=`mean`")
      
      points = apply(points, 2, function(x) do.call(method, list(x)) )
      attr(ena_plot, "means") <- length(attr(ena_plot, "means")) + 1
    }
  }
  #print(points)
  ena_plot <- ena_plot_points(
    ena_plot,
    points = points,
    labels = labels,
    colors = colors,
    shape = shape,
    confidence.interval = confidence.interval,
    confidence.interval.values = confidence.interval.values,
    outlier.interval = outlier.interval,
    outlier.interval.values = outlier.interval.values,
    label.offset = label.offset,
    label.font.size = label.font.size,
    label.font.color = label.font.color,
    label.font.family = label.font.family,
    show.legend = show.legend,
    legend.name = legend.name,
    x_axis=x_axis,
    y_axis=y_axis,
    z_axis=z_axis,
    group_name = group_name,
    ...
  )
  
  ena_plot$confidence.interval.values <-confidence.interval.values
  ena_plot$outlier.interval.values <-outlier.interval.values
  
  return(ena_plot)
}


ena_plot_points = function(
    ena_plot,
    
    points = NULL,    #vector of unit names or row indices
    point.size = '10',
    labels = NULL, #unique(enaplot$enaset$enadata$unit.names),
    label.offset = "top left",
    label.group = NULL,
    
    label.font.size =10, #enaplot$get("font.size"),
    label.font.color = '#000000', #enaplot$get("font.color"),
    label.font.family = "Arial", #enaplot$get("font.family"),
    
    shape = "circle",
    colors = NULL, # c("blue"), #rep(I("black"), nrow(points)),
    
    confidence.interval.values = NULL,
    confidence.interval = c("none", "crosshairs", "box"),
    
    outlier.interval.values = NULL,
    outlier.interval = c("none", "crosshairs", "box"),
    show.legend = T,
    legend.name = "Points",
    texts = NULL,
    x_axis='MR1',
    y_axis='SVD2',
    z_axis='SVD3',
    group_name=NULL,
    ...
) {
  ###
  # Parameter Checking and Cleaning
  ###
  # env = environment();
  # for(n in c("font.size", "font.color", "font.family")) {
  #   if(is.null(get(paste0("label.",n))))
  #     env[[paste0("label.",n)]] = enaplot$get(n);
  # }
  label.font.size;
  
  if(is.null(points)) {
    stop("Must provide points to plot.")
    # points = enaplot$enaset$points
  }
  #print(points)
  #save the colnames
  cnames = row.names(as.data.frame(points))
  if(is(points, "numeric")){
    points = matrix(points);
    dim(points) = c(1,nrow(points))
    points.layout = data.table::data.table(points);
    #reset the column names
    colnames(points.layout) <-cnames
  }
  else if (is.data.table(points)) {
    # points.layout = remove_meta_data(points)
    points.layout = data.table::copy(points)
  }
  else {
    points.layout = data.table::data.table(points);
  }
  
  if(!is.character(label.font.family)) {
    stop("Must provide label font family as character.")
    
    # label.font.family = enaplot$get("font.family");
  }
  
  confidence.interval = match.arg(confidence.interval);
  outlier.interval = match.arg(outlier.interval);
  
  # shape = match.arg(shape);
  valid.shapes = c("circle", "square", "triangle-up", "diamond");
  if(!all(shape %in% valid.shapes))
    stop(sprintf( "Unrecognized shapes: %s", paste(unique(shape[!(shape %in% valid.shapes)]), collapse = ", ") ))
  if(length(shape) == 1)
    shape = rep(shape, nrow(points.layout))
  
  valid.label.offsets = c("top left","top center","top right","middle left","middle center","middle right","bottom left","bottom center","bottom right");
  if(!all(label.offset %in% valid.label.offsets))
    stop(sprintf( "Unrecognized label.offsets: %s", paste(unique(label.offset[!(label.offset %in% valid.label.offsets)]), collapse = ", ") ))
  if(length(label.offset) == 1)
    label.offset = rep(label.offset, nrow(points.layout))
  
  if(grepl("^c", confidence.interval) && grepl("^c", outlier.interval)) {
    message("Confidence Interval and Outlier Interval cannot both be crosshair");
    message("Plotting Outlier Interval as box");
    outlier.interval = "box";
  }
  
  if(length(colors) == 1) {
    colors = rep(colors, nrow(points.layout))
  }
  if(length(point.size) == 1)
    point.size = rep(point.size, nrow(points.layout))
  if(is.null(labels))
    show.legend = F
  ###
  # END: Parameter Checking and Cleaning
  ###
  
  ###
  # Set error value for CI|OI crosshair on plot
  ###
  error = list(x = list(visible=T, type="data"), y = list(visible=T, type="data"));
  int.values = NULL;
  if(grepl("^c", confidence.interval) && !is.null(confidence.interval.values)) {
    int.values = confidence.interval.values;
  }
  else if(grepl("^c", outlier.interval) && !is.null(outlier.interval.values)) {
    int.values = outlier.interval.values;
  }
  error$x$array = int.values[, 1];
  error$y$array = int.values[, 2];
  ###
  # END: Set error value for crosshair on plot
  ###
  
  ###
  # Set box value for CI|OI box on plot
  #####
  box.values = NULL;
  if(grepl("^b", confidence.interval) && !is.null(confidence.interval.values)) {
    box.values = confidence.interval.values;
    box.label = "Conf. Int.";
  }
  if(grepl("^b", outlier.interval) && !is.null(outlier.interval.values)) {
    box.values = outlier.interval.values;
    box.label = "Outlier Int.";
  }
  ######
  # END: Set box value for CI|OI box on plot
  ###
  
  ###
  # Plot
  #####
  #print('points.layout')
  #print(points.layout)
  points.matrix = rENA::remove_meta_data(points.layout)
  colnames(points.matrix) = paste0("X", rep(1:ncol(points.matrix)));
  #print('points.matrix')
  #print(points.matrix)
  this.max = max(points.matrix);
  for(m in 1:nrow(points.matrix)) {
    #print('m')
    #print(points.matrix[m,])
    ena_plot = plotly::add_trace(
      p = ena_plot,
      data = points.matrix[m,],
      type ="scatter3d",
      x = ~X1,
      y = ~X2,
      z = ~X3,
      #x = as.formula(paste("~", x_axis)),
      #y = as.formula(paste("~", y_axis)),
      #z=as.formula(paste("~", z_axis)),
      mode = "markers+text",
      marker = list(
        #symbol = shape[m],
        symbol = 'square',
        color = colors[m],
        size = 5
      ),
      error_x = error$x, error_y = error$y,
      showlegend = show.legend,
      # legendgroup = label.group,
      # legendgroup = ifelse(!is.null(box.label), labels[1], NULL),
      name = labels[m],
      text = group_name,#texts[m] #labels[m],
      textfont = list(
        family = label.font.family,
        size = label.font.size,
        color = label.font.color
      ),
      legendgroup = legend.name,
      textposition = label.offset[m],
      #hoverinfo = paste0('mean',"x+y+z"),
      hovertemplate = 'Mean: </br></br>x: %{x:.5f} </br>y: %{y:.5f}</br>z: %{z:.5f}'
    )
  }
  
  if(!is.null(box.values)) {
    # boxv = data.frame(
    #   X1 = c(box.values[1,1], box.values[2,1], box.values[2,1], box.values[1,1]),
    #   X2 = c(box.values[1,2], box.values[1,2], box.values[2,2], box.values[2,2]),
    # )
    # boxv = data.frame(
    #   X1 = c(box.values[1,1], box.values[2,1], box.values[2,1], box.values[1,1],box.values[1,1], box.values[2,1], box.values[2,1], box.values[1,1]),
    #   X2 = c(box.values[1,2], box.values[1,2], box.values[2,2], box.values[2,2],box.values[1,2], box.values[1,2], box.values[2,2], box.values[2,2]),
    #   X3 = c(box.values[1,3], box.values[1,3], box.values[1,3], box.values[1,3],box.values[2,3], box.values[2,3], box.values[2,3], box.values[2,3])
    # )
    boxv1 = data.frame(
      X1 = c(box.values[1,1], box.values[2,1], box.values[2,1], box.values[1,1],box.values[1,1]),
      X2 = c(box.values[1,2], box.values[1,2], box.values[2,2], box.values[2,2],box.values[1,2]),
      X3 = c(box.values[1,3], box.values[1,3], box.values[1,3], box.values[1,3],box.values[1,3])
    )
    
    this.max = max(boxv1, this.max)
    ena_plot = plotly::add_trace(
      p = ena_plot,
      data = boxv1,
      type = "scatter3d",
      x = ~X1, y = ~X2,z=~X3,
      mode = "lines",
      line = list(
        width = 1,
        color = colors[1],
        dash = "dash"
      ),
      # "legendgroup" = labels[1],
      showlegend = show.legend,
      name = box.label
    )
    
    boxv2 = data.frame(
      X1 = c(box.values[1,1], box.values[2,1], box.values[2,1], box.values[1,1],box.values[1,1]),
      X2 = c(box.values[1,2], box.values[1,2], box.values[2,2], box.values[2,2],box.values[1,2]),
      X3 = c(box.values[2,3], box.values[2,3], box.values[2,3], box.values[2,3],box.values[2,3])
    )
    ena_plot = plotly::add_trace(
      p = ena_plot,
      data = boxv2,
      type = "scatter3d",
      x = ~X1, y = ~X2,z=~X3,
      mode = "lines",
      line = list(
        width = 1,
        color = colors[1],
        dash = "dash"
      ),
      # "legendgroup" = labels[1],
      showlegend = show.legend,
      name = box.label
    )
    
    boxv3 = data.frame(
      X1 = c(box.values[1,1], box.values[2,1], box.values[2,1], box.values[1,1],box.values[1,1]),
      X2 = c(box.values[1,2], box.values[1,2], box.values[1,2], box.values[1,2],box.values[1,2]),
      X3 = c(box.values[1,3], box.values[1,3], box.values[2,3], box.values[2,3],box.values[1,3])
    )
    ena_plot = plotly::add_trace(
      p = ena_plot,
      data = boxv3,
      type = "scatter3d",
      x = ~X1, y = ~X2,z=~X3,
      mode = "lines",
      line = list(
        width = 1,
        color = colors[1],
        dash = "dash"
      ),
      # "legendgroup" = labels[1],
      showlegend = show.legend,
      name = box.label
    )
    boxv4 = data.frame(
      X1 = c(box.values[1,1], box.values[2,1], box.values[2,1], box.values[1,1],box.values[1,1]),
      X2 = c(box.values[2,2], box.values[2,2], box.values[2,2], box.values[2,2],box.values[2,2]),
      X3 = c(box.values[1,3], box.values[1,3], box.values[2,3], box.values[2,3],box.values[1,3])
    )
    ena_plot = plotly::add_trace(
      p = ena_plot,
      data = boxv4,
      type = "scatter3d",
      x = ~X1, y = ~X2,z=~X3,
      mode = "lines",
      line = list(
        width = 1,
        color = colors[1],
        dash = "dash"
      ),
      # "legendgroup" = labels[1],
      showlegend = show.legend,
      name = box.label
    )
    ena_plot$boxv1<-boxv1
    ena_plot$boxv2<-boxv2
    ena_plot$boxv3<-boxv3
    ena_plot$boxv4<-boxv4
  }
  # enaplot$axes$y$range = c()
  # enaplot$axes$y$range
  # if(this.max*1.2 > max(enaplot$axes$y$range)) {
  #   this.max = this.max * 1.2
  #   enaplot$axes$x$range = c(-this.max, this.max)
  #   enaplot$axes$y$range = c(-this.max, this.max)
  #   ena_plot = plotly::layout(
  #     ena_plot,
  #     xaxis = enaplot$axes$x,
  #     yaxis = enaplot$axes$y
  #   );
  # }
  #####
  # END: Plot
  ###

  #print(box.values)
  return(ena_plot);
}