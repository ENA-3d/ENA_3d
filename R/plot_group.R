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
    x_axis,
    y_axis,
    z_axis,
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
          c(as.vector(t.test(points[,1], conf.level = 0.95)$conf.int), as.vector(t.test(points[,2], conf.level = 0.95)$conf.int),as.vector(t.test(points[,3], conf.level = 0.95)$conf.int)),
          ncol=3
        );
      }
      if(outlier.interval != "none") {
        outlier.interval.values = c(IQR(points[,1]), IQR(points[,2]),IQR(points[,3])) * 1.5;
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
    ...
  )
  return(ena_plot)
}


ena_plot_points_3d = function(
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
  
  if(is(points, "numeric")){
    points = matrix(points);
    dim(points) = c(1,nrow(points))
    points.layout = data.table::data.table(points);
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
  points.matrix = rENA::remove_meta_data(points.layout)
  colnames(points.matrix) = paste0("X", rep(1:ncol(points.matrix)));
  this.max = max(points.matrix);
  for(m in 1:nrow(points.matrix)) {
    ena_plot = plotly::add_trace(
      p = ena_plot,
      data = points.matrix[m,],
      type ="scatter3d",
      x = ~X1, y = ~X2,z=~X3,
      mode = "markers+text",
      marker = list(
        symbol = shape[m],
        color = colors[m],
        size = point.size[m]
      ),
      error_x = error$x, error_y = error$y,
      showlegend = show.legend,
      # legendgroup = label.group,
      # legendgroup = ifelse(!is.null(box.label), labels[1], NULL),
      name = labels[m],
      text = texts[m], #labels[m],
      textfont = list(
        family = label.font.family,
        size = label.font.size,
        color = label.font.color
      ),
      legendgroup = legend.name,
      textposition = label.offset[m],
      hoverinfo = "x+y+name"
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

    this.max = max(boxv, this.max)
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
  
  return(ena_plot);
}

#group analysis# 
first.gruop.lineweights = as.matrix(set$line.weights$groupid$"1")
second.group.lineweights = as.matrix(set$line.weights$groupid$"2")
first.group.mean = as.vector(colMeans(first.gruop.lineweights))
second.group.mean = as.vector(colMeans(second.group.lineweights))


#points
first.group.points = as.matrix(set$points$groupid$`1`)
second.group.points = as.matrix(set$points$groupid$`2`)

#for all groups
lineweights = as.matrix(set$line.weights)
linesmean = as.vector(colMeans(lineweights))
allpoints = as.matrix(set$points)

plot = ena_plot_group(plot_ly(), points = first.group.points, confidence.interval = "box")
plot<-ena_plot_group(plot, points = second.group.points, confidence.interval = "box",color='blue')
network<-build_network(set$rotation$nodes,network=subtracted_network,adjacency.key = set$rotation$adjacency.key)
plot<-plot_network(plot,network,line_width = 1)
plot
# #plots group netwroks
plotgroup <- function(title, mean, point, color){
  plot = ena.plot(set, scale.to = "network", title = title)
  plot = ena.plot.group(plot, points = point, confidence.interval = "box", colors = c(color))
  # plot = ena.plot.points(plot, points = point, confidence.interval = "box", colors = c("blue"))
  plot = ena.plot.network(plot, network = mean*2)
  plot$plot
}
# 
# # #compare two groups
# comparegroup <- function(title, mean, point1, point2){
#   plot = ena.plot(set, scale.to = "network", title = title)
#   plot = ena.plot.group(plot, points = point1, confidence.interval = "box", colors = c("blue"))
#   # plot = ena.plot.points(plot, points = point1, confidence.interval = "box", colors = c("blue"))
#   plot = ena.plot.group(plot, points = point2, confidence.interval = "box", colors = c("red"))
#   # plot = ena.plot.points(plot, points = point2, confidence.interval = "box", colors = c("red"))
#   plot = ena.plot.network(plot, network = mean*2)
#   plot$plot
# }
# 
# comparegroup("Group 1 & 2",first.group.mean - second.group.mean, first.group.points, second.group.points)
# # # 
# mplot <- plotgroup("Group 1",first.group.mean, first.group.points, color = "blue")
# mplot <- ena_plot_group(mplot, first.group.points, confidence.interval = "box")
# mplot