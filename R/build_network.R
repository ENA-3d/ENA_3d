build_network = function(node.positions,network,edge_type = "line",
                         adjacency.key,
                         threshold = c(0),
                         thickness = c(min(abs(network)), max(abs(network))),
                         opacity = thickness,
                         saturation = thickness,
                         scale.range = c(ifelse(min(network)==0, 0, 0.1), 1),
                         labels = NULL,
                         label.offset = "middle right",
                         legend.include.edges = F,
                         scale.weights = F,
                         colors=c('#BF382A', '#0C4B8E'),
                         thin.lines.in.front=T,
                         show.all.nodes = T,
                         node.size = c(3,10)){

  network = network
  if(choose(nrow(node.positions), 2) != length(network)) {
    stop(paste0("Network vector needs to be of length ", choose(nrow(node.positions), 2)))
  }
  node.rows <- NULL
  if(is(node.positions, "ena.nodes")) {
    if(is.null(adjacency.key)) {
      adjacency.key <- namesToAdjacencyKey(node.positions$code)
    }
    node.rows <- node.positions$code

    if(is.null(labels)) {
      labels <- node.positions$code
    }
  }
  else {
    if(is.matrix(node.positions)) {
      node.positions <- as.data.frame(node.positions)
    }
    adjacency.key <- namesToAdjacencyKey(rownames(node.positions))
    node.rows <- rownames(node.positions)
    if(is.null(labels)) {
      labels  <- rownames(node.positions)
    }
  }
  network.edges.shapes = list();
  # print(network)
  nodes = data.frame(as.matrix(node.positions));
  # colnames(nodes) = paste0("X", seq(colnames(nodes)))
  nodes$weight = rep(0, nrow(nodes))
  nodes$color = "black";

  # Handle label parameters
  if(length(label.offset) == 1) {
    label.offset = rep(label.offset[1], length(labels))
  }
  if(length(label.offset) != length(labels)) {
    stop("length(label.offset) must be equal to 1 or length(labels)")
  }

  # Handle legend parameters
  if(legend.include.edges == T && !is.null(legend.name)) {
    legend.name = "Nodes"
  }

  network.scaled = network;
  if(!is.null(threshold)) {
    multiplier.mask = ((network.scaled >= 0) * 1) - ((network.scaled < 0) * 1)
    if(length(threshold) == 1) {
      threshold[2] = Inf;
    }
    else if(threshold[2] < threshold[1]) {
      stop("Minimum threshold value must be less than the maximum value.");
    }

    if(threshold[1] > 0) {
      # network.scaled = network.scaled[sizes > threshold[1]]
      network.scaled[abs(network.scaled) < threshold[1]] = 0
    }
    if(threshold[2] < Inf && any(abs(network.scaled) > threshold[2]))  {
      to.threshold = abs(network.scaled) > threshold[2]
      network.scaled[to.threshold] = threshold[2]
      network.scaled[to.threshold] = network.scaled[to.threshold] * multiplier.mask[to.threshold]
    }
  }
  network.thickness = abs(network.scaled) * 10;
  network.saturation = abs(network.scaled);
  network.opacity = abs(network.scaled);

  network.to.keep = (network != 0) * 1
  if(scale.weights == T) {
    network.scaled = network * (1 / max(abs(network)));
    network.thickness = scales::rescale(x = abs(network.scaled), to = scale.range, from = thickness);
  }
  network.scaled = network.scaled * network.to.keep
  network.thickness = network.thickness * network.to.keep

  network.saturation = scales::rescale(x = abs(network.scaled), to = scale.range, from = saturation);
  network.opacity = scales::rescale(x = abs(network.scaled), to = scale.range, from = opacity);

  pos.inds = as.numeric(which(network.scaled >=0));
  neg.inds = as.numeric(which(network.scaled < 0));

  colors.hsv = rgb2hsv(col2rgb(colors))

  if(ncol(colors.hsv) == 1) {
    colors.hsv[[4]] = colors.hsv[1] + 0.5;
    if(colors.hsv[4] > 1) {
      colors.hsv[4] = colors.hsv[4] - 1;
    }

    colors.hsv[[5]] = colors.hsv[2];
    colors.hsv[[6]] = colors.hsv[3];
    dim(colors.hsv) = c(3,2);
  }

  mat = as.matrix(adjacency.key);
  for (i in 1:length(network)) {
    v0 <- nodes[node.rows==mat[1,i], ];
    v1 <- nodes[node.rows==mat[2,i], ];
    # print('v0')
    # print(v0)
    # nodes[node.rows==mat[1,i],]$weight = nodes[node.rows==mat[1,i],]$weight + abs(network.thickness[i]);
    # nodes[node.rows==mat[2,i],]$weight = nodes[node.rows==mat[2,i],]$weight + abs(network.thickness[i]);

    color = NULL
    if(i %in% pos.inds) {
      color = colors.hsv[,1];
    } else {
      color = colors.hsv[,2];
    }
    color[2] = network.saturation[i];

    edge_shape = list(
      type = "line",
      opacity = network.opacity[i],
      nodes = c(mat[,i]),
      line = list(
        name = "test",
        color= hsv(color[1],color[2],color[3]),
        # width= abs(network.thickness[i]) * enaplot$get("multiplier"),
        width= abs(network.thickness[i]) * 1,
        dash = edge_type
      ),
      x0 = as.numeric(v0[1]),
      y0 = as.numeric(v0[2]),
      x1 = as.numeric(v1[1]),
      y1 = as.numeric(v1[2]),
      node1 = v0,
      node2 = v1,
      layer = "below",
      size = as.numeric(abs(network.scaled[i]))
    );
    network.edges.shapes[[i]] = edge_shape
  };
  if(thin.lines.in.front) {
    network.edges.shapes = network.edges.shapes[rev(order(sapply(network.edges.shapes, "[[", "size")))]
  }
  else {
    network.edges.shapes = network.edges.shapes[order(sapply(network.edges.shapes, "[[", "size"))]
  }

  rows.to.keep = rep(T, nrow(nodes))
  if(show.all.nodes == F) {
    rows.to.keep = nodes$weight != 0
    # nodes = nodes[rownames(nodes) %in% unique(as.character(sapply(network.edges.shapes, "[[", "nodes"))), ]
  }
  nodes = nodes[rows.to.keep,];

  if( any(nodes$weight > 0)) {
    nodes$weight = scales::rescale((nodes$weight * (1 / max(abs(nodes$weight)))), node.size) # * enaplot$get("multiplier"));
  }
  else {
    nodes$weight = node.size[2]
  }


  network_obj=list()
  network_obj$network.edges.shapes <-network.edges.shapes
  network_obj$network <- network
  network_obj$network.scaled <- network.scaled
  network_obj$nodes <- nodes
  network_obj$rows.to.keep <- rows.to.keep
  return(network_obj)
}



plot_network <- function(ena_plot,
                         network,
                         legend.include.edges=F,
                         x_axis='MR1',
                         y_axis='SVD2',
                         z_axis='SVD3',
                         line_width=5){
  for(i in 1:length(network$network.edges.shapes)){
    edge = network$network.edges.shapes[[i]]
    # print(data.frame(X1=c(edge$x0,edge$x1), X2=c(edge$y0,edge$y1)))

    edge_name = paste(edge$nodes[1],edge$nodes[2],sep='.')

    show.legend = F
    if(legend.include.edges) {
      show.legend = T;
    }

    node1 = edge$node1
    node2 = edge$node2

    edge_line <- edge$line
    edge_line$width <- edge_line$width * line_width

    ena_plot = plotly::add_trace(
      ena_plot,
      type = "scatter3d",mode = "lines",
      # data = data.frame(X1=c(node1$MR1,node2$MR1), X2=c(node1$SVD2,node2$SVD2),X3=c(node1$SVD3,node2$SVD3)),
      data = data.frame(X1=c(node1[,x_axis],node2[,x_axis]),
                        X2=c(node1[,y_axis],node2[,y_axis]),
                        X3=c(node1[,z_axis],node2[,z_axis])),

      # data.frame(X1=c(edge$x0,edge$x1), X2=c(edge$y0,edge$y1)),
      x = ~X1, y = ~X2,z = ~X3,
      line = edge_line,
      opacity = edge$opacity,
      # legendgroup = if(legend.include.edges == T) this.name else legend.name,
      showlegend = show.legend,
      name = edge_name
    )

  }
  ena_plot
}

plot_network_improve <- function(ena_plot,
                         network,
                         legend.include.edges=F,
                         x_axis='MR1',
                         y_axis='SVD2',
                         z_axis='SVD3',
                         line_width=5){

  trace_data <- data.frame()

  for(i in 1:length(network$network.edges.shapes)){
    edge = network$network.edges.shapes[[i]]
    # print(data.frame(X1=c(edge$x0,edge$x1), X2=c(edge$y0,edge$y1)))

    edge_name = paste(edge$nodes[1],edge$nodes[2],sep='.')

    show.legend = F
    if(legend.include.edges) {
      show.legend = T;
    }

    node1 = edge$node1
    node2 = edge$node2

    edge_line <- edge$line
    edge_line$width <- edge_line$width * line_width

    trace_data <- rbind(trace_data, data.frame(
      X1 = c(node1[, x_axis], node2[, x_axis]),
      X2 = c(node1[, y_axis], node2[, y_axis]),
      X3 = c(node1[, z_axis], node2[, z_axis]),
      edge_line = list(edge_line),
      opacity = edge$opacity,
      showlegend = show.legend,
      name = edge_name
    ))
  }
  print('trace data:')
  print(trace_data)
  ena_plot <- add_trace(
    ena_plot,
    type = "scatter3d",
    mode = "lines",
    data = trace_data,
    x = ~X1, y = ~X2, z = ~X3,
    line = ~edge_line,
    opacity = ~opacity
  )

  ena_plot
}

