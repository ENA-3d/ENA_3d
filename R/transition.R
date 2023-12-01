get_transition_list <- function(first_group_mean,second_group_mean,steps){
  transition_list =list()
  for(i in 1:length(first_group_mean)){
    s = seq(first_group_mean[i],second_group_mean[i],length.out=steps)
    transition_list <- append(transition_list,list(s))
  }
  transition_list
}
get_transition_matrix <- function(transition_list){
  transition_matrix <- do.call("cbind",transition_list)
  transition_matrix
}
# steps = 30
# transition_list =list()
# for(i in 1:length(fgm)){
#   s = seq(fgm[i],sgm[i],length.out=steps)
#   transition_list <- append(transition_list,list(s))
# }
# length(transition_list)
get_mean_group_lineweights <- function(ena_obj,groupVar,group_name){
  # ena_line_weights = as.data.frame(ena_obj$line.weights)
  # group_lineweights = ena_line_weights[which(ena_line_weights[,groupVar]==group_name),]
  group_lineweights = ena_obj$line.weights[get(groupVar)==group_name]

  group_lineweights = as.matrix(group_lineweights)

  mean_group_lineweights = as.vector(colMeans(group_lineweights))
  mean_group_lineweights

}
get_mean_group_lineweights_in_groups <- function(ena_obj,groupVar,group_names){
  # ena_line_weights = as.data.frame(ena_obj$line.weights)
  # group_lineweights = ena_line_weights[which(ena_line_weights[,groupVar]==group_name),]
  group_lineweights = ena_obj$line.weights[which(ena_obj$line.weights[[groupVar]] %in% group_names)]
  print('group_lineweights')
  print(group_lineweights)
  group_lineweights = as.matrix(group_lineweights)
  
  mean_group_lineweights = as.vector(colMeans(group_lineweights))
  mean_group_lineweights

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
