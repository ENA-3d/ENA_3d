get_cvar = function(){
  RES =   as.formula(paste('~',I("'Science Performance'")))
  print(RES)
  RES
}
main_plot = plot_ly()
main_plot = add_trace(main_plot,
                      data = as.data.frame(ena_obj$points),
                      x = ~MR1,
                      y = ~SVD2,
                      z = ~SVD3,
                      color = ~'Science Performance',
                      colors=randomColor(length(unique(ena_obj$points$`Science Performance`))),
                      type = 'scatter3d',
                      mode = "markers",
                      # name = "Points",
                      marker = list(
                        size = 5,
                        line = list(
                          width = 0
                        )
                        # ,name = labels[i] #rownames(nodes)[i]
                      ))
print(main_plot)