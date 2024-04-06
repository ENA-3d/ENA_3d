dependencies_list = c('shiny',
                      'plotly',
                      'collapse',
                      'shinyjs',
                      'R6',
                      'shinyWidgets',
                      'rENA',
                      'coin',
                      'shinyfullscreen',
                      'shinytest',
                      'shiny.fluent',
                      'colourpicker'
                      )

for(i in 1:length(dependencies_list)){
  pkg_name = dependencies_list[i]
  is_pkg_available <- system.file(package = pkg_name)!=''
  if(!is_pkg_available){
    install.packages(pkg_name)
  }else{
    print(paste0(pkg_name,' is available'))
  }
}
