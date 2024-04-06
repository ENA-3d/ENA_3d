camera = reactive({
  pos = input$camera_position
  print(pos)
  if(pos =='default'){
    camera = list(eye=list(x=1.25, y=1.25, z=1.25))
  }else if(pos =='x_y'){
    camera = list(eye=list(x=0, y=0, z=2.5),up=list(x=0,y=1,z=0))
  }else if(pos == 'x_z'){
    camera = list(eye=list(x=0., y=-2.5, z=0.),up=list(x=0,y=0,z=1))
  }else if(pos =='y_z'){
    camera = list(eye=list(x=2.5, y=0., z=0.),up=list(x=0,y=0,z=1))
  }else if(pos =='y_x'){
    camera = list(eye=list(x=0, y=0, z=-2.5),up=list(x=1,y=0,z=0))
  }else if(pos =='z_x'){
    camera = list(eye=list(x=0, y=2.5, z=0),up=list(x=1,y=0,z=0))
  }else if(pos =='z_y'){
    camera = list(eye=list(x=-2.5, y=0, z=0),up=list(x=0,y=1,z=0))
  }
  # list(eye=camera_eye(),up=list(x=0,y=1,z=0))
})