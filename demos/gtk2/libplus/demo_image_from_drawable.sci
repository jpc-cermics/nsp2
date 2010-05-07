// Test for getting an image from a drawable
// ------------------------------------------

function take_snapshot (button, data)
//data= list(sw1,src,snap)]
// snap: image to fill
// sw1 : the scrolled window which contains the drawiing are 
// src : a drawing area 
  visible = data(1).allocation;
  xh = data(1).get_hadjustment[].value;
  yv = data(1).get_vadjustment[].value;
  printf("rectangle visible %d %d %d %d\n",visible.x , visible.y, visible.width, visible.height);
  black_gc = data(2).style.black_gc;

  // Screen shot area 

  target=[ visible.x+xh , visible.y+yv , visible.width, visible.height];
  target1 = target + [ visible.width/5,visible.height/5,- 2* visible.width/5, - 2* visible.height/5];

  printf("rectangle target1 %d %d %d %d\n",target1(1) , target1(2), target1(3), target1(4));

  shot = data(2).window.get_image[target1(1), target1(2),target1(3), target1(4)];
  data(3).set_from_image[gdk_image=shot,mask=none_create()];

  //  data(2).window.begin_paint_rect[target];
  data(2).window.draw_rectangle[ black_gc, %f, target1(1), target1(2), target1(3), target1(4)];
  //  data(2).window.end_paint[];
endfunction 

function ret= image_source_expose (da,event, data)
  // expose event handler for the drawing area 
  x = event.area.x;
  blue = gdkcolor_new(0,0,65535, 0)
  gc = gdkgc_new(event.window);
  gc.set_rgb_fg_color[blue];
  event.window.draw_rectangle[gc,%t,event.area.x,event.area.y,event.area.width,event.area.height];
  // draw data 
  for i=1:data(1) 
    rgb=data(3)(i,:);
    gc.set_rgb_fg_color[gdkcolor_new(rgb(1),rgb(2),rgb(3),0)];
    r= data(2)(i,:)
    event.window.draw_rectangle[gc,%t,r(1), r(2), r(3), r(4)];
  end
  ret=%t
endfunction

function demo_image_from_drawable()
  window = gtkwindow_new();
  // window.set_screen[  widget.get_screen[]]
  //window.connect[ "destroy", gtk_widget_destroyed];
      
  vbox = gtkvbox_new(homogeneous=%f,spacing=0);
  window.add[  vbox]
  sw1 = gtkscrolledwindow_new ();
  sw1.set_policy[ GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC]

  sw1.set_size_request[  400, 400]
  src = gtkdrawingarea_new ();
  w=800
  src.set_size_request[w,w]

  n=100;
  rects=[rand(n,2,'u')*w,rand(n,2,'u')*50+20];
  rgb= rand(n,3,'u')*65535; 

  src.connect["expose_event", image_source_expose,list(n,rects,rgb)]

  sw1.add_with_viewport[ src]
  vbox.pack_start[ sw1,expand=%t,fill=%t,padding=0]                          

  hbox = gtkhbox_new(homogeneous=%f,spacing=3);
  snap = gtkimage_new()

  sw = gtkscrolledwindow_new();
  sw.set_policy[GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC]
  sw.set_size_request[300,300]
      
  sw.add_with_viewport[snap]
  hbox.pack_end[  sw,expand=%f,fill=%f,padding=5]                          

  button = gtkbutton_new(label="Get image from drawable");
  button.connect[ "clicked", take_snapshot,list(sw1,src,snap)]
      
  hbox.pack_start[ button,expand=%f,fill=%f,padding=0];
  vbox.pack_end[  hbox,expand=%f,fill=%f,padding=5] ;
  window.show_all[];
endfunction 

