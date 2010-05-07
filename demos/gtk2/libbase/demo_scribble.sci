// this is a simple translation of the scribble example that comes with GTK+

function [rep]=scr_configure_event(widget, event)
  win = widget.window;
  wh=win.get_size[];
  pixmap= gdkpixmap_new(drawable=win,width=wh(1),height=wh(2),depth= -1)	
  // attach the pixmap to the widget 
  widget.set_data[pixmap=pixmap]; 
  //draw_rectangle(pixmap, widget.get_style().white_gc, %t,
  //	       0, 0, win.width, win.height)
  pixmap.draw_rectangle[ widget.style.white_gc,%t,0,0,wh(1),wh(2)];
  rep=%f;
endfunction

function [rep]=scr_expose_event(widget, event)
  area = event.area
  gc = widget.style.get_fg_gc[GTK.STATE_NORMAL]
  pixmap= widget.get_data['pixmap'];
  widget.window.draw_drawable[gc, pixmap,area.x, area.y,area.x, area.y, area.width,area.height]
  rep= %f;
endfunction

function scr_draw_brush(widget, x, y)
  gc = widget.style.black_gc ;
  pixmap= widget.get_data['pixmap'];
  pixmap.draw_rectangle[gc,%t, x-1, y-1, 2, 2]; 
  widget.queue_draw[];
endfunction

function [rep]=scr_button_press_event(widget, event)
  pixmap= widget.get_data['pixmap'];
  if event.button == 1 then 
    scr_draw_brush(widget, event.x, event.y);
  end
  rep=%t
endfunction

function [rep]=scr_motion_notify_event(widget, event)
  if event.is_hint then
    ptr = event.window.get_pointer[]
    x=ptr(1);y=ptr(2);state = ptr(3);
  else
    x = event.x; y = event.y; state = event.state
  end 
  pixmap= widget.get_data['pixmap'];
  if iand(state,GDK.BUTTON1_MASK)
    scr_draw_brush(widget,x,y);
  end 
  rep=%t
endfunction

function demo_scribble()
  win = gtkwindow_new();
  win.set_name["Test Input"]
  //win.connect["destroy",hide];
  win.set_border_width[10]
  vbox = gtkvbox_new(homogeneous=%f,spacing=3);
  win.add[vbox];
  vbox.show[]
  drawing_area = gtkdrawingarea_new() 
  drawing_area.set_size_request[200,200];
  vbox.pack_start[drawing_area,expand=%f,fill=%t,padding=0]
  drawing_area.show[]
  drawing_area.connect["expose_event", scr_expose_event];
  drawing_area.connect["configure_event", scr_configure_event];
  drawing_area.connect["motion_notify_event", scr_motion_notify_event];
  drawing_area.connect["button_press_event", scr_button_press_event];
  ev=ior([GDK.EXPOSURE_MASK;GDK.LEAVE_NOTIFY_MASK;GDK.BUTTON_PRESS_MASK;
	  GDK.POINTER_MOTION_MASK;GDK.POINTER_MOTION_HINT_MASK]);
  drawing_area.set_events[ev];
  button = gtkbutton_new(label="Quit")
  vbox.pack_start[button,expand= %f,fill=%f,padding=0]
  button.connect["clicked",button_destroy_win,list(win)];
  button.show[]
  win.show[]
endfunction 
	
