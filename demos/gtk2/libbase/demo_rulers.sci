
// Rulers 
// Les handlers marchent pas XXXXXX 

function demo_rulers()
  win = gtkwindow_new()
  win.connect["delete_event", demo_delete];
  win.set_title["rulers"];
  win.set_size_request[300, 300]
  win.set_events[ior(GDK.POINTER_MOTION_MASK,GDK.POINTER_MOTION_HINT_MASK)]
  table = gtktable_new(rows=2,columns=3,homogeneous=%f) 
  win.add[table]
  table.show[]
  xruler = gtkhruler_new()
  xruler.set_range[5, 15, 0, 20]
  //	ruler.set_metric(PIXELS)
  xruler.set_metric[GTK.PIXELS]
  
  function [y]=motion_notify(obj, event)
    // pause
    // y = ruler.emit("motion_notify_event", event)
    // y = gtk_signal_emitv_by_name(xruler,"motion_notify_event",list( event))
  endfunction 
  win.connect["motion_notify_event", motion_notify];
  //	table.attach(ruler, 1,2, 0,1, yoptions=GTK.FILL)
  xoptions=ior(GTK.EXPAND,GTK.FILL),  yoptions=GTK.FILL, 
  table.attach[xruler,1,2,0,1,xoptions=xoptions,yoptions=yoptions];
  xruler.show[]
  yruler = gtkvruler_new()
  yruler.set_range[5, 15, 0, 20]
  yruler.set_metric[GTK.PIXELS];
  
  function [y]=motion_notify(obj, event) // ruler=ruler)
    y=1
    //return ruler.emit("motion_notify_event", event)
    y=gtk_signal_emitv_by_name(yruler,"motion_notify_event",list( event))
  endfunction
  win.connect["motion_notify_event", motion_notify];
  xoptions=GTK.FILL,  yoptions=ior(GTK.EXPAND, GTK.FILL), 
  table.attach[yruler,0,1,1,2,xoptions=xoptions,yoptions=yoptions];
  yruler.show[]
  label = gtklabel_new(str = "The rulers now work!\n" +
  "They use the new interface to\n" +
  "gtk_signal_emit.")
  xoptions=ior(GTK.EXPAND,GTK.FILL),  yoptions=xoptions;
  table.attach[label,1,2,1,2,xoptions=xoptions,yoptions=yoptions];
  label.show[]
  win.show[]
  //gtk_main()
endfunction
