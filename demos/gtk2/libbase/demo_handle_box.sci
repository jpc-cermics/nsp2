// GtkHandleBox


function handle_box_child_signal (hb,child,action)
  printf("signal: %s: child <%s> %s\n",type(hb,'string'),
         type(child,'string'),action(1));
	  
endfunction 

function demo_handle_box () 
  window = gtkwindow_new ();// GTK.WINDOW_TOPLEVEL);
  window.set_title[  "Handle Box Test"]
  window.set_resizable[  %f]
    
  //window.connect[  "destroy",hide]
      
  window.set_border_width[  20]

  vbox = gtkvbox_new(homogeneous=%f,spacing=0);
  window.add[  vbox]
  vbox.show[];

  label = gtklabel_new(str="Above");
  vbox.add[  label]
  label.show[];

  separator = gtkhseparator_new ();
  vbox.add[  separator]
  separator.show[];
    
  hbox = gtkhbox_new(homogeneous=%f,spacing=10);
  vbox.add[  hbox]
  hbox.show[];

  separator = gtkhseparator_new ();
  vbox.add[  separator]
  separator.show[];

  label = gtklabel_new(str="Below");
  vbox.add[  label]
  label.show[];

  handle_box = gtkhandlebox_new ();
  hbox.pack_start[ handle_box,expand=%f,fill=%f,padding=0]
  handle_box.connect[ "child_attached",  handle_box_child_signal,list( "attached")]
  handle_box.connect[ "child_detached",  handle_box_child_signal,list( "detached")]
  handle_box.show[];

  toolbar = make_toolbar (window);
  handle_box.add[  toolbar]
  toolbar.show[];
  handle_box = gtkhandlebox_new ();
  hbox.pack_start[ handle_box,expand=%f,fill=%f,padding=0]
  handle_box.connect[ "child_attached",  handle_box_child_signal,list(  "attached")]
  handle_box.connect[ "child_detached",  handle_box_child_signal,list(  "detached")]
  handle_box.show[];

  handle_box2 = gtkhandlebox_new ();
  handle_box.add[  handle_box2]
  handle_box2.connect[ "child_attached", handle_box_child_signal,list( "attached")]
  handle_box2.connect[ "child_detached", handle_box_child_signal, list( "detached")]
  handle_box2.show[];

  label = gtklabel_new(str="Fooo!");
  handle_box2.add[  label]
  label.show[];
  window.show[];
endfunction

