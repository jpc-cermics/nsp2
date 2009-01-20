// Drag and drop 
// note that these two functions don't use any global variables
// to communicate.  In fact, try openning two copies of
// testgtk.sci (or the C version) and drag between them.
// To be finished XXXX 

function []=dnd_message_box(title, message, button) 
    win = gtkdialog_new()
    win.set_title[title]; 
    win.connect[ "delete_event", demo_delete];
    //win.connect[ "destroy", do_quit];
    hbox = gtkhbox_new(homogeneous=%f,spacing=5);
    hbox.set_border_width[5]
    win.vbox.pack_start[hbox] 
    hbox.show[] 
    label = gtklabel_new(str=message) 
    hbox.pack_start[label]
    label.show[] 
    b = gtkbutton_new(label=button)
    b.set_flags[GTK.CAN_DEFAULT]
    b.connect["clicked",button_destroy_win,list(win)];
    win.action_area.pack_start[b]
    b.show[] 
    win.show[]
endfunction 

function dnd_drag_data_get(w, context, selection_data, info, time)
  dnd_string = "Bill Gates demands royalties for\n" + 
  "your use of his innovation."
  // methode set 
  selection_data.set[selection_data.target, 8,dnd_string]
endfunction 

function dnd_drag_data_received(w, context, x, y, data, info, time)
  if data.format == 8 
    msg = sprintf("Drop data of type %s was:\n\n%s",data.target.get_name[],data.data)
    dnd_message_box("Drop", msg,"Continue with life in\n" +
    "spite of this oppression")
  end
endfunction 

function demo_dnd() 
  targets = list(list('text/plain',GTK.TARGET_SAME_APP, 0))
  win = gtkwindow_new() 
  win.connect["delete_event", demo_delete];
  win.set_title["Drag -N- Drop"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  box2=	gtkhbox_new(homogeneous= %f,spacing=5)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  frame = gtkframe_new(label="Drag")
  box2.pack_start[frame]
  frame.show[]
  box3 = gtkvbox_new(homogeneous=%f,spacing=5)
  box3.set_border_width[5]
  frame.add[box3]
  box3.show[]
  button = gtkbutton_new(label="Drag me!")
  box3.pack_start[button]
  button.show[]
  button.connect['drag_data_get', dnd_drag_data_get];
  button.drag_source_set[ior(GDK.BUTTON1_MASK,GDK.BUTTON3_MASK),targets, GDK.ACTION_COPY]
  frame = gtkframe_new(label="Drop")
  box2.pack_start[frame]
  frame.show[]
  box3 = gtkvbox_new(homogeneous=%f,spacing=5)
  box3.set_border_width[5]
  frame.add[box3]
  box3.show[]
  button = gtkbutton_new(label="To")
  box3.pack_start[button]
  button.show[]
  button.realize[]
  button.connect['drag_data_received', dnd_drag_data_received];
  button.drag_dest_set[GTK.DEST_DEFAULT_ALL,targets,GDK.ACTION_COPY];
  separator = gtkhseparator_new()
  box1.pack_start[separator,expand= %f,fill=%t,padding=0]
  separator.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,fill=%t,padding=0]
  box2.show[]
  button = gtkbutton_new(label="close")
  button.connect["clicked", button_destroy_win,list(win) ];
  box2.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show[];
  // gtk_main()
endfunction 
