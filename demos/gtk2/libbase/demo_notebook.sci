
function demo_notebook()

  function next_page(b,args)
    args(1).next_page[];
  endfunction 

  function prev_page(b,args)
    args(1).prev_page[];
  endfunction 

  function rotate_notebook(b,args)
    notebook=args(1);
    notebook.set_tab_pos[notebook.get_tab_pos[]+1];
  endfunction 
  
  win = gtkwindow_new()
  win.connect["delete_event",demo_delete];
  win.set_title["notebook"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  notebook = gtknotebook_new()
  notebook.set_tab_pos[GTK.POS_TOP];
  box2.pack_start[notebook]
  notebook.show[]
  for i = 0:4
    frame = gtkframe_new(label=sprintf("Page %d",i));
    frame.set_border_width[10]
    frame.set_size_request[200, 150]
    frame.show[]
    label = gtklabel_new(str=sprintf("Page %d",i));
    frame.add[label]
    label.show[]
    label = gtklabel_new(str=sprintf("Tab %d",i));
    notebook.append_page[frame,label];
  end
  separator = gtkhseparator_new()
  box1.pack_start[separator,expand= %f,fill=%t,padding=0]
  separator.show[]
  box2=	gtkhbox_new(homogeneous= %t,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,fill=%t,padding=0]
  box2.show[]
  button = gtkbutton_new(label="close")
  button.connect["clicked", button_destroy_win,list(win)];
  box2.pack_start[button]
  button.show[]
  button = gtkbutton_new(label="next")
  button.connect["clicked", next_page,list(notebook)];
  box2.pack_start[button]
  button.show[]
  button = gtkbutton_new(label="prev")
  button.connect["clicked", prev_page,list(notebook)];
  box2.pack_start[button]
  button.show[]
  button = gtkbutton_new(label="rotate")
  button.connect["clicked", rotate_notebook,list(notebook)];
  box2.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction
