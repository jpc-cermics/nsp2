
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
  
  win = gtk_window_new()
  win.connect["delete_event",demo_delete];
  win.set_title["notebook"];
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  notebook = gtk_notebook_new()
  notebook.set_tab_pos[GTK.POS_TOP];
  box2.pack_start[notebook]
  notebook.show[]
  for i = 0:4
    frame = gtk_frame_new(label=sprintf("Page %d",i));
    frame.set_border_width[10]
    frame.set_size_request[200, 150]
    frame.show[]
    label = gtk_label_new(str=sprintf("Page %d",i));
    frame.add[label]
    label.show[]
    label = gtk_label_new(str=sprintf("Tab %d",i));
    notebook.append_page[frame,label];
  end
  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand= %f,fill=%t,padding=0]
  separator.show[]
  box2=	gtk_box_new("horizontal",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,fill=%t,padding=0]
  box2.show[]
  button = gtk_button_new(label="close")
  button.connect["clicked", button_destroy_win,list(win)];
  box2.pack_start[button]
  button.show[]
  button = gtk_button_new(label="next")
  button.connect["clicked", next_page,list(notebook)];
  box2.pack_start[button]
  button.show[]
  button = gtk_button_new(label="prev")
  button.connect["clicked", prev_page,list(notebook)];
  box2.pack_start[button]
  button.show[]
  button = gtk_button_new(label="rotate")
  button.connect["clicked", rotate_notebook,list(notebook)];
  box2.pack_start[button]
  //button.set_flags[GTK.CAN_DEFAULT]
  button.set_can_default[%t]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction
