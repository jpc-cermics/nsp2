function frame=create_bbox(horizontal,title,spacing)
// create a frame with three stock buttons 
  layout = 'GTK_BUTTONBOX_'+toupper(title);
  frame = gtkframe_new (label=title);
  if (horizontal)
    bbox = gtkhbuttonbox_new();
  else
     bbox = gtkvbuttonbox_new();
  end
  bbox.set_border_width[5];
  frame.add[bbox];
  
  bbox.set_layout[layout];
  bbox.set_spacing[spacing];

  for st=["gtk-ok","gtk-cancel","gtk-help"]
    button = gtkbutton_new(stock=st);
    bbox.add[button];
  end
endfunction

function box_test() 
  window = gtkwindow_new();
  window.set_title["Button Boxes"]
  // window.connect["destroy",...]
  window.set_border_width[10];
  main_vbox = gtkvbox_new(homogeneous= %t ,spacing = 0);
  window.add[main_vbox];
  frame_horz = gtkframe_new(label="Horizontal Button Boxes");
  main_vbox.pack_start[frame_horz, expand=%t ,fill= %t ,padding= 10];
  vbox = gtkvbox_new(homogeneous= %t ,spacing = 0);
  vbox.set_border_width[10];
  frame_horz.add[vbox];
  vbox.pack_start[create_bbox(%t, "Spread", 40),padding= 0];
  vbox.pack_start[create_bbox(%t, "Edge", 40),padding= 5];
  vbox.pack_start[create_bbox(%t, "Start", 40),padding= 5];
  vbox.pack_start[create_bbox(%t, "End", 40),padding= 5];
  frame_vert = gtkframe_new (label="Vertical Button Boxes");
  main_vbox.pack_start[frame_vert, padding=10];
  hbox = gtkhbox_new (homogeneous= %t ,spacing = 0);
  hbox.set_border_width[10];
  frame_vert.add[hbox];
  hbox.pack_start[create_bbox(%f, "Spread", 30),padding= 0];
  hbox.pack_start[create_bbox(%f, "Edge", 30), padding=5];
  hbox.pack_start[create_bbox(%f, "Start", 30),padding= 5];
  hbox.pack_start[create_bbox(%f, "End", 30),padding= 5];
  window.show_all[];
endfunction


