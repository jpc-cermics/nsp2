// GtkFileSelection 
//-------------------------------------------------------

function show_fileops (widget,fs) 
  show_ops = widget.get_active[];
  if show_ops
    fs(1).show_fileop_buttons[];
  else
    fs(1).hide_fileop_buttons[];
  end
endfunction 
    
function select_multiple (widget,fs)
  select_multiple = widget.get_active[];
  fs(1).set_select_multiple[ select_multiple];
endfunction 

function file_selection_ok (button,fs) 
  selections = fs(1).get_selections[];
  for i=1:size(selections,0) 
    printf("file : %s selected\n",selections(i));
  end
endfunction

function demo_file_selection ()
  window = gtkfileselection_new (title="file selection dialog");
  window.hide_fileop_buttons[];

  window.set_position[  GTK.WIN_POS_MOUSE]
  window.connect[  "destroy",hide];

  window.ok_button.connect["clicked", file_selection_ok,list(window)]
  window.cancel_button.connect["clicked", win_hide, list(window)]
  button = gtkcheckbutton_new(label="Show Fileops");
  button.connect[  "toggled",  show_fileops,list(  window)];
  window.action_area.pack_start[ button,expand=%f,fill=%f,padding=0];
  button.show[];
  
  button = gtkcheckbutton_new(label="Select Multiple");
  button.connect[  "clicked",  select_multiple,list(window)]
  window.action_area.pack_start[ button,expand=%f,fill=%f,padding=0];
  button.show[];
  window.show[];
endfunction 
