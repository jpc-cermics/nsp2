//
// GtkEntry
// 

function demo_entry ()
  
  function entry_toggle_frame (checkbutton,args)
    args(1).set_has_frame[ checkbutton.get_active[]]
  endfunction
  
  function entry_toggle_sensitive (checkbutton,args)
    args(1).set_sensitive[  checkbutton.get_active[]]
  endfunction

  function entry_props_clicked (button,entry) 
   // window = create_prop_editor (entry, 0);
   // window.set_title[  "Entry Properties"]
  endfunction 
  
  cbitems = [ "item0", "item1 item1",  "item2 item2 item2"]; 
  
  window = gtkwindow_new ();//GTK.WINDOW_TOPLEVEL);
  // window.connect[ "destroy",hide]
  
  window.set_title[  "entry"]
  window.set_border_width[  0]


  box1 = gtkvbox_new(homogeneous=%f,spacing=0);
  window.add[  box1]


  box2 = gtkvbox_new(homogeneous=%f,spacing=10);
  box2.set_border_width[  10]
  box1.pack_start[ box2,expand=%t,fill=%t,padding=0]

  hbox = gtkhbox_new(homogeneous=%f,spacing=5);
  box2.pack_start[ hbox,expand=%t,fill=%t,padding=0]
      
  entry = gtkentry_new ();
  // Utf8 string 
  str= "Utf8 string:\330\247\331\204\330\263\331\204\330\247\331\205\330\271\331\204\331\212\331\203\331\205";
  entry.set_text[str]
  
  entry.select_region[ 0, 5];
  hbox.pack_start[ entry,expand=%t,fill=%t,padding=0]

  button = gtkbutton_new(mnemonic="_Props");
  hbox.pack_start[ button,expand=%f,fill=%f,padding=0]
  button.connect[  "clicked", entry_props_clicked, list(entry) ]

  cb = gtkcombo_new ();
  cb.set_popdown_strings[cbitems];
  cb.entry.set_text["hello world \n\n\n foo"];
  cb.entry.select_region[0, -1];
  box2.pack_start[ cb,expand=%t,fill=%t,padding=0]

  sensitive_check = gtkcheckbutton_new(label="Sensitive");
  box2.pack_start[ sensitive_check,expand=%f,fill=%t,padding=0]
  sensitive_check.connect[  "toggled",	entry_toggle_sensitive, list(entry)]
  sensitive_check.set_active[  %t]

  has_frame_check = gtkcheckbutton_new(label="Has Frame");
  box2.pack_start[ has_frame_check,expand=%f,fill=%t,padding=0]
  has_frame_check.connect[  "toggled",	entry_toggle_frame,list(entry)]
  has_frame_check.set_active[  %t]
      
  separator = gtkhseparator_new ();
  box1.pack_start[ separator,expand=%f,fill=%t,padding=0]

  box2 = gtkvbox_new(homogeneous=%f,spacing=10);
  box2.set_border_width[  10]
  box1.pack_start[ box2,expand=%f,fill=%t,padding=0]

  button = gtkbutton_new(label="close");
  // button.connect[ "clicked", win_hide,list(window)];
  box2.pack_start[ button,expand=%t,fill=%t,padding=0]
  button.set_flags[GTK.CAN_DEFAULT];
  button.grab_default[];
  window.show_all[];
endfunction 

