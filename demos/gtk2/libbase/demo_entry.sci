//
// GtkEntry
// 

function demo_entry ()

  function return_handler(w,ev,data)
  // check if return was entered   
  // all the codes can be found in nsp2/src/gtk2/codegen/keysyms.sce
  if ev.keyval == 0xFF0D then 
    printf("Return pressed in entry: %s\n",w.get_text[]);
  end
  endfunction
  
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
  
  
  flags = ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),
  window = gtkdialog_new(title= "Entry demo",flags = flags,...
			 buttons = ["gtk-ok","gtk-cancel"]);
  ok_rep = 1; // buttons return code is their indice in buttons matrix
    
  // window.connect[  "destroy",gtk_widget_destroyed, &window]
  // window.vbox.pack_start[hbox,expand=%f,fill=%f,padding=0]
  box1 = window.vbox;
    
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
  entry.connect["key_press_event",return_handler];
  hbox.pack_start[ entry,expand=%t,fill=%t,padding=0]
  
  button = gtkbutton_new(mnemonic="_Props");
  hbox.pack_start[ button,expand=%f,fill=%f,padding=0]
  button.connect[  "clicked", entry_props_clicked, list(entry) ]

  cb = gtkcombo_new ();
  cbitems = [ "item0", "item1 item1",  "item2 item2 item2"]; 
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
      
  window.show_all[];
  // treeview.columns_autosize[];
  // a modal window undestroyed at end of run. 
  response = window.run[];
  if response == ok_rep; 
    // GTK.RESPONSE_OK 
    // print the entry ? 
  end
  window.destroy[];
endfunction 

