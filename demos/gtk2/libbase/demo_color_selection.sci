// GtkColorSelection

function demo_color_selection ()

  function color_selection_ok (w,csd) 
    color = csd(1).colorsel.get_current_color[];
    printf("color rgb= [%d,%d,%d] pixel=%d\n",color.red,color.green,color.blue,color.pixel);
  endfunction 

  function color_selection_changed (w,csd) 
    color = csd(1).colorsel.get_current_color[];
  endfunction 
  
  function opacity_toggled_cb (w,csd) 
    csd(1).colorsel.set_has_opacity_control [ w.get_active[]];
  endfunction

  function palette_toggled_cb (w,csd) 
    csd(1).colorsel.set_has_palette[w.get_active[]];
  endfunction 

  window = gtkcolorselectiondialog_new ("color selection dialog");
  window.help_button.show[];
  window.set_position[  GTK.WIN_POS_MOUSE]
  // window.connect["destroy",hide];
  
  options_hbox = gtkhbox_new(homogeneous=%f,spacing=0);
  window.vbox.pack_start[ options_hbox,expand=%f,fill=%f,padding=0];
  options_hbox.set_border_width[  10]
      
  check_button = gtkcheckbutton_new(label="Show Opacity");
  options_hbox.pack_start[ check_button,expand=%f,fill=%f,padding=0]
  check_button.connect[  "toggled", opacity_toggled_cb,list(window)]

  check_button = gtkcheckbutton_new(label="Show Palette");
  options_hbox.pack_end[  check_button,expand=%f,fill=%f,padding=0]
  check_button.connect[  "toggled",palette_toggled_cb,list( window) ]

  window.colorsel.connect["color_changed", color_selection_changed,	list(window)];
  window.ok_button.connect["clicked",color_selection_ok,list(window)];
  window.cancel_button.connect["clicked",button_destroy_win,list(window)];
  window.show_all[];
endfunction


