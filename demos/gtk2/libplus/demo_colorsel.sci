// Color Selector
// GtkColorSelection lets the user choose a color. GtkColorSelectionDialog is
// a prebuilt dialog containing a GtkColorSelection.


function demo_colorsel () 

  function y = expose_event_callback (widget,event,data) 
    style = widget.get_style[];
    widget.window.draw_rectangle[style.get_bg_gc[GTK.STATE_NORMAL], %t, event.area.x, event.area.y,
		    event.area.width, event.area.height];
    y=%t ;
  endfunction

  function change_color_callback (button,args) 
    window=args(1);
    da=args(2);
    color=window.get_data['color'];
    dialog = gtkcolorselectiondialog_new ("Changing color");
    dialog.set_transient_for[args(1)]
    colorsel = dialog.colorsel; 
    
    colorsel.set_previous_color[color];
    colorsel.set_current_color[color];
    colorsel.set_has_palette[%t]; 
    response = dialog.run[];
    if response == GTK.RESPONSE_OK 
      color= colorsel.get_current_color[]
      window.set_data[color=color];
      args(2).modify_bg[ GTK.STATE_NORMAL,color]
    end
    dialog.destroy[];
  endfunction 
  
  color= gdkcolor_new(0,0,65535,0); 
  window = gtkwindow_new ();
  window.set_title[  "Color Selection"]

  //window.connect[  "destroy", hide];
  window.set_border_width[  8]

  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]
  window.set_data[color=color];
    
  // Create the color swatch area
      
  frame = gtkframe_new();
  frame.set_shadow_type[GTK.SHADOW_IN];
  vbox.pack_start[ frame,expand=%t,fill=%t,padding=0]

  da = gtkdrawingarea_new ();
  da.connect[  "expose_event",expose_event_callback]

  // set a minimum size */
  da.set_size_request[  200, 200]
  // set the color */
  da.modify_bg[  GTK.STATE_NORMAL,color]
      
  frame.add[  da]

  alignment = gtkalignment_new(xalign=1.0,yalign=0.5,xscale=0.0,yscale=0.0);
      
  button = gtkbutton_new(mnemonic="_Change the above color");
  alignment.add[  button]
      
  vbox.pack_start[ alignment,expand=%f,fill=%f,padding=0]
      
  button.connect[  "clicked",change_color_callback,list(window,da)];
  window.show_all[];
  
endfunction
