// buttons
//-----------------------------------------------

function demo_buttons()
  win = gtkwindow_new()
  win.connect[ "delete_event", hide];
  win.set_title["buttons"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0);
  win.add[box1];
  box1.show[]
  // mettre %f 
  // table = GtkTable(3, 3, %f)
  table = gtktable_new(rows=3,columns=3,homogeneous=%f) 
  table.set_row_spacings[5]
  table.set_col_spacings[5]
  table.set_border_width[10]
  box1.pack_start[table]
  table.show[]
  function []=toggle_show(button,args)
    if iand(args(1).flags[],GTK.VISIBLE) then
      args(1).hide[] 
    else 
      args(1).show[] 
    end	
  endfunction 

  buttons=list();
  for i= 0:8, 
    button = gtkbutton_new(mnemonic="button"+m2s(i,"_%0.f"))
    buttons($+1) = button; 
    [y,x] = divmod(i, 3)
    xoptions=ior(GTK.EXPAND,GTK.FILL)
    yoptions=ior(GTK.EXPAND,GTK.FILL)
    xpadding=0, ypadding=0
    table.attach[button,x,x+1,y,y+1,xoptions=xoptions,yoptions=yoptions,xpadding=0,ypadding=0]
    button.show[]
  end 
  for i= 1:9
    [y,x] = divmod(i+1,9);x=x+1;
    buttons(i).connect["clicked",toggle_show, list(buttons(x))];
  end
  separator = gtkhseparator_new() 
  box1.pack_start[separator,expand=%f,fill=%t,padding=0] 	
  separator.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10);	
  box2.set_border_width[10]
  box1.pack_start[box2,expand=%f,fill=%t,padding=0] 	
  box2.show[];
  button = gtkbutton_new(label="Close")
  button.connect["clicked",win_hide,list(win)];
  box2.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[];
  win.show[]
  //gtk_main()
endfunction
