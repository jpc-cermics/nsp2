// Buttons

function win=demo_buttons(do_widget)
// 
  win = gtk_window_new()
  win.connect[ "delete_event", demo_delete];
  win.set_title["buttons"];
  box1 = gtk_box_new("vertical",spacing=0);
  win.add[box1];
  box1.show[]
  table = gtk_grid_new();
  table.set_row_spacing[5]
  table.set_column_spacing[5]
  table.set_border_width[10]
  box1.pack_start[table]
  table.show[]
  
  function []=toggle_show(button,args)
    button1 = args(1);
    if button1.get_visible[] then 
      button1.hide[]
    else
      button1.show[]
    end
  endfunction

  function [y,x]=divmod(i,j)
  // i= y*j + x
    y=idiv(i,j)
    x=modulo(i,j)
  endfunction

  buttons=list();
  for i= 0:8,
    button = gtk_button_new(mnemonic="button"+m2s(i,"_%0.f"))
    buttons($+1) = button;
    [y,x] = divmod(i, 3);
    if %t then
      table.attach[button,x,y,1,1];
    else
      xoptions=ior(GTK.EXPAND,GTK.FILL)
      yoptions=ior(GTK.EXPAND,GTK.FILL)
      xpadding=0, ypadding=0
      table.attach[button,x,x+1,y,y+1,xoptions=xoptions,yoptions= ...
		   yoptions,xpadding=0,ypadding=0]
    end
    button.show[]
  end
  for i= 1:9
    [y,x] = divmod(i+1,9);x=x+1;
    buttons(i).connect["clicked",toggle_show, list(buttons(x))];
  end
  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand=%f,fill=%t,padding=0]
  separator.show[]
  box2 = gtk_box_new("vertical",spacing=10);
  box2.set_border_width[10]
  box1.pack_start[box2,expand=%f,fill=%t,padding=0]
  box2.show[];
  button = gtk_button_new(label="Close")
  button.connect["clicked",button_destroy_win,list(win)];
  box2.pack_start[button]
  button.set_can_default[%t]
  button.grab_default[]
  button.show[];
  win.show[]
endfunction
