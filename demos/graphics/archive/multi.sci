// Nsp Graphihc window inside a gtk widget 

function [y]=delete(win, event) 
  rep=x_message('Really kill',['Yes','No'])
  if rep == 1 then 
    y=%f;
  else
    y=%t;
  end
endfunction

function [y]=win_delete(but,args)  
  args(1).destroy[];y=%t; 
endfunction

function create_graphics()
  win = gtkdialog_new()
  win.connect["delete_event", delete];
  win.set_title["dialog"];
  nsp_graphic_new(win,win.vbox,dim=[300,200]);
  nsp_graphic_new(win,win.vbox,dim=[300,200]);
  button = gtkbutton_new(label="close")
  button.connect["clicked", win_delete,list(win)];
  win.action_area.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show_all[] // must be sure that graphic window is realized 
endfunction
