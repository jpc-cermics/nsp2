
function [id1,id2]=demo_nsp_graphic_widget()
// Nsp Graphic window inside a gtk widget 

  function [y]=delete(win, event) 
    rep=x_message('Really kill',['Yes','No'])
    if rep == 1 then 
      y=%f;
    else
      y=%t;
    end
  endfunction
    
  win = gtkdialog_new()
  // win.connect["delete_event", delete];
  win.set_title["dialog"];
  id1=nsp_graphic_new(win,win.vbox,dim=[300,200]);
  plot3d1();
  id2=nsp_graphic_new(win,win.vbox,opengl=%t,dim=[300,200]);
  plot3d1()
  button = gtkbutton_new(label="close")
  button.connect["clicked",button_destroy_win,list(win)];
  win.action_area.pack_start[button]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show_all[] // must be sure that graphic window is realized 
endfunction
