
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

  dialog = gtk_dialog_new();
  dialog.set_title["dialog"];
  dialog_vbox = dialog.get_content_area[];
  label = gtk_label_new(str="Insert a Nsp graphic window in a dialog");
  dialog_vbox.pack_start[label, expand=%f,fill= %f,padding=5];
  id1=nsp_graphic_new(dialog,dialog_vbox,dim=[600,400]);
  plot3d1();
  dialog.add_button["Close",1];
  dialog.show_all[];
  response = dialog.run[];
  dialog.destroy[];
endfunction
