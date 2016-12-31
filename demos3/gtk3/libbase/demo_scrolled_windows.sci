// GtkScrolledWindow

function demo_scrolled_windows()
  dialog = gtk_dialog_new()
  //dialog.connect["delete_event", demo_delete];
  dialog.set_title["dialog"];
  scrolled_window= gtk_scrolled_window_new()
  scrolled_window.set_min_content_width[200];
  scrolled_window.set_min_content_height[200];
  scrolled_window.set_border_width[10]
  scrolled_window.set_policy[GTK.POLICY_ALWAYS, GTK.POLICY_ALWAYS]
  window_vbox = dialog.get_content_area[];
  window_vbox.pack_start[scrolled_window]
  scrolled_window.show[]
  table = gtk_grid_new();
  //table.set_row_spacings[10]
  //table.set_col_spacings[10]
  scrolled_window.add[table];
  table.show[]
  for i = 0:5
    for j = 0:5
      button = gtk_toggle_button_new(label=sprintf("button (%d,%d)",i,j));
      table.attach[button,i,j,1,1];
      button.show[]
    end
  end
  
  function y=dialog_configure (widget,event, data)
    allocation= widget.get_allocation[];
    y=%f;
    scrolled_window=data(1);
    scrolled_window.set_min_content_width[0];
    scrolled_window.set_min_content_height[0];
  endfunction;
  
  dialog.connect["configure-event",dialog_configure,list(scrolled_window)];
  dialog.add_button["size:(200x200)",3];
  dialog.add_button["min:(400x400)",2];
  dialog.add_button["Close",1];
  dialog.show_all[];
  while %t 
    response = dialog.run[]
    select response 
     case 1 then break;
     case 2 then 
      scrolled_window.set_min_content_width[400];
      scrolled_window.set_min_content_height[400];
    else 
      scrolled_window.set_min_content_width[0];
      scrolled_window.set_min_content_height[0];
      scrolled_window.get_window[].resize[200,200]
    end
  end
  dialog.destroy[];
endfunction
