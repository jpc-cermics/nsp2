
function demo_main_window()

  // shoud be adapted for gtk3 
  // {'Image from drawable', "demo_image_from_drawable"}
  
  Demos={ 
      {'Buttons',  "demo_buttons"}
      {'Buttons: Check buttons',  "demo_check_buttons"}
      {'Buttons: Radio buttons',  "demo_radio_buttons"}
      {'Buttons: Toggle buttons',  "demo_toggle_buttons"}
      {'Cairo Drawing',  "demo_cairo"}
      {'Calendar',  "demo_calendar"}
      {'Cursor',  "demo_cursor"}
      {'Display and Screen', "demo_display_screen"}
      {'Dnd', "demo_dnd"}
      {'Entry',  "demo_entry"}
      {'File chooser',  "demo_file_chooser"}
      {'Flipping',  "demo_flipping"}
      {'Focus',  "demo_focus"}
      {'Gmenumodel', "demo_gmenumodel"}
      {'Images1', "demo_images1"}
      {'Images2', "demo_images2"}
      {'Images3', "demo_images3"}
      {'Labels', "demo_labels"}
      {'Messages', "demo_messagedialog"}
      {'Notebook',  "demo_notebook"}
      {'Nsp graphic widget 1', "demo_nsp_graphic_widget"}
      {'Nsp graphic widget 2', "demo_dyn_graph"}
      {'Picker (Color)',  "demo_color_picker"}
      {'Progress bar',  "demo_progressbar"}
      {'Range controls',  "demo_range_controls"}
      {'Reparent',  "demo_reparent"}
      {'Scrolled windows',  "demo_scrolled_windows"}
      {'Status bar',  "demo_statusbar"}
      {'Test idle', "demo_idle"}
      {'Test timeout', "demo_timeout"}
      {'Toolbar',  "demo_toolbar"}
      {'Tooltips', "demo_tooltips"}}
  win = gtk_window_new()
  win.set_title["main window"]
  win.set_size_request[200, 400]
  //win.set_uposition[20,20]
  //win.connect["destroy",hide];
  win.connect["delete_event", demo_delete];
  win.set_title["gtktest"];
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  scrolled_window = gtk_scrolled_window_new()
  scrolled_window.set_border_width[10]
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  box1.pack_start[scrolled_window, expand=%t,fill=%t,padding=0]
  scrolled_window.show[]
  box2 = gtk_box_new("vertical",spacing=0)
  box2.set_border_width[0]
  scrolled_window.add[box2];
  box2.show[];
  
  for i1 = 1:size(Demos,'*')
    button = gtk_button_new(label=Demos{i1}{1})
    ok = execstr(sprintf("func=%s;",Demos{i1}{2}),errcatch=%t);
    if ok && is(func,%types.PList) then
      button.connect["clicked",func,win];
    else
      button.set_sensitive[%f]
    end
    box2.pack_start[button]
    button.show[]
  end
  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand= %f,fill= %f,padding=0]
  separator.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,expand=%f,fill=%f,padding=0]
  box2.show[]
  button = gtk_button_new(label="close")
  button.connect["clicked",button_destroy_win,list(win)];
  //button.set_flags[GTK.CAN_DEFAULT]
  box2.pack_start[button]
  button.set_can_default[%t]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction

function [y]=demo_delete(win, event)
  // used when
  y=%f; // if false then destroy is performed
        // if true then destroy is not done
  if y==%t then
    // printf("in delete returning true (no destroy)\n");
  else
    // printf("in delete returning false (destroy)\n");
  end
endfunction

function destroy(win, event)
// called when window is destroyed
// printf("in destroy \n");
endfunction

function button_destroy_win(but,args)
  // button handler which destroy args(1)
  //printf("in win destroy \n");
  args(1).destroy[];
endfunction
