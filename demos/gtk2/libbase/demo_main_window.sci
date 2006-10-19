
function demo_main_window()
  B = hash_create(100);
  B('button box')=  demo_button_box,
  B('buttons')=  demo_buttons,
  B('toggle buttons')=  demo_toggle_buttons,
  B('check buttons')=  demo_check_buttons,
  B('radio buttons')=  demo_radio_buttons,
  B('calendar')=  demo_calendar,
  B('toolbar')=  demo_toolbar,
  B('handle box')= demo_handle_box,
  B('reparent')=  demo_reparent,
  B('tooltips')= demo_tooltips,
  B('menus')=  demo_menus,
  B('scrolled windows')=  demo_scrolled_windows,
  B('drawing areas')= [],// None,
  B('entry')=  demo_entry,
  B('color selection')=  demo_color_selection,
  B('file selection')=  demo_file_selection,
  B('flipping')=  demo_flipping,
  B('focus')=  demo_focus,
  B('dialogs')=  demo_dialogs,
  B('messages') = demo_messagedialog,
  B('miscellaneous')= [],// None,
  B('range controls')=  demo_range_controls,
  B('rulers')= demo_rulers,
  B('notebook')=  demo_notebook,
  B('panes')=  demo_panes,
  B('shapes')= [],// None, //demo_shapes,
  B('dnd')= demo_dnd,
  B('spinbutton')= demo_spinbutton,
  B('progress bar')=  demo_progressbar,
  B('gamma curve')=  demo_gamma_curve,
  B('test scrolling')= [],// None, //creaate_scroll_text,
  B('test selection')= [],// None, //demo_selection_test,
  B('test timeout')= demo_timeout,
  B('test idle')= demo_idle,
  B('test')= [],// None, //demo_test
  B('status bar')=  demo_statusbar,
  B('cursor')=  demo_cursor,
  B('scribble')=  demo_scribble, 
  win = gtkwindow_new()
  win.set_title["main window"]
  win.set_size_request[200, 400]	
  //win.set_uposition[20,20]
  //win.connect["destroy",hide];
  win.connect["delete_event", demo_delete];
  win.set_title["gtktest"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  scrolled_window = gtkscrolledwindow_new()
  scrolled_window.set_border_width[10]
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  box1.pack_start[scrolled_window, expand=%t,fill=%t,padding=0] 	
  scrolled_window.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=0)
  box2.set_border_width[0]
  scrolled_window.add_with_viewport[box2];
  box2.show[]
  k = B.__keys;
  Bk = sort(k,'g','i');
  for i1 = 1:size(Bk,1)
    button = gtkbutton_new(label=Bk(i1,1))
    func= B.find[Bk(i1,1)]
    if is(func,%types.PList) then 
      button.connect["clicked",func];
    else
      button.set_sensitive[%f]
    end 
    box2.pack_start[button]
    button.show[]
  end 
  separator = gtkhseparator_new()
  box1.pack_start[separator,expand= %f,fill= %f,padding=0]
  separator.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,expand=%f,fill=%f,padding=0]
  box2.show[]
  button = gtkbutton_new(label="close")
  button.connect["clicked",button_destroy_win,list(win)];
  button.set_flags[GTK.CAN_DEFAULT]
  box2.pack_start[button]
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







