
function demo_main_window()
  
  Demos={ {'Assistant', "demo_assistant"}
	  {'Button Boxes',  "demo_button_box"}
	  {'Buttons',  "demo_buttons"}
	  {'Buttons: Check buttons',  "demo_check_buttons"}
	  {'Buttons: Radio buttons',  "demo_radio_buttons"}
	  {'Buttons: Toggle buttons',  "demo_toggle_buttons"}
	  {'Cairo Drawing',  "demo_cairo"}
	  {'Calendar',  "demo_calendar"}
	  {'Color Selection',  "demo_color_selection"}
	  {'Cursor',  "demo_cursor"}
	  {'Dialog and Message Boxes',  "demo_dialog"}	  
	  {'Dnd', "demo_dnd"}
	  {'Drawing Area', "demo_drawingarea"}
	  {'Entry',  "demo_entry"}
	  {'File chooser',  "demo_file_chooser"}
	  {'Flipping',  "demo_flipping"}
	  {'Focus',  "demo_focus"}
	  {'Gamma curve',  "demo_gamma_curve"}
	  {'Gmenumodel', "demo_gmenumodel"}
	  {'Menus',  "demo_menus"}
	  {'Messages', "demo_messagedialog"}
	  {'Notebook',  "demo_notebook"}
	  {'Panes',  "demo_panes"}
	  {'Pickers',  "demo_pickers"}
	  {'Picker (Color)',  "demo_color_picker"}
	  {'Progress bar',  "demo_progressbar"}
	  {'Range controls',  "demo_range_controls"}
	  {'Reparent',  "demo_reparent"}
	  {'Revealer', "demo_revealer"}
	  {'Scrolled windows',  "demo_scrolled_windows"}
	  {'Spinbutton', "demo_spinbutton"}
	  {'Stack', "demo_stack"}
	  {'Status bar',  "demo_statusbar"}
	  {'Test idle', "demo_idle"}
	  {'Test timeout', "demo_timeout"}
	  {'Toolbar',  "demo_toolbar"}
	  {'Tooltips', "demo_tooltips"}}
    
  win = gtkwindow_new()
  win.set_title["main window"]
  win.set_size_request[200, 400]
  //win.set_uposition[20,20]
  //win.connect["destroy",hide];
  win.connect["delete_event", demo_delete];
  win.set_title["gtktest"];
  box1 = gtkbox_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  scrolled_window = gtkscrolledwindow_new()
  scrolled_window.set_border_width[10]
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  box1.pack_start[scrolled_window, expand=%t,fill=%t,padding=0]
  scrolled_window.show[]
  box2 = gtkbox_new("vertical",spacing=0)
  box2.set_border_width[0]
  scrolled_window.add[box2];
  box2.show[];
  
  for i1 = 1:size(Demos,'*')
    button = gtkbutton_new(label=Demos{i1}{1})
    ok = execstr(sprintf("func=%s;",Demos{i1}{2}),errcatch=%t);
    if ok && is(func,%types.PList) then
      button.connect["clicked",func,win];
    else
      button.set_sensitive[%f]
    end
    box2.pack_start[button]
    button.show[]
  end
  separator = gtkseparator_new("horizontal")
  box1.pack_start[separator,expand= %f,fill= %f,padding=0]
  separator.show[]
  box2 = gtkbox_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,expand=%f,fill=%f,padding=0]
  box2.show[]
  button = gtkbutton_new(label="close")
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
