//gtk_progress_bar_set_pulse_step(),
//gtk_progress_bar_get_pulse_step().

function demo_progressbar()

  function y=progress_timeout (args)
  // args=list(pbar,label)
    activity= args(1).get_data['activity_mode'];
    if activity then
      args(1).pulse[];
    else
      pcnt = args(1).get_fraction[];
      pcnt = modulo(pcnt*100 + 2,100)/100
      // printf("%f\n",pcnt);
      args(1).set_fraction[pcnt];
      args(2).set_text[sprintf("%3.0f %%",pcnt*100)];
    end
    y=%t
  endfunction

  function toggle_show_text (widget,args)
  // args= list(pbar,entry)
    active =widget.get_active[];
    args(2).set_sensitive[active];
    if active then
      args(1).set_text[args(2).get_text[]];
      args(1).set_show_text[%t];
    else
      args(1).set_text[""];
    end
  endfunction

  function adjust_step (widget,args)
    v = args(2).get_value_as_int[];
    printf("setting the pulse to %d\n",v/100);
    args(1).set_pulse_step[v/100];
  endfunction

  function toggle_activity_mode (widget,args)
    active =widget.get_active[];
    args(2).set_sensitive[active];
    args(1).set_data[activity_mode=active];
  endfunction

  function toggle_inverted_mode (widget,args)
    b= args(1).get_inverted[];
    args(1).set_inverted[~b];
  endfunction

  function entry_changed (widget,args)
    args(1).set_text[widget.get_text[]];
  endfunction

  dialog = gtk_dialog_new (flags=ior(GTK.DIALOG_MODAL,GTK.DIALOG_DESTROY_WITH_PARENT));
  dialog.set_resizable[ %f]
  dialog.set_title["GtkProgressBar"]
  dialog.set_border_width[ 0]

  vbox = dialog.get_content_area[];
  frame = gtk_frame_new(label="Progress");
  vbox.pack_start[ frame,expand=%f,fill=%t,padding=0]
  vbox2 = gtk_box_new("vertical",spacing=5);
  frame.add[ vbox2]

  align = gtk_box_new("horizontal",spacing=5);
  vbox2.pack_start[ align,expand=%f,fill=%f,padding=5]

  pbar = gtk_progress_bar_new();
  align.add[pbar];
  align = gtk_box_new("horizontal",spacing=5);
  vbox2.pack_start[ align,expand=%f,fill=%f,padding=5]
  hbox = gtk_box_new("horizontal",spacing=5);
  align.add[ hbox]
  label = gtk_label_new(str="Label updated by user :");
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]
  label = gtk_label_new(str="");
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]

  pbar.set_data[activity_mode=%f];
  timeout = gtk_timeout_add (100, progress_timeout,list(pbar,label));

  function destroy_timeout (widget,args)
    g_source_remove(args(1));
  endfunction

  dialog.connect["destroy",destroy_timeout,list(timeout)];

  //---- Options
  frame = gtk_frame_new(label="Options");
  vbox.pack_start[ frame,expand=%f,fill=%t,padding=0]
  vbox2 = gtk_box_new("vertical",spacing=5);
  frame.add[ vbox2]

  tab = gtk_grid_new();
  vbox2.pack_start[ tab,expand=%f,fill=%t,padding=0]
  //----- Orientation
  label = gtk_label_new(str="Orientation:");
  //label.set_alignment[  0, 0.5]
  tab.attach[label,0,0,1,1];

  // ---- select the orientation of the progressbar
  // Horizontal or Vertical

  function progressbar_toggle_orientation(combobox,args)
    // O: horizontal GTK.ORIENTATION_HORIZONTAL
    // 1: vertical   GTK.ORIENTATION_VERTICAL
    i = combobox.get_active[];
    args(1).set_orientation[i];
  endfunction

  combobox =gtk_combo_box_new(text=['Horizontal','Vertical']);
  combobox.set_active[0];
  combobox.connect [ "changed",progressbar_toggle_orientation,list(pbar)];
  hbox = gtk_box_new("horizontal",spacing=0);
  tab.attach[ hbox, 1, 0, 1,1];
  hbox.pack_start[combobox,expand=%t,fill=%t,padding=0]

  //----- Show text
  check = gtk_check_button_new(label="Show text");
  tab.attach[  check, 0, 1,1,1];
  hbox = gtk_box_new("horizontal",spacing=0);
  tab.attach[  hbox, 1, 1,1, 1];
  label = gtk_label_new(str="Text: ");
  hbox.pack_start[label,expand=%f,fill=%t,padding=0]

  entry = gtk_entry_new ();
  entry.connect["changed",entry_changed,list(pbar)];
  hbox.pack_start[ entry,expand=%t,fill=%t,padding=0]
  entry.set_text["Progress bar text"];
  entry.set_size_request[100,-1];
  entry.set_sensitive[%f]

  pbar.set_data[show_text=%f];
  check.connect[  "clicked",toggle_show_text,list(pbar,entry)];

  //----- Activity mode
  check = gtk_check_button_new(label="Activity mode");
  tab.attach[ check,0,3,1,1];

  hbox = gtk_box_new("horizontal",spacing=0);
  tab.attach[ hbox,1, 3,1,1];
  label = gtk_label_new(str="Pulse step: ");
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]
  adj = gtk_adjustment_new(value=3,lower=1,upper=20,step_incr=1,page_incr=5,page_size=0);
  step_spin = gtk_spin_button_new(adjustment=adj,climb_rate=0,digits=0);
  adj.connect[ "value_changed",adjust_step, list(pbar,step_spin)];
  hbox.pack_start[ step_spin,expand=%f,fill=%t,padding=0]
  step_spin.set_sensitive[%f];
  check.connect[ "clicked",toggle_activity_mode,list(pbar,step_spin)];

  // ---- Invert movement
  check = gtk_check_button_new(label="inverted");
  tab.attach[ check,0,4,1,1];
  check.connect[ "clicked",toggle_inverted_mode,list(pbar)];
  //------ Close
  dialog.add_button["Close",1];
  dialog.show_all[];
  response = dialog.run[]
  dialog.destroy[];

endfunction
