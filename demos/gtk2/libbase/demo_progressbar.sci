//gtk_progress_bar_set_pulse_step(), 
//gtk_progress_bar_get_pulse_step().

function demo_progressbar() 

  function omenu=build_option_menu (items, history, func, args)
    omenu = gtkoptionmenu_new ();
    omenu.connect [ "changed",func, args];
    menu = gtkmenu_new ();
    menu_item = gtkradiomenuitem_new(label= items(1));
    menu.append[ menu_item];		
    group = menu_item;
    n= size(items,"*")
    for it=2:n
      menu_item = gtkradiomenuitem_new(group=group,label= items(it));
      menu.append[ menu_item];
      if it == history then  menu_item.set_active[%t];end 
      menu_item.show[];
    end
    omenu.set_menu[menu];
    omenu.set_history[history];
  endfunction

  function progress_timeout (args)
  // args=list(pbar,label)
    activity= args(1).get_data['activity_mode'];
    if activity then 
      args(1).pulse[];
    else 
      pcnt = args(1).get_fraction[];
      pcnt = modulo(pcnt*100 + 2,100)/100
      printf("%f\n",pcnt);
      args(1).set_fraction[pcnt];
      args(2).set_text[sprintf("%3.0f %%",pcnt*100)];
      y=%t
    end 
  endfunction

  function destroy_progress (widget,args)
    gtk_timeout_remove(args(1));
  endfunction

  function progressbar_toggle_orientation(widget,args)
  // if (!GTK_WIDGET_MAPPED (widget)) XXXXXX 
    i = widget.get_history[];
    args(1).set_orientation[i];
  endfunction

  function toggle_show_text (widget,args)
  // args= list(pbar,entry)
    active =widget.get_active[]; 
    args(2).set_sensitive[active];
    if active then 
      args(1).set_text[args(2).get_text[]];
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

  function entry_changed (widget,args)
    args(1).set_text[widget.get_text[]];
  endfunction 
  
  items1 = ["Left-Right","Right-Left","Bottom-Top","Top-Bottom"];
  items2 = ["Continuous","Discrete"];
  pdata = list(1);  
  window = gtkdialog_new ();
  window.set_resizable[ %f]

  window.set_title["GtkProgressBar"]
  window.set_border_width[ 0]
  vbox = gtkvbox_new(homogeneous=%f,spacing=5);
  vbox.set_border_width[ 10]
  window.vbox.pack_start[ vbox,expand=%f,fill=%t,padding=0]
  frame = gtkframe_new(label="Progress");
  vbox.pack_start[ frame,expand=%f,fill=%t,padding=0]
  vbox2 = gtkvbox_new(homogeneous=%f,spacing=5);
  frame.add[ vbox2]
  align = gtkalignment_new(xalign=0.5,yalign=0.5,xscale=0,yscale=0);
  vbox2.pack_start[ align,expand=%f,fill=%f,padding=5]

  pbar = gtkprogressbar_new();

  align.add[pbar];
  align = gtkalignment_new(xalign=0.5,yalign=0.5,xscale=0,yscale=0);
  vbox2.pack_start[ align,expand=%f,fill=%f,padding=5]
  hbox = gtkhbox_new(homogeneous=%f,spacing=5);
  align.add[ hbox]
  label = gtklabel_new(str="Label updated by user :"); 
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]
  label = gtklabel_new(str="");
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]

  pbar.set_data[activity_mode=%f];
  tt = gtk_timeout_add (100, progress_timeout,list(pbar,label));
  window.connect["destroy",destroy_progress,list(tt)];

  //---- Options 
  
  frame = gtkframe_new(label="Options");
  vbox.pack_start[ frame,expand=%f,fill=%t,padding=0]
  vbox2 = gtkvbox_new(homogeneous=%f,spacing=5);
  frame.add[ vbox2]
  tab = gtktable_new(rows=3,columns=2,homogeneous=%f);
  vbox2.pack_start[ tab,expand=%f,fill=%t,padding=0]

  //----- Orientation 

  label = gtklabel_new(str="Orientation :");
  tab_option= ior(GTK.EXPAND,GTK.FILL)

  tab.attach[label,0,1,0,1,xoptions=tab_option,yoptions=tab_option, xpadding=5,ypadding=5]
  label.set_alignment[  0, 0.5]

  omenu1 = build_option_menu (items1, 0, progressbar_toggle_orientation, list(pbar));
  hbox = gtkhbox_new(homogeneous=%f,spacing=0);
  tab.attach[  hbox, 1, 2, 0, 1,xoptions= tab_option,yoptions=tab_option,xpadding=5,ypadding=5]
  hbox.pack_start[ omenu1,expand=%t,fill=%t,padding=0]
      
  //----- Show text 
  check = gtkcheckbutton_new(label="Show text");
  tab.attach[  check, 0, 1, 1, 2,xoptions=tab_option,yoptions=tab_option,xpadding=5,ypadding=5]
  hbox = gtkhbox_new(homogeneous=%f,spacing=0);
  tab.attach[  hbox, 1, 2, 1, 2,xoptions=tab_option,yoptions=  tab_option,xpadding=5,ypadding=5]

  label = gtklabel_new(str="Text: ");
  hbox.pack_start[label,expand=%f,fill=%t,padding=0]

  entry = gtkentry_new ();
  entry.connect["changed",entry_changed,list(pbar)];
  hbox.pack_start[ entry,expand=%t,fill=%t,padding=0]
  entry.set_text["Progress bar text"];
  entry.set_size_request[100,-1];
  entry.set_sensitive[%f]

  pbar.set_data[show_text=%f];
  check.connect[  "clicked",toggle_show_text,list(pbar,entry)];  

  //----- Activity mode 

  check = gtkcheckbutton_new(label="Activity mode");
  tab.attach[ check,0,1,3,4,xoptions=tab_option,yoptions= tab_option,xpadding=5,ypadding=5]

  hbox = gtkhbox_new(homogeneous=%f,spacing=0);
  tab.attach[ hbox,1,2, 3,4,xoptions=tab_option,yoptions= tab_option,xpadding=5,ypadding=5]
  label = gtklabel_new(str="Pulse step: ");
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]
  adj = gtkadjustment_new(value=3,lower=1,upper=20,step_incr=1,page_incr=5,page_size=0);
  step_spin = gtkspinbutton_new(adjustment=adj,climb_rate=0,digits=0);
  adj.connect[ "value_changed",adjust_step, list(pbar,step_spin)];
  hbox.pack_start[ step_spin,expand=%f,fill=%t,padding=0]
  step_spin.set_sensitive[%f];

  check.connect[ "clicked",toggle_activity_mode,list(pbar,step_spin)];

  //------ Close 

  button = gtkbutton_new(label="close");

  function y=win_kill(widget,args) 
    gtk_timeout_remove(args(2));
    args(1).destroy[];
    y=%t
  endfunction

  button.connect["clicked",win_kill,list(window,tt)];
  button.set_flags[GTK.CAN_DEFAULT];
  window.action_area.pack_start[ button,expand=%t,fill=%t,padding=0];
  button.grab_default[];
  window.show_all[];
endfunction 

