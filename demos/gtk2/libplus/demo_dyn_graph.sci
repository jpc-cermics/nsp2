function demo_dyn_graph()
  win = gtkwindow_new()
  win.connect["delete_event", demo_delete];
  win.set_title["range_controls"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]

  id_win=nsp_graphic_new(win,box1,dim=[300,200]);

  //adjustment = GtkAdjustment(0,expand= 0,fill= 101,padding= 0.1, 1, 1)
  initial_v=10;
  adjustment =gtkadjustment_new(value=initial_v,lower=5,upper=21,step_incr=1,...
				page_incr=1,page_size=1)
  win.set_data[hscale=initial_v];
  scale = gtkhscale_new(adjustment=adjustment)
  scale.set_size_request[150,-1]
  scale.set_update_policy[GTK.UPDATE_DELAYED];
  scale.set_digits[1];
  scale.set_draw_value[%t];
  box1.pack_start[scale,expand=%f,fill=%f,padding=0]
  scale.show[]
  adjustment.connect[ "value_changed", demo_dyn_graph_value_changed,...
		      list(id_win,scale,win)];

  box2=gtkvbox_new(homogeneous=%f,spacing=0);
  box1.pack_start[box2,expand=%f,fill=%t,padding=0];
  box1=box2;
  
  // hbox for radio buttons 
  // ----------------------
  box2 = gtkhbox_new(homogeneous=%f,spacing=0)
  box2.set_border_width[10]
  box1.pack_start[box2,expand=%f,fill=%f,padding=0];
  box2.show[];
  // radio buttons
  // ----------------------
  
  // The "toggled" signal
    
  button1=gtkradiobutton_new(label="hot")
  box2.pack_start[button1]
  button1.show[]
  // button1 i sthe default selection.
  button1.set_active[%t];
  win.set_data[colormap_id = 1];
  //
  button1.connect["toggled",demo_dyn_graph_toggled, list(id_win,button1,1,win)];
  // 
  button=gtkradiobutton_new(group=button1,label= "gray");
  box2.pack_start[button];
  button.show[];
  button.connect[ "toggled",demo_dyn_graph_toggled,
		   list(id_win,button,2,win)];
  // 
  button=gtkradiobutton_new(group=button1,label= "jet");
  box2.pack_start[button];
  button.show[];
  button.connect[ "toggled",demo_dyn_graph_toggled,
		   list(id_win,button,3,win)];

  separator = gtkhseparator_new ();
  box1.pack_start[separator];
  separator.show[];

  // 
  button = gtkbutton_new(label="Close")
  button.connect["clicked", button_destroy_win,list(win)];
  box1.pack_start[button,expand=%f,fill=%f,padding=0]
  button.set_flags[GTK.CAN_DEFAULT]
  button.grab_default[]
  button.show[]
  win.show_all[]

  demo_dyn_graph_draw(id_win,10,1);
  //gtk_main()
endfunction

function demo_dyn_graph_value_changed(widget,args)
// args=list(win_id, gtkhscale,win);
  args(3).set_data[hscale=args(2).get_value[]];
  demo_dyn_graph_draw(args(1),args(2).get_value[],args(3).get_data['colormap_id']);
endfunction

function demo_dyn_graph_toggled(widget,args)
  // args(2) is the gtkhscale
  if args(2).get_active[]==%t then 
   args(4).set_data[colormap_id =  args(3)];
  end
  value=args(4).get_data['hscale'];
  demo_dyn_graph_draw(args(1),value,args(3));
endfunction


function demo_dyn_graph_draw(id,value,colormap_id)
  id1=xget('window');
  if id1<>id then xset('window',id);end 
  xclear();
  select colormap_id 
   case 1 then xset('colormap',hotcolormap(64));
   case 2 then xset('colormap',graycolormap(64));
   case 3 then xset('colormap',jetcolormap(64));
  end
  t=linspace(-%pi,%pi,value);plot3d1(t,t,sin(t)'*cos(t));
  if id1<>id then xset('window',id1);end 
endfunction
