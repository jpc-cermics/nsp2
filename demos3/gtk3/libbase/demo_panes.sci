// Paned Widgets
//
// The GtkHPaned and GtkVPaned Widgets divide their content
// area into two panes with a divider in between that the
// user can adjust. A separate child is placed into each
// pane.
//
// There are a number of options that can be set for each pane.
// This test contains both a horizontal (HPaned) and a vertical
// (VPaned) widget, and allows you to adjust the options for
// each side of each widget.


function demo_panes ()

  function toggle_resize (widget,args)
    paned = args(1);
    num= args(2);
    ch = paned.get_children[];   
    if num == 1 then 
      paned.remove[ch(1)];
      paned.pack1[ch(1), resize=~paned.child1_resize, shrink=paned.child1_shrink];
    else
      paned.remove[ch(2)];
      paned.pack2[ch(2), resize=~paned.child2_resize, shrink=paned.child2_shrink];
    end
  endfunction

  function toggle_shrink (widget,args)
    paned = args(1);
    num= args(2);
    ch = paned.get_children[];   
    if num == 1 then 
      paned.remove[ch(1)];
      paned.pack1[ch(1), resize=paned.child1_resize, shrink= ~paned.child1_shrink];
    else
      paned.remove[ch(2)];
      paned.pack2[ch(2), resize=paned.child2_resize, shrink= ~paned.child2_shrink];
    end
  endfunction 
  
  function [frame]=create_pane_options (paned,frame_label,label1,label2)
    frame = gtk_frame_new(label=frame_label);
    frame.set_border_width[  4]
    
    table = gtk_grid_new();
    frame.add[table]
    
    label = gtk_label_new(str=label1);
    table.attach[  label,0,0,1,1]
    
    check_button = gtk_check_button_new(mnemonic="_Resize");
    table.attach[check_button, 0, 1, 1,1]
    check_button.connect[  "toggled", toggle_resize, list(paned,1)]

    check_button = gtk_check_button_new(mnemonic="_Shrink");
    table.attach[  check_button,  0, 2,1,1]
    check_button.set_active[ %t]
    check_button.connect[  "toggled", toggle_shrink, list(paned,1)]
    
    label = gtk_label_new(str=label2);
    table.attach[  label,  1, 0, 1,1]
    
    check_button = gtk_check_button_new(mnemonic="_Resize");
    table.attach[  check_button, 1, 1,1,1]
    check_button.set_active[  %t]
    check_button.connect[  "toggled", toggle_resize, list(paned,2)];
    
    check_button = gtk_check_button_new(mnemonic="_Shrink");
    table.attach[  check_button, 1, 2, 1, 1]
    check_button.set_active[ %t]
    check_button.connect[  "toggled", toggle_shrink, list(paned,2)];

  endfunction 
  
  window = gtk_window_new()// GTK.WINDOW_TOPLEVEL
  //XXXX window.connect[  "destroy",gtk_widget_destroyed];
  window.set_title["Panes"]
  window.set_border_width[0]

  vbox = gtk_box_new("vertical",spacing=0);
  window.add[  vbox]
  
  vpaned = gtk_paned_new("vertical");
  vbox.pack_start[vpaned,expand=%t,fill=%t,padding=0]
  vpaned.set_border_width[5]

  hpaned = gtk_paned_new("horizontal");
  vpaned.add1[hpaned];

  frame = gtk_frame_new();
  frame.set_shadow_type[ GTK.SHADOW_IN];
  frame.set_size_request[  60, 60]
  hpaned.add1[frame];
  
  button = gtk_button_new(mnemonic="_Hi there");
  frame.add[button]
  
  frame = gtk_frame_new();
  frame.set_shadow_type[ GTK.SHADOW_IN];
  frame.set_size_request[  80, 60]
  hpaned.add2[frame];

  frame = gtk_frame_new();
  frame.set_shadow_type[ GTK.SHADOW_IN];
  frame.set_size_request[  60, 80]
  vpaned.add2[frame];
  
  ho = create_pane_options (hpaned,"Horizontal","Left","Right");
  vo = create_pane_options (vpaned,"Vertical","Top","Bottom")
  vbox.pack_start[ ho,expand=%f,fill=%f,padding=0]
  vbox.pack_start[ vo,expand=%f,fill=%f,padding=0]
  vbox.show_all[];
  window.show[];
endfunction

