// building a list store from Matrix 

function demo_liststore1()
  demo_liststore_from_mat () 
  demo_liststore_from_smat () 
  demo_liststore_from_bmat () 
endfunction

function demo_liststore_from_mat () 
  window = gtkwindow_new()
  window.set_title["GtkListStore demo"]
  //window.connect["destroy", gtk_widget_destroyed]
  window.set_border_width[  8]

  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  window.add[  vbox]
  label = gtklabel_new(str="This is a list store test From a real Matrix .");
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

  sw = gtkscrolledwindow_new();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC]
  vbox.pack_start[sw,expand=%t,fill=%t,padding=0]

  M= testmatrix('magic',8);
  model = gtkliststore_new(M); 
  model = gtkliststore_new(list(M));

  M=matrix(1:20,4,5);
  model = gtkliststore_new(list(M(:,1),M(:,2:3),M(:,4:5)));
  
  // create tree view */

  treeview = gtktreeview_new(model);
  treeview.set_rules_hint[%t];
  treeview.set_search_column[3];

  sw.add[treeview]

  renderer = gtkcellrenderertext_new ();
  for i=1:size(M,'c') 
    col = gtktreeviewcolumn_new(title=sprintf("Col %d",i),renderer=renderer,attrs=hcreate(text=i-1));
    col.set_sort_column_id[i-1];
    treeview.append_column[col];
  end 
  window.set_default_size[  280, 250]
  window.show_all[];
endfunction 

function demo_liststore_from_smat () 
  window = gtkwindow_new()
  window.set_title["GtkListStore demo"]
  //window.connect["destroy", gtk_widget_destroyed]
  window.set_border_width[  8]

  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  window.add[  vbox]
  label = gtklabel_new(str="This is a list store test from a String Matrix ");
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

  sw = gtkscrolledwindow_new();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
  sw.set_policy[GTK.POLICY_NEVER,GTK.POLICY_AUTOMATIC]
  vbox.pack_start[ sw,expand=%t,fill=%t,padding=0]

  M= testmatrix('magic',8);
  M=m2s(M,"%.0f")
  model = gtkliststore_new(M);

  // create tree view */
  treeview = gtktreeview_new(model);
  treeview.set_rules_hint[%t];
  treeview.set_search_column[3];
  sw.add[treeview]

  renderer = gtkcellrenderertext_new ();
  for i=1:size(M,'c') 
    col = gtktreeviewcolumn_new(title=sprintf("Col %d",i),renderer=renderer,attrs=hcreate(text=i-1));
    col.set_sort_column_id[i-1];
    treeview.append_column[col];
  end 
  window.set_default_size[  280, 250]
  window.show_all[];
endfunction 

function demo_liststore_from_bmat () 
  window = gtkwindow_new()
  window.set_title["GtkListStore demo"]
  //window.connect["destroy", gtk_widget_destroyed]
  window.set_border_width[  8]

  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  window.add[  vbox]
  label = gtklabel_new(str="This is a list store test from a Boolean Matrix ");
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

  sw = gtkscrolledwindow_new();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
  sw.set_policy[GTK.POLICY_NEVER,GTK.POLICY_AUTOMATIC]
  vbox.pack_start[ sw,expand=%t,fill=%t,padding=0]

  M= rand(5,8) >= 0.5;
  model = gtkliststore_new(M);

  // create tree view */
  treeview = gtktreeview_new(model);
  treeview.set_rules_hint[%t];
  treeview.set_search_column[3];
  sw.add[treeview]
  renderer = gtkcellrenderertext_new ();
  for i=1:size(M,'c') 
    col = gtktreeviewcolumn_new(title=sprintf("Col %d",i),renderer=renderer,attrs=hcreate(text=i-1));
    col.set_sort_column_id[i-1];
    treeview.append_column[col];
  end 
  window.set_default_size[  280, 250]
  window.show_all[];
endfunction 
