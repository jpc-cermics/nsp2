
function demo_liststore () 
  window = gtkwindow_new()
  window.set_title[  "GtkListStore demo"]
  //window.connect[  "destroy", gtk_widget_destroyed]
  window.set_border_width[  8]

  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  window.add[  vbox]
  label = gtklabel_new(str="List store test");
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

  sw = gtkscrolledwindow_new();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
  sw.set_policy[GTK.POLICY_NEVER,GTK.POLICY_AUTOMATIC]
  vbox.pack_start[ sw,expand=%t,fill=%t,padding=0]

  // --------------------------------------------
  // A model in a liststore
  // -------------------------------------------
  C1 = [%t;%f;%t];
  C23 = [1,4;2,4;3,5]; // two columns 
  C4 = ["first row";"second row";"third row"];
  C5 = list(10,20,30);
  list_store=list(C1,C23,C4,C5);

  model = gtkliststore_new(list_store);

  // --------------------------------------------
  // A treeview for model visualization 
  // -------------------------------------------

  treeview = gtktreeview_new(model);
  treeview.set_rules_hint[%t];
  treeview.set_search_column[3];
  sw.add[treeview]

  // give name to columns 
  
  C1=0;
  C2=1;
  C3=2;
  C4=3;
  C5=4;

  model = treeview.get_model[];
  // column for booleans 
  renderer = gtkcellrenderertoggle_new ();
  //uncomment if toggle button can be activated 
  
  function fixed_toggled (cell, path_str, data)
    // callback for changing the toggle status 
      model=data(1);
      column_number = data(2);
      // get toggled iter */
      iter=model.get_iter[path_str];
      val = model.get_value[iter,column_number];
      // do something with the value 
      val = ~val;
      // set new value */
      model.set[iter,column_number, val];
  endfunction
  
  renderer.connect[  "toggled", fixed_toggled,list(model,C1)]
  col = gtktreeviewcolumn_new(title="Boolean as toggle",renderer=renderer,attrs= hcreate(active=C1));
  col.set_sizing[ GTK.TREE_VIEW_COLUMN_FIXED]   // set this column to a fixed sizing (of 50 pixels) */
  col.set_fixed_width[50]
  treeview.append_column[col];

  // same column  for booleans as text 
  renderer = gtkcellrenderertext_new ();
  col = gtktreeviewcolumn_new(title="Boolean as text",renderer=renderer,attrs= hcreate(text=C1));
  col.set_sizing[ GTK.TREE_VIEW_COLUMN_FIXED]   // set this column to a fixed sizing (of 50 pixels) */
  col.set_fixed_width[50]
  treeview.append_column[col];
  
  // column for numbers */
  renderer = gtkcellrenderertext_new ();
  col = gtktreeviewcolumn_new(title="Scalars",renderer=renderer,attrs=hcreate(text=C2));
  col.set_sort_column_id[C2];
  treeview.append_column[col];

  // column for numbers 
  renderer = gtkcellrenderertext_new ();
  col = gtktreeviewcolumn_new(title="Scalars",renderer=renderer,attrs=hcreate(text=C3));
  col.set_sort_column_id[C3];
  treeview.append_column[col];

  // column for strings */
  renderer = gtkcellrenderertext_new ();
  col = gtktreeviewcolumn_new(title="String",renderer=renderer,attrs=hcreate(text=C4));
  col.set_sort_column_id[C4];
  treeview.append_column[col];
  
  // column for numbers */
  renderer = gtkcellrenderertext_new ();
  col = gtktreeviewcolumn_new(title="Scalars",renderer=renderer,attrs=hcreate(text=C5));
  col.set_sort_column_id[C5];
  treeview.append_column[col];
  // ---------------------------------------------

  window.set_default_size[  280, 250]
  window.show_all[];
endfunction 


