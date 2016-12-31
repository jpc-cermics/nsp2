// List Store
//
// The GtkListStore is used to store data in list form, to be used later on
// by a GtkTreeView to display it. 
// This demo builds simple GtkListStores from matrices 

function demo_list_store_2()

  function demo_liststore_from_mat () 
    window = gtk_window_new()
    window.set_title["GtkListStore demo"]
    //window.connect["destroy", gtk_widget_destroyed]
    window.set_border_width[  8]

    vbox = gtk_box_new("vertical",spacing=8);
    window.add[  vbox]
    label = gtk_label_new(str="This is a list store test From a real Matrix .");
    vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

    sw = gtk_scrolled_window_new();
    sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
    sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC]
    vbox.pack_start[sw,expand=%t,fill=%t,padding=0]

    M= testmatrix('magic',8);
    model = gtk_list_store_new(M); 
    model = gtk_list_store_new(list(M));

    M=matrix(1:20,4,5);
    model = gtk_list_store_new(list(M(:,1),M(:,2:3),M(:,4:5)));
    
    // create tree view */

    treeview = gtk_tree_view_new(model);
    // deprecated in 3.14
    // treeview.set_rules_hint[%t];
    treeview.set_search_column[3];
    
    sw.add[treeview]

    renderer = gtk_cell_renderer_text_new ();
    for i=1:size(M,'c') 
      col = gtk_tree_view_column_new(title=sprintf("Col %d",i),renderer=renderer,attrs=hash(text=i-1));
      col.set_sort_column_id[i-1];
      treeview.append_column[col];
    end 
    window.set_default_size[  280, 250]
    window.show_all[];
  endfunction 

  function demo_liststore_from_smat () 
    window = gtk_window_new()
    window.set_title["GtkListStore demo"]
    //window.connect["destroy", gtk_widget_destroyed]
    window.set_border_width[  8]

    vbox = gtk_box_new("vertical",spacing=8);
    window.add[  vbox]
    label = gtk_label_new(str="This is a list store test from a String Matrix ");
    vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

    sw = gtk_scrolled_window_new();
    sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
    sw.set_policy[GTK.POLICY_NEVER,GTK.POLICY_AUTOMATIC]
    vbox.pack_start[ sw,expand=%t,fill=%t,padding=0]

    M= testmatrix('magic',8);
    M=m2s(M,"%.0f")
    model = gtk_list_store_new(M);

    // create tree view */
    treeview = gtk_tree_view_new(model);
    // deprecated in 3.14
    //treeview.set_rules_hint[%t];
    treeview.set_search_column[3];
    sw.add[treeview]

    renderer = gtk_cell_renderer_text_new ();
    // change the alignment (added March 2013)
    renderer.set_alignment[0.5,0.5]; // center in cells 
    for i=1:size(M,'c') 
      col = gtk_tree_view_column_new(title=sprintf("Col %d",i),renderer=renderer,attrs=hash(text=i-1));
      col.set_sort_column_id[i-1];
      // sepcify the alignment of the title of the gtktreeviewcolumn
      col.set_alignment[1.0]// 0: left  0.5: middle 1.0 :right 
      treeview.append_column[col];
    end 
    window.set_default_size[  280, 250]
    window.show_all[];
  endfunction 

  function demo_liststore_from_bmat () 
    window = gtk_window_new()
    window.set_title["GtkListStore demo"]
    //window.connect["destroy", gtk_widget_destroyed]
    window.set_border_width[  8]

    vbox = gtk_box_new("vertical",spacing=8);
    window.add[  vbox]
    label = gtk_label_new(str="This is a list store test from a Boolean Matrix ");
    vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

    sw = gtk_scrolled_window_new();
    sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
    sw.set_policy[GTK.POLICY_NEVER,GTK.POLICY_AUTOMATIC]
    vbox.pack_start[ sw,expand=%t,fill=%t,padding=0]

    M= rand(5,8) >= 0.5;
    model = gtk_list_store_new(M);

    // create tree view */
    treeview = gtk_tree_view_new(model);
    //treeview.set_rules_hint[%t];
    treeview.set_search_column[3];
    sw.add[treeview]
    renderer = gtk_cell_renderer_text_new ();
    for i=1:size(M,'c') 
      col = gtk_tree_view_column_new(title=sprintf("Col %d",i),renderer=renderer,attrs=hash(text=i-1));
      col.set_sort_column_id[i-1];
      treeview.append_column[col];
    end 
    window.set_default_size[  280, 250]
    window.show_all[];
  endfunction 

  
  demo_liststore_from_mat () 
  demo_liststore_from_smat () 
  demo_liststore_from_bmat () 

endfunction

