// Tree View/Editable Cells


function demo_treeview_editable() 
  
  function add_item(button,data)
    model = data(1);
    model.append[list(0,"description here",%t)]
  endfunction 

  function remove_item(widget,data) 
    treeview = data(1)
    model = treeview.get_model[];
    selection = treeview.get_selection[];
    iter= selection.get_selected[];
    if type(iter,'string') == 'None' then 
      printf("No selected row\n"); 
    else
      path = model.get_path[iter];
      i = path.get_indices[];
      printf("object %d removed\n",i); 
      model.remove[iter];
    end
  endfunction 

  function cell_edited (cell,path_string,new_text,data)
    model = data(1);
    col = cell.get_data["column"];
    path = gtktreepath_new(path_string);
    i = path.get_indices[];
    iter = model.get_iter[path_string];
    select col 
     case 0 then // colonne des nombres 
      rep=execstr('val='+new_text');
      model.set[iter,col,val];
     case 1 then // colonne des produits
      model.set[iter,col,new_text];
    end
  endfunction

  function add_columns (treeview)
    model = treeview.get_model[];
    col_editable = 2;
    cols=["Nombre","Produits"];
    for col= 0:1
      renderer = gtkcellrenderertext_new ();
      renderer.connect[  "edited",  cell_edited,list(model)]
      renderer.set_data[column=col ];
      attrs= hash_create(text= col, editable= col_editable);
      // XXXX bugué a reparer 
      // treeview.insert_column_with_attributes[ -1,title=sprintf("Column
      // %d",col+1),renderer=renderer,attrs= attrs];
      tc = gtktreeviewcolumn_new(title=cols[col+1],renderer=renderer,attrs=attrs);
      treeview.append_column[tc];
    end 

    // column for editable flag 
    renderer = gtkcellrenderertoggle_new ();
  
    function editable_toggled (cell, path_str, data)
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
  
    renderer.connect["toggled", editable_toggled,list(model,col_editable)]
    col = gtktreeviewcolumn_new(title="Editable",renderer=renderer,attrs= hash(active=col_editable));
    col.set_sizing[ GTK.TREE_VIEW_COLUMN_FIXED] 
    col.set_fixed_width[50]
    treeview.append_column[col];
  endfunction
    
  window = gtkwindow_new();// (GTK.WINDOW_TOPLEVEL);
  window.set_title[  "Shopping list"]
  window.set_border_width[  5]
  // window.connect[  "destroy",gtk_widget_destroyed, &window]

  vbox = gtkvbox_new(homogeneous=%f,spacing=5);
  window.add[  vbox]
  label=gtklabel_new(str="Shopping list (you can edit the cells!)")
  vbox.pack_start[label,expand=%f,fill=%f,padding=0];

  sw = gtkscrolledwindow_new();
  sw.set_shadow_type[ GTK.SHADOW_ETCHED_IN]
  sw.set_policy[ GTK.POLICY_AUTOMATIC,  GTK.POLICY_AUTOMATIC]
  vbox.pack_start[ sw,expand=%t,fill=%t,padding=0];
  
  // create model 
  
  str = ["Chateau Margaud"; "Kg de pommes de terre"; "Boites de confit de canard"];
  fls = list( [1;2;2], str, [%t;%t;%t]);

  model = gtkliststore_new(fls) 

  // create tree view 
  treeview = gtktreeview_new(model);
  treeview.set_rules_hint[  %t]
  treeview.get_selection[].set_mode[GTK.SELECTION_SINGLE];

  add_columns(treeview);

  sw.add[treeview]

  // some buttons 
  hbox = gtkhbox_new(homogeneous=%t,spacing=4);
  vbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]

  button = gtkbutton_new(label="Add item");
  button.connect[  "clicked", add_item, list(model)]
  hbox.pack_start[ button,expand=%t,fill=%t,padding=0]

  button = gtkbutton_new(label="Remove item");
  button.connect[  "clicked",remove_item, list(treeview)]
  hbox.pack_start[ button,expand=%t,fill=%t,padding=0]

  window.set_default_size[  320, 200]
  window.show_all[];
endfunction 

