//Tree Store
//
// The GtkTreeStore is used to store data in tree form, to be used later on 
// by a GtkTreeView to display it. This demo builds a simple GtkTreeStore 
// and displays it. If you're new to the GtkTreeView widgets and associates,
// look into the GtkListStore example first. 

function demo_tree_store_1()

  window = gtk_window_new()
  window.set_title["Gtk TreeStore demo"]
  //window.connect["destroy", gtk_widget_destroyed]
  window.set_border_width[  8]

  vbox = gtk_box_new("vertical",spacing=8);
  window.add[  vbox]
  label = gtk_label_new(str="This is a tree store test From a real Matrix .");
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

  sw = gtk_scrolled_window_new();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC]
  vbox.pack_start[sw,expand=%t,fill=%t,padding=0]
 
  // testing ways for filing tree model. 
  mode = 2
  if mode==1 then 
    M= testmatrix('magic',5);
    model = gtk_tree_store_new(M); 
    // fill the model at next level 
    it = model.get_iter_first[] 
    while %t 
      model.insert[it,0,list(rand(3,5))];
      if model.iter_next[it] == %f 
	break;
      end
    end
  end
      
  if mode == 2 
    M= testmatrix('magic',5);
    model = gtk_tree_store_new(list(1,2,3,4,5),%f); 
    for i=1:3 
      iter=model.append[list(1,2,3,4,5)];
      for j=1:3 
	model.append[iter,list([1:5]*2)];
      end
    end
  end
  
  if mode == 3 
    M= testmatrix('magic',5);
    model = gtk_tree_store_new(list(1,2,3,4,5),%f); 
    for i=1:3 
      iter=model.insert[0,list([1:5]*i)];
      for j=1:3 
	model.insert[iter,0,list([1:5]+i+j)];
      end
    end
  end
    
  // create tree view */

  treeview = gtk_tree_view_new(model);
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

