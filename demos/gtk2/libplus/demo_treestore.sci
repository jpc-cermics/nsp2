
function demo_treestore()
  demo_treestore_from_mat () 
endfunction


function demo_treestore_from_mat () 
  window = gtkwindow_new()
  window.set_title["Gtk TreeStore demo"]
  //window.connect["destroy", gtk_widget_destroyed]
  window.set_border_width[  8]

  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  window.add[  vbox]
  label = gtklabel_new(str="This is a tree store test From a real Matrix .");
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]

  sw = gtkscrolledwindow_new();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC]
  vbox.pack_start[sw,expand=%t,fill=%t,padding=0]
 
  // testing ways for filing tree model. 
  mode = 2
  if mode==1 then 
    M= testmatrix('magic',5);
    model = gtktreestore_new(M); 
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
    model = gtktreestore_new(list(1,2,3,4,5),%f); 
    for i=1:3 
      iter=model.append[list(1,2,3,4,5)];
      for j=1:3 
	model.append[iter,list([1:5]*2)];
      end
    end
  end
  
  if mode == 3 
    M= testmatrix('magic',5);
    model = gtktreestore_new(list(1,2,3,4,5),%f); 
    for i=1:3 
      iter=model.insert[0,list([1:5]*i)];
      for j=1:3 
	model.insert[iter,0,list([1:5]+i+j)];
      end
    end
  end
    
  // create tree view */

  treeview = gtktreeview_new(model);
  treeview.set_rules_hint[%t];
  treeview.set_search_column[3];

  sw.add[treeview]

  renderer = gtkcellrenderertext_new ();
  for i=1:size(M,'c') 
    col = gtktreeviewcolumn_new(title=sprintf("Col %d",i),renderer=renderer,attrs=hash(text=i-1));
    col.set_sort_column_id[i-1];
    treeview.append_column[col];
  end 
  window.set_default_size[  280, 250]
  window.show_all[];
endfunction 

