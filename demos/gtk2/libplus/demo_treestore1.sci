
// XXXX : si on met 1 a la place de "mxn" 
// le message d'erreur ensuite est pas clair a revoir 

function tree_view=create_tree(h)

  function message(t,mess) 
    dialog = gtkmessagedialog_new (flags= GTK.DIALOG_MODAL,
    type= t,
    buttons= GTK.BUTTONS_OK,
    message = mess );
    dialog.run[];
    dialog.destroy[];
  endfunction 

  function row_activated_cb (tree_view,path,data)
// $$$     model = tree_view.get_model[];
// $$$     iter = model.get_iter[path];
// $$$     func=model.get_value[iter,FUNC_COLUMN];
// $$$     italic= model.get_value[iter,ITALIC_COLUMN];
// $$$     if length(func)<>0 then 
// $$$       model.set[iter,ITALIC_COLUMN,~italic]; 
// $$$       str=sprintf(" Execute function \n %s\n",func);
// $$$       message(GTK.MESSAGE_INFO,str) 
// $$$ 	  cbdata = g_new (CallbackData, 1);
// $$$ 	  cbdata->model = model;
// $$$ 	  cbdata->path = gtk_tree_path_copy (path);
// $$$ 	  window.connect[  "destroy", window_closed_cb, cbdata]
// $$$     end
  endfunction 

  function selection_cb(selection,args)
// $$$   model=args(1);
// $$$   iter=selection.get_selected[]
// $$$   fname= model.get_value[iter,FILENAME_COLUMN]
// $$$   str=sprintf("file name to be displayed:\n %s\n",fname);
// $$$   //message(GTK.MESSAGE_INFO,str) 
  endfunction 

  function tree_model_append(model,h,iter) 
// A recursive function which walk through the given hash 
// table and insert all the elements in the tree 
// when an element is itself a Hash table we enter a 
// recursive call 
  h_names = h.__keys // to be changed XXXX 
  h_names =sort(h_names,'g','i')
  
  for name=h_names' 
    t = type(h(name),'string');
    // On first call to this recursive function iter is not 
    // a GtkTreeIter
    if is(iter,%types.GtkTreeIter) then 
      iter1=model.append[iter,list(name,t,sprintf("%dx%d",size(h(name),1),size(h(name),2)))];
    else 
      iter1=model.append[list(name,t,sprintf("%dx%d",size(h(name),1),size(h(name),2)))];
    end
    if t == 'Hash' 
      // we use the current iter1 to enter the given hashtable 
      tree_model_append(model,h(name),iter1);
    end
  end
  endfunction
  
  model = gtktreestore_new(list("var","type","mxn"),%f);
  tree_model_append(model,h,0);
  
  tree_view = gtktreeview_new ();
  tree_view.set_model[model=model];
  selection = tree_view.get_selection[];
  selection.set_mode[ GTK.SELECTION_BROWSE];
  tree_view.set_size_request[  200, -1]
  
  cell = gtkcellrenderertext_new ();
  col = gtktreeviewcolumn_new(title="Name",renderer=cell,attrs=hcreate(text= 0));
  tree_view.append_column[col];
  
  cell = gtkcellrenderertext_new ();
  col = gtktreeviewcolumn_new(title="Type",renderer=cell,attrs=hcreate(text= 1));
  tree_view.append_column[col];
  
  cell = gtkcellrenderertext_new ();
  col = gtktreeviewcolumn_new(title="mxn",renderer=cell,attrs=hcreate(text=  2));
  tree_view.append_column[col];
    
  selection.connect["changed", selection_cb,list(model)]
  tree_view.connect["row_activated",row_activated_cb,list(model)]
  tree_view.expand_all[];
endfunction

function setup_default_icon () 
  filename = "gtk-logo-rgb.gif";
  pixbuf = gdk_pixbuf_new_from_file(filename);
  // The gtk-logo-rgb icon has a white background, make it transparent
  transparent = pixbuf.add_alpha[ %t, 0xff, 0xff, 0xff];
  // XXXXX gtk_window_set_default_icon_list (list(transparent));
endfunction

function demo_treestore1() 
// 
  h=hcreate(A=89,B=%t,C=rand(4,4));
  h1=hcreate(A=89,B=%t,C=rand(4,4),h1=h,h2=h);
  h3=hcreate(A=78,foo=h1);
  
  setup_default_icon ();
  window = gtkwindow_new();// (GTK.WINDOW_TOPLEVEL);
  window.set_title[  "GTK+ Treestore demo"]
  //window.connect[  "destroy", hide];
  window.set_default_size[280,300]

  hbox = gtkhbox_new(homogeneous=%f,spacing=0);
  window.add[hbox]
  sw = gtkscrolledwindow_new();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC]
  hbox.pack_start[sw,expand=%t,fill=%t,padding=0]

  tree = create_tree (h3); 
  sw.add[tree]
  window.show_all[];
endfunction 


