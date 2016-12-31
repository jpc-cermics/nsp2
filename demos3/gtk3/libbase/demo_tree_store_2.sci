//Tree Store
//
// The GtkTreeStore is used to store data in tree form, to be used later on 
// by a GtkTreeView to display it. This demo builds a simple GtkTreeStore 
// and displays it. If you're new to the GtkTreeView widgets and associates,
// look into the GtkListStore example first. 

function window= demo_tree_store_2()
//
  h=hash_create(A=89,B=%t,C=rand(4,4));
  h1=hash_create(A=89,B=%t,C=rand(4,4),h1=h,h2=h);
  h3=hash_create(A=78,foo=h1);
  
  function setup_default_icon ()
    nsp_logo = getenv('NSP')+'/demos3/gtk3/libplus/nsp.png';
    pixbuf = gdk_pixbuf_new_from_file(nsp_logo);
    // The gtk-logo-rgb icon has a white background, make it transparent
    transparent = pixbuf.add_alpha[ %t, 0xff, 0xff, 0xff];
    // FIXME: unfinihed .... gtk_window_set_default_icon_list (list(transparent));
  endfunction
  
  setup_default_icon ();
  
  window = gtk_window_new();// (GTK.WINDOW_TOPLEVEL);
  window.set_title[  "GTK+ Treestore demo"]
  //window.connect[  "destroy", hide];
  window.set_default_size[280,300]

  hbox = gtk_box_new("horizontal",spacing=0);
  window.add[hbox]
  sw = gtk_scrolled_window_new();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN]
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC]
  hbox.pack_start[sw,expand=%t,fill=%t,padding=0]

  function tree_view=create_tree(h)

    function message(t,mess)
      dialog = gtk_message_dialog_new (flags= GTK.DIALOG_MODAL,
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

    model = gtk_tree_store_new(list("var","type","mxn"),%f);
    tree_model_append(model,h,0);

    tree_view = gtk_tree_view_new ();
    tree_view.set_model[model=model];
    selection = tree_view.get_selection[];
    selection.set_mode[ GTK.SELECTION_BROWSE];
    tree_view.set_size_request[  200, -1]

    cell = gtk_cell_renderer_text_new ();
    col = gtk_tree_view_column_new(title="Name",renderer=cell,attrs=hash(text= 0));
    tree_view.append_column[col];

    cell = gtk_cell_renderer_text_new ();
    col = gtk_tree_view_column_new(title="Type",renderer=cell,attrs=hash(text= 1));
    tree_view.append_column[col];

    cell = gtk_cell_renderer_text_new ();
    col = gtk_tree_view_column_new(title="mxn",renderer=cell,attrs=hash(text=  2));
    tree_view.append_column[col];

    selection.connect["changed", selection_cb,list(model)]
    tree_view.connect["row_activated",row_activated_cb,list(model)]
    tree_view.expand_all[];
  endfunction

  
  tree = create_tree (h3);
  sw.add[tree]
  window.show_all[];
endfunction



