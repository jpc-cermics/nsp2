//  Icon View/Icon View Basics
//
// The GtkIconView widget is used to display and manipulate icons.
// It uses a GtkTreeModel for data storage, so the list store
// example might be helpful.

function demo_iconview_fill_store (window, parent,store)
//  First clear the store
  store.clear[];
  if ~file('exists',parent) then return;end 
  
  // Now go through the directory and extract all the file
  // information  
  ok=execstr("F=glob(parent+""/*"");",errcatch=%t);
  if ~ok then printf(catenate(lasterror()));return;end 
  
  file_pixbuf = window.get_data["file_pixbuf"];
  folder_pixbuf = window.get_data["folder_pixbuf"];
  for i=1:size(F,'*') 
    //  We ignore hidden files that start with a '.'  
    if part(F,1)=="." then continue;end 
    is_dir = file('isdirectory',F(i));
    name = file('tail',F(i));
    iter = store.append[];
    store.set[iter,0,F(i)];
    store.set[iter,1,name];
    store.set[iter,3,is_dir];
    if is_dir then
      store.set[iter,2,folder_pixbuf];  
    else
      store.set[iter,2,file_pixbuf];
    end
  end
endfunction 


function window=demo_iconview (do_widget)

  function item_activated (icon_view, tree_path, window)
  store = window.get_data["store"]
  up_button = window.get_data["up"];
  
  iter = store.get_iter[tree_path];
  path= store.get_value[iter, 0];
  is_dir = store.get_value[iter, 3];
  if ~is_dir then return;end 
  //  Replace parent with path and re-fill the model 
  parent = path;
  window.set_data[parent=parent];
  demo_iconview_fill_store (window,parent,store);
  //  Sensitize the up button  
  up_button.set_sensitive[%t];
endfunction 

function up_clicked (item, window)
  store = window.get_data["store"]
  parent = window.get_data["parent"];
  up_button = window.get_data["up"];
  newp = file('split',parent);
  newp = file('join',newp(1:$-1));
  window.set_data[parent=newp];
  parent=newp;
  demo_iconview_fill_store (window,parent,store);
  //  Maybe de-sensitize the up button  
  up_button.set_sensitive[ parent <> "/"];
endfunction 

function home_clicked (item, window)
  store = window.get_data["store"]
  up_button = window.get_data["up"];
  parent = glob('~/');
  window.set_data[parent=parent];
  demo_iconview_fill_store (window,parent,store);
  //  Sensitize the up button  
  up_button.set_sensitive[%t];
endfunction
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_default_size[650, 400];
  if nargin >= 1 then 
    window.set_screen[do_widget.get_screen []];
  end
  window.set_title[ "Icon View Basics"];
  
  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing= 0);
  window.add[vbox];

  tool_bar = gtk_toolbar_new ();
  vbox.pack_start[tool_bar, expand=%f,fill= %f, padding= 0];
  
  up_button = gtk_tool_button_new ();
  up_button.set_label["_Up"];
  up_button.set_use_underline[%t];
  up_button.set_icon_name[ "go-up"];
  up_button.set_is_important[%t];
  up_button.set_sensitive[%f];
  tool_bar.insert[ up_button,-1];
  
  home_button = gtk_tool_button_new ();
  home_button.set_label[ "_Home"];
  home_button.set_use_underline[%t];
  home_button.set_icon_name[ "go-home"];
  home_button.set_is_important[%t];
  tool_bar.insert[ home_button,-1];

  sw = gtk_scrolled_window_new ();
  sw.set_shadow_type[GTK.SHADOW_ETCHED_IN];
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];

  vbox.pack_start[sw,expand= %t,fill= %t,padding= 0];

  //  Create the store and fill it with the contents of '/'  
  fname = getenv('NSP')+'/demos3/gtk3/libbase/demo_iconview/gnome-fs-regular.png';
  file_pixbuf = gdk_pixbuf_new_from_file(fname);
  
  fname = getenv('NSP')+'/demos3/gtk3/libbase/demo_iconview/gnome-fs-directory.png';
  folder_pixbuf = gdk_pixbuf_new_from_file(fname);
  window.set_data[file_pixbuf=file_pixbuf];
  window.set_data[folder_pixbuf=folder_pixbuf];
  
  parent = "/";
  
  function store = create_store (pixbuf)
    
    function y = sort_func (model, a, b, user_data)
    //  We need this function because we want to sort
    // folders before files.
      is_dir_a = model.get_value[a,3]
      name_a = model.get_value[a,1]
      
      is_dir_b = model.get_value[b,3]
      name_b = model.get_value[b,1]
      
      if ( ~is_dir_a && is_dir_b)
	ret = 1;
      elseif (is_dir_a && ~is_dir_b)
	ret = -1;
      else
	ret = g_utf8_collate (name_a, name_b);
      end
      y=ret;
    endfunction 
    
    store = gtk_list_store_new (list("","",list(pixbuf),%t));
    store.set_default_sort_func[sort_func];
    // GTK.TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID,
    store.set_sort_column_id[-1, GTK.SORT_ASCENDING];
  endfunction 

  store = create_store (file_pixbuf);
  demo_iconview_fill_store (window,parent,store);

  icon_view = gtk_icon_view_new_with_model (store);
  icon_view.set_selection_mode[ GTK.SELECTION_MULTIPLE];
  
    //  Connect to the "clicked" signal of the "Up" tool button  
  up_button.connect[ "clicked", up_clicked, window];
  
  //  Connect to the "clicked" signal of the "Home" tool button  
  home_button.connect[ "clicked", home_clicked, window];
    
  //  We now set which model columns that correspond to the text
  // and pixbuf of each item
  
  icon_view.set_text_column[1]
  icon_view.set_pixbuf_column[2];
  
  //  Connect to the "item-activated" signal  
  icon_view.connect[ "item-activated", item_activated, window];
  
  window.set_data[store=store];
  window.set_data[parent=parent];
  window.set_data[up=up_button];
  
  sw.add[icon_view];
  icon_view.grab_focus[];
  window.show_all[];
endfunction

