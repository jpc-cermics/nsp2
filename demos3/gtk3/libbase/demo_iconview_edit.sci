//  Icon View/Editing and Drag-and-Drop
//
// The GtkIconView widget supports Editing and Drag-and-Drop.
// This example also demonstrates using the generic GtkCellLayout
// interface to set up cell renderers in an icon view.

function window=demo_iconview_edit (do_widget)

  function fill_store (store)
    text= ["Red", "Green", "Blue", "Yellow"];
    store.clear[];
    for i = 1:size(text,'*')
      iter = store.append[];
      store.set[iter, 0, text(i)];
    end
  endfunction 

  function store = create_store (void)
    store = gtk_list_store_new(list(""));
  endfunction

  function set_cell_color (cell_layout, cell, tree_model, iter, data)
    text= tree_model.get_value[iter,0]
    ok = execstr('color = gdk_rgba_new(text)',errcatch=%t)
    if ~ok then lasterror(); return;end 
    printf("set color %s\n",text);
    pixel = [ishift(m2i(color.red * 255,"uint32"),24), ...
	     ishift(m2i(color.green * 255,"uint32"),16), ...
	     ishift(m2i(color.blue * 255,"uint32"),8), ...
	     m2i(color.alpha * 255)];
    pixbuf = gdk_pixbuf_new (GDK.COLORSPACE_RGB, %t, 8, 24, 24);
    if i2m(sum(pixel)) < 0 then pause;end
    pixbuf.fill[i2m(sum(pixel))];
    cell.set_property["pixbuf", pixbuf];
  endfunction 

  function edited (cell, path_string, text, data)
    model = data.get_model [];
    path = gtk_tree_path_new_from_string (path_string);
    iter = model.get_iter[ path];
    model.set[iter, 0,text];
  endfunction 

  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then 
    window.set_screen[do_widget.get_screen []];
  end
  window.set_title[ "Editing and Drag-and-Drop"];
  // window.connect[ "destroy", gtk_widget_destroyed, &window],

  store = create_store ();
  fill_store (store);

  icon_view = gtk_icon_view_new_with_model (store);
  
  icon_view.set_selection_mode[ GTK.SELECTION_SINGLE];
  icon_view.set_item_orientation[ GTK.ORIENTATION_HORIZONTAL];
  icon_view.set_columns[2];
  icon_view.set_reorderable[%t];

  renderer = gtk_cell_renderer_pixbuf_new ();
  icon_view.pack_start[renderer, expand=%t];
  icon_view.set_cell_data_func[ renderer, set_cell_color];
  
  renderer = gtk_cell_renderer_text_new ();
  icon_view.pack_start[ renderer, expand=%t];
  renderer.set_property["editable", %t];
  renderer.connect[ "edited", edited, icon_view];
  icon_view.set_attributes[ renderer, hash(text=0)];
  
  window.add[icon_view];

  window.show_all[];
endfunction 

