// Stock Item and Icon Browser
// Can be used as a demo for TreeView 

function macro=id_to_macro (id) 
  macro = "GTK_STOCK_" 
  if part(id,1:4)== "gtk-" then 
    n=length(id);
    id= part(id,5:n);
  end
  macro = macro+id 
  macro= strsubst(macro,'-','_')
  macro= toupper(macro);
endfunction
    
function ids1=icon_collect(window)
  ids = gtk_stock_list_ids ();
  ids1= list();
  for i=1:size(ids,1)
    [ok,l]=gtk_stock_lookup(ids(i));
    // l = stock_id, label,modifier,keyval,translation_domain);
    if ok then 
      icon_set = gtk_icon_factory_lookup_default(l(1));
      if ~is(icon_set,%types.None) 
	sz=icon_set.get_sizes[]
	I=find(sz== GTK.ICON_SIZE_MENU)
	if I<>[] then 
	  sz=I(1) 
	else
	  sz=sz(1);
	end
      end
      small_icon = window.render_icon[l(1),sz]	
      if sz <> GTK.ICON_SIZE_MENU 
	// Make the result the proper size for our thumbnail */
	[wh]= gtk_icon_size_lookup(GTK.ICON_SIZE_MENU);
	small_icon =  small_icon.scale_simple[ wh(1),wh(2), GDK.INTERP_BILINEAR];
      end
      if l(4)<>0 then 
	accel_str = gtk_accelerator_name (l(4),l(3));
      else
	accel_str = "";
      end
      ids1($+1)= list(ids(i), l,sz,small_icon,accel_str, id_to_macro(ids(i)));
    end
  end
endfunction 

function best_size=get_largest_size (id)
  icon_set = gtk_icon_factory_lookup_default (id);
  best_size = GTK.ICON_SIZE_INVALID;
  best_pixels = 0;
  sz= icon_set.get_sizes[];
  for s=sz 
    wh= gtk_icon_size_lookup (s);
    if wh(1) <> -1 then 
      if wh(1)*wh(2) > best_pixels 
	best_size = s;
	best_pixels = wh(1)*wh(2) 
      end
    end
  end
endfunction

function selection_changed (selection)
  treeview = selection.get_tree_view[];
  display = treeview.get_data["stock_display"];
  [iter,model] = selection.get_selected[]
  if is(iter,%types.None) then  return; end ;
  // iter is a GtkTreeIter 
  info_d = model.get_value[ iter,0]
  // l = list(ids(i), l1,sz,small_icon,accel_str, id_to_macro(ids(i)));
  //     l1 = list(stock_id, label,modifier,keyval,translation_domain)
  l= model.get_data['nsp_data'](info_d); 
  display(1).set_text[l(6)]; 
  display(2).set_text[l(1)]; 
  display(3).set_text_with_mnemonic[sprintf("%s %s",l(2)(2),l(5))];
  display(4).set_from_stock[l(2)(1),get_largest_size (l(2)(1))];
endfunction

// XXXXXX a modifier pour que l'acces  l=
// model.get_data['nsp_data'](info_d)(6); 
// ne fasse pas de copie 

function macro_set_func_text(tree_column,cell,model,iter,data)
  info_d = model.get_value[ iter,0]
  l= model.get_data['nsp_data'](info_d)(6);
  cell.set_property["text",l];// info_d->macro;
endfunction

function macro_set_func_pixbuf(tree_column,cell,model,iter,data)
  info_d = model.get_value[ iter,0]
  l= model.get_data['nsp_data'](info_d)(4);
  cell.set_property["pixbuf",l];// info_d->macro;
endfunction

function id_set_func (tree_column,cell,model,iter,data)
  info_d = model.get_value[ iter,0]
  l= model.get_data['nsp_data'](info_d)(1);
  cell.set_property["text",l];//info_d->id
endfunction

function accel_set_func (tree_column,cell,model,iter,data)
  info_d = model.get_value[ iter,0]
  l= model.get_data['nsp_data'](info_d)(5);
  cell.set_property["text", l];//info_d->accel_str
endfunction

function label_set_func (tree_column,cell,model,iter,data)
  info_d = model.get_value[ iter,0];
  l= model.get_data['nsp_data'](info_d)(2)(2);
  cell.set_property["text", l];// info_d ->item.label
endfunction 

function demo_stock_browser () 
  window = gtkwindow_new ();
  window.set_title[  "Stock Icons and Items"]
  window.set_default_size[  -1, 500]
  //window.connect[  "destroy", gtk_widget_destroyed, &window]
  window.set_border_width[  8]

  hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  window.add[  hbox]

  sw = gtkscrolledwindow_new ();
  sw.set_policy[ GTK.POLICY_NEVER, GTK.POLICY_AUTOMATIC]
  hbox.pack_start[ sw,expand=%f,fill=%f,padding=0]
  
  icon_data = icon_collect(window);
  n= size(icon_data,1);
  pix = list();
  for i=1:n 
    pix(i) = icon_data(i)(4);
  end
  model = gtkliststore_new(list((1:n)',pix));
  model.set_data[nsp_data=icon_data];
  
  treeview = gtktreeview_new(model);
  sw.add[treeview]
      
  col = gtktreeviewcolumn_new ();
  col.set_title["Macro"];
  
  cell_renderer = gtkcellrendererpixbuf_new ();
  col.pack_start[ cell_renderer,  %f];
  //  col.set_attributes[cell_renderer,hcreate(1,stock_id=1) ];
  col.set_cell_data_func[ cell_renderer, macro_set_func_pixbuf];
    
  cell_renderer = gtkcellrenderertext_new ();
  col.pack_start[ cell_renderer, %t];
  col.set_cell_data_func[ cell_renderer, macro_set_func_text];
  treeview.append_column[ col]
    
  cell_renderer = gtkcellrenderertext_new ();
  treeview.insert_column_with_data_func[ -1, "Label", cell_renderer, label_set_func];

  cell_renderer = gtkcellrenderertext_new ();
  treeview.insert_column_with_data_func[ -1 ,"Accel", cell_renderer, accel_set_func];  

  cell_renderer = gtkcellrenderertext_new ();
  treeview.insert_column_with_data_func[ -1, "ID",  cell_renderer,  id_set_func];
        
  align = gtkalignment_new(xalign=0.5,yalign=0.0,xscale=0.0,yscale=0.0);
  hbox.pack_end[ align, expand=%f,fill=%f,padding=0]
      
  frame = gtkframe_new(label="Selected Item");
  align.add[  frame]

  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  4]
  frame.add[  vbox]
  
  //g_object_set_data_full (G_OBJECT (treeview), "stock-display",display, g_free); 
  display= list( gtklabel_new(),gtklabel_new(), gtklabel_new(), gtkimage_new());
  treeview.set_data[stock_display=display];
  for el=display ;  vbox.pack_start[el, expand=%f,fill=%f,padding=0];end 
  selection = treeview.get_selection[];
  selection.set_mode[GTK.SELECTION_SINGLE];
  selection.connect["changed",selection_changed]
  window.show_all[];
endfunction 

