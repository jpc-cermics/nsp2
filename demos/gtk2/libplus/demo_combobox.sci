// testcombo.sci 
// Copyright (C) 2005-2008  Jean-Philippe Chancelier 
// translated and adapted to Nsp from 
// 
// testcombo.c  
// Copyright (C) 2003  Kristian Rietveld
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Library General Public License for more details.
//
// You should have received a copy of the GNU Library General Public
// License along with this library; if not, write to the
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.


function demo_combobox()

    function [pixbufs,names]=icon_collect(n)
  //collect the first n icons of GTK_STOCK
    window = gtkwindow_new ();
    ids = gtk_stock_list_ids ();
    pixbufs= list();
    names=[];
    for i=1:min(size(ids,1),n)
      [ok,l]=gtk_stock_lookup(ids(i));
      
      if ok then
	icon_set = gtk_icon_factory_lookup_default(l(1));
	sz=-1;
	if ~is(icon_set,%types.None) 
	  sz=icon_set.get_sizes[]
	  I=find(sz== GTK.ICON_SIZE_MENU)
	  if ~isempty(I) then
	    sz=I(1)
	  else
	    sz=sz(1)
	  end
	end
	// if an icon set is not found then
	// window.render_icon can crash !
	if sz<>-1 then 
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
	  pixbufs($+1)= small_icon;
	  names=[names;ids(i)];
	end
      end
    end
  endfunction

  
  function model=create_tree_store_model_for_combo(n)
  // create a tree model 
  // to be improved we only have one level 
    [pixbufs,names]=icon_collect(n);
    // the third element is used to set sensitivity of element 
    bools=ones(n,1)>=0;
    bools(3)=%f;
    // here we need to be able to use cells 
    // it would be better
    model = gtktreestore_new(list(pixbufs,names,bools));
    // model.set_data[nsp_data=list(pixbufs,names,bools)];
  endfunction

  function model=create_list_store_model_for_combo(n)
  // create a tree model 
  //
    [pixbufs,names]=icon_collect(n);
    // the third element is used to set sensitivity of element 
    bools=ones(n,1)>=0;
    bools(3)=%f;
    // here we need to be able to use cells 
    // it would be better
    model = gtkliststore_new(list(pixbufs,names,bools));
    // we also keep track of data 
    // model.set_data[nsp_data=list(pixbufs,names,bools)];
  endfunction

  function y=macro_set_func_set_sensitive(cell_view,cell,model,iter,data)
  // info_d = model.get_value[ iter,0] // pixbuf in column 0
  // info_d = model.get_value[ iter,1] // text in column one 
  // info_d = model.get_value[ iter,2] // sensitivity in column 2 
  // object on row 1 (first row is zero is set unsensitive;
  // sensitive = model.get_path[iter].get_indices[] <> 1 
    sensitive = model.get_value[ iter,2];
    // g_object_set (cell, "sensitive", sensitive, NULL);
    cell.set_property['sensitive',sensitive]
    y=%t;
  endfunction


  function y=macro_is_separator(cell_view,cell,model,iter,data)
  // add a separator in combo list : unfinished
    y= model.get_path[iter].get_indices[](1) == 4;
  endfunction

  function displayed_row_changed (combo,cell)
  // handler for the  GtkComboBox custom entry 
    printf("row changed\n");
    row = combo.get_active[];
    // path = gtk_tree_path_new_from_indices (row, -1);
    path = gtktreepath_new(row);
    cell(1).set_displayed_row[path=path];
  endfunction

  // mainfunction 
  
  window = gtkwindow_new ();
  window.set_title[  "ComboBox"]
  //window.set_default_size[  -1, 500]
  //window.connect[  "destroy", gtk_widget_destroyed, &window]
  window.set_border_width[  8]

  mainbox = gtkvbox_new(homogeneous=%f,spacing=2);
  window.add[mainbox]

  //1-- GtkCellView 
  //---------------

  hbox = gtkhbox_new();
  mainbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  
  tmp1 = gtkframe_new (label="GtkCellView");
  hbox.pack_start[ tmp1,expand=%f,fill=%f,padding=0]
  
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp1.add[boom];
  
  cellview = gtkcellview_new ();
  cell_renderer = gtkcellrendererpixbuf_new ();
  // get one pixbuf from stock 
  [pixbufs,names]=icon_collect(1)
  pixbuf = pixbufs[1];
  cellview.pack_start[ cell_renderer, expand= %f];
  // g_object_set (renderer, "text", "la la la", NULL);
  // pixbuf is in column 0
  cellview.add_attribute[cell_renderer,"pixbuf",0];
  cell_renderer.set_property["pixbuf",pixbuf];
  
  cell_renderer = gtkcellrenderertext_new ();
  cellview.pack_start[ cell_renderer, expand= %f];
  // text is in column 1 
  cellview.add_attribute[cell_renderer,"text",1];
  cell_renderer.set_property["text","Some text"];
  
  boom.add[cellview];

  //1.2-- GtkCellView (with text)
  //---------------
  tmp1 = gtkframe_new (label="GtkCellView (text=)");
  hbox.pack_start[ tmp1,expand=%f,fill=%f,padding=0]
  
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp1.add[boom];
  
  cellview = gtkcellview_new(text='Some text');
  boom.add[cellview];
  
  //1.3-- GtkCellView (with markup)
  //-----------------------------
  
  tmp1 = gtkframe_new (label="GtkCellView (markup=)");
  hbox.pack_start[ tmp1,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp1.add[boom];
  markup = "La terre est <span foreground=''blue''>bleue</span> comme une <span foreground=''orange''><i>orange</i></span>";
  //markup = "<span font_desc=''Symbols''>pOOO</span>";
  cellview = gtkcellview_new (markup=markup);
  boom.add[cellview];
  
  //1.4-- GtkCellView (with pixbuf) 
  //-----------------------------
  tmp1 = gtkframe_new (label="GtkCellView (pixbuf=)");
  hbox.pack_start[ tmp1,expand=%f,fill=%f,padding=0]
  
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp1.add[boom];

  [pixbufs,names]=icon_collect(1)
  pixbuf = pixbufs[1];
  cellview = gtkcellview_new (pixbuf=pixbuf);
  boom.add[cellview];

  
  //2--- GtkComboBox (from a list store model)
  //--------------------

  hbox = gtkhbox_new();
  mainbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  
  tmp = gtkframe_new (label="GtkComboBox (list store model)");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];

  model = create_list_store_model_for_combo(5);
  // new_with_model 
  combobox = gtkcombobox_new(model=model);
  
  combobox.set_add_tearoffs[%t];
  //g_object_unref (model);
  boom.add[combobox];

  cell_renderer = gtkcellrendererpixbuf_new ();
  // combobox can use gtk_cell_layout
  combobox.pack_start[ cell_renderer, expand= %f];
  // column 0 is used for pixbuf
  combobox.add_attribute[cell_renderer,"pixbuf",0];
  // the sensitive property will be set in the function macro_set_func_set_sensitive
  combobox.set_cell_data_func[cell_renderer,macro_set_func_set_sensitive];
  
  cell_renderer = gtkcellrenderertext_new ();
  
  combobox.pack_start[ cell_renderer,expand= %t];
  combobox.add_attribute[cell_renderer,"text",1];
  // the sensitive property will be set in the function macro_set_func_set_sensitive
  combobox.set_cell_data_func[cell_renderer,macro_set_func_set_sensitive];

  //XXXX Fixme: to be done 
  //combobox.set_row_separator_func[macro_is_separator];
  
  // default value 
  combobox.set_active[0];

  //5-- GtkComboBox (grid mode) */
  
  tmp = gtkframe_new (label="GtkComboBox (grid mode)");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];

  model = create_list_store_model_for_combo(20);
  combobox = gtkcombobox_new(model=model);
  combobox.set_add_tearoffs[%t];
  boom.add[combobox];
  
  cell_renderer = gtkcellrendererpixbuf_new ();
  combobox.pack_start[ cell_renderer, expand= %f];
  combobox.add_attribute[cell_renderer,"pixbuf",0];
  // 5 per lines 
  combobox.set_wrap_width[5];
  combobox.set_active[0];
  
  //--3 GtkComboBox custom entry 
  //-----------------------------

  tmp = gtkframe_new (label="GtkComboBox (custom)");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];
  
  model = create_list_store_model_for_combo(5);
  // with_model 
  combobox = gtkcombobox_new(model=model);
  combobox.set_add_tearoffs[%t];
  //g_object_unref (model);
  boom.add[combobox];

  cell_renderer = gtkcellrendererpixbuf_new ();
  // combobox can use gtk_cell_layout
  combobox.pack_start[ cell_renderer, expand= %f];
  // column 0 is used for pixbuf
  combobox.add_attribute[cell_renderer,"pixbuf",0];
  // the sensitive property will be set in the function macro_set_func_set_sensitive
  combobox.set_cell_data_func[cell_renderer,macro_set_func_set_sensitive];
  
  cell_renderer = gtkcellrenderertext_new ();
  combobox.pack_start[ cell_renderer,expand= %t];
  combobox.add_attribute[cell_renderer,"text",1];
  // the sensitive property will be set in the function macro_set_func_set_sensitive
  combobox.set_cell_data_func[cell_renderer,macro_set_func_set_sensitive];

  combobox.set_active[0];
  
  tmp = gtkcellview_new ();
  tmp.show[];
  tmp.set_model[model=model];
  renderer = gtkcellrenderertext_new ();
  tmp.pack_start[ cell_renderer, expand= %f];
  tmp.add_attribute[cell_renderer,"text",1];
  color = gdkcolor_new(0xffff, 0xffff,0,0);
  tmp.set_background_color[color];
  //   gtk_cell_view_set_background_color (GTK_CELL_VIEW (tmp), &color);
  displayed_row_changed(combobox,list(tmp));
  combobox.connect[ "changed", displayed_row_changed,list(tmp)]; 
  combobox.add[tmp];

  //3--- GtkComboBox (with_text)
  //--------------------

  tmp = gtkframe_new (label="GtkComboBox (text)");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];
  // new_with_text, text added at creation
  text = sprintf('combo text %d',(1:5)');
  //text= [text;"La terre est <span foreground=''blue''>bleue</span> comme une <span foreground=''orange''><i>orange</i></span>"];

  combobox = gtkcombobox_new(text=text);
  combobox.set_add_tearoffs[%t];
  boom.add[combobox];
  // default value 
  combobox.set_active[0];
  
  //-- 4 GtkComboBox tree
  
  hbox = gtkhbox_new();
  mainbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  
  tmp = gtkframe_new (label="GtkComboBox (tree model)");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];

  model = create_tree_store_model_for_combo(5);
  combobox = gtkcombobox_new(model=model);
  combobox.set_add_tearoffs[%t];
  //g_object_unref (model);
  boom.add[combobox];

  cell_renderer = gtkcellrendererpixbuf_new ();
  // combobox can use gtk_cell_layout
  combobox.pack_start[ cell_renderer, expand= %f];
  // column 0 is used for pixbuf
  combobox.add_attribute[cell_renderer,"pixbuf",0];
  // the sensitive property will be set in the function macro_set_func_set_sensitive
  combobox.set_cell_data_func[cell_renderer,macro_set_func_set_sensitive];
  
  cell_renderer = gtkcellrenderertext_new ();
  combobox.pack_start[ cell_renderer,expand= %t];
  combobox.add_attribute[cell_renderer,"text",1];
  // the sensitive property will be set in the function macro_set_func_set_sensitive
  combobox.set_cell_data_func[cell_renderer,macro_set_func_set_sensitive];

  //XXXX Fixme: to be done 
  //combobox.set_row_separator_func[macro_is_separator];
  
  // default value 
  combobox.set_active[0];

  function combo_changed(combo,args)
  // this can be used as handler for 
  // combobox.connect["changed", current_option ]
    M= combo.get_model[]
    iter=combo.get_active_iter[]
    name = M.get_value[iter,args(1)];
    printf('Value selected %s\n",name);
  endfunction
  
  combobox.connect["changed", combo_changed,list(1) ];
  
  //7 --  Phylogenetic tree */
  //-------------
  
  tmp = gtkframe_new (label="What are you ? (tree model)");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];

  model =  phylogenic_tree_model()
  combobox = gtkcombobox_new(model=model);
  combobox.set_add_tearoffs[%t];
  //g_object_unref (model);
  boom.add[combobox];
  
  cell_renderer = gtkcellrenderertext_new ();
  combobox.pack_start[ cell_renderer,expand= %t];
  combobox.add_attribute[cell_renderer,"text",0];
  // set active element in the combobox 
  combobox.set_active[0];

  combobox.connect["changed", combo_changed,list(0) ];
  
  //8 -- Capitals 
  //-------------
  tmp = gtkframe_new (label="Where are you ? (tree model)");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];

  model= create_capital_tree()

  combobox = gtkcombobox_new(model=model);
  combobox.set_add_tearoffs[%t];
  //g_object_unref (model);
  boom.add[combobox];
  
  cell_renderer = gtkcellrenderertext_new ();
  combobox.pack_start[ cell_renderer,expand= %t];
  combobox.add_attribute[cell_renderer,"text",0];
  // the sensitive property will be set in the function macro_set_func_set_sensitive
  combobox.set_cell_data_func[cell_renderer,macro_set_capital_sensitive];
  // set active element in the combobox 
  path= gtktreepath_new([0;8]);
  iter= model.get_iter[path];
  combobox.set_active_iter[iter];
  
  combobox.connect["changed", combo_changed,list(0)];
  
  
  //6 -- GtkComboBoxEntry */
  //-------------

  hbox = gtkhbox_new();
  mainbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  
  tmp = gtkframe_new (label="GtkComboBoxEntry");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];
  // creates a gtk_combo_box_entry_new_text
  // and append text during create
  //combobox = gtk_combo_box_entry_new_text ();
  //for i=1:5 ;  combobox.append_text[sprintf('entry text %d',i)];end
  
  text = sprintf('entry text %d',(1:5)');
  combobox =gtkcomboboxentry_new(text=text)
  boom.add[combobox];
  
  window.show_all[];
  
endfunction

function y=macro_set_capital_sensitive(cell_view,cell,model,iter,data)
// unset senstitivity to the X -- Y fields 
  sensitive = model.iter_has_child[iter];
  cell.set_property['sensitive',~sensitive]
  y=%t;
endfunction

function model= create_capital_tree()
// a tree store model for storing capitals
  Top = [ "A - B";
	  "C - D";
	  "E - J";
	  "K - O";
	  "P - S";
	  "T - Z"];
  capitals = list( [ "Albany";
		     "Annapolis";
		     "Atlanta";
		     "Augusta";
		     "Austin";
		     "Baton Rouge";
		     "Bismarck";
		     "Boise";
		     "Boston"],...

		   ["Carson City";
		    "Charleston";
		    "Cheyenne";
		    "Columbia";
		    "Columbus";
		    "Concord";
		    "Denver";
		    "Des Moines";
		    "Dover"],...

		   ["Frankfort";
		    "Harrisburg";
		    "Hartford";
		    "Helena";
		    "Honolulu";
		    "Indianapolis";
		    "Jackson";
		    "Jefferson City";
		    "Juneau"],...

		   ["Lansing";
		    "Lincoln";
		    "Little Rock";
		    "Madison";
		    "Montgomery";
		    "Montpelier";
		    "Nashville";
		    "Oklahoma City";
		    "Olympia"],...

		   ["Phoenix";
		    "Pierre";
		    "Providence";
		    "Raleigh";
		    "Richmond";
		    "Sacramento";
		    "Salem";
		    "Salt Lake City";
		    "Santa Fe";
		    "Springfield";
		    "St. Paul"],...
		   ["Tallahassee";
		    "Topeka";
		    "Trenton"]);
  
  model = gtktreestore_new(Top); 
  // fill the model at next level 
  it = model.get_iter_first[] 
  i=1;
  while %t 
    model.insert[it,0,list(capitals(i))];
    i = i+1;
    if model.iter_next[it] == %f 
      break;
    end
  end
endfunction


function model= phylogenic_tree_model()
// gives an example of a tree build from a 
// list tree 
  data = list( "Eubacteria",...
	       list(  "Aquifecales",...
		      "Thermotogales",...
		      "Thermodesulfobacterium",...
		      "Thermus-Deinococcus group",...
		      "Chloroflecales",...
		      "Cyanobacteria",...
		      "Firmicutes",...
		      "Leptospirillium Group",...
		      "Synergistes",...
		      "Chlorobium-Flavobacteria group",...
		      "Chlamydia-Verrucomicrobia group",...
		      list( ...
			  "Verrucomicrobia",...
			  "Chlamydia"),...
		      "Flexistipes",...
		      "Fibrobacter group",...
		      "spirocheteus",...
		      "Proteobacteria",...
		      list( "alpha",...
			    "beta",...
			    "delta ",...
			    "epsilon",...
			    "gamma ")),...
	       "Eukaryotes",...
	       list(  "Metazoa",...
		      "Bilateria",...
		      "Myxozoa",...
		      "Cnidaria",...
		      "Ctenophora",...
		      "Placozoa",...
		      "Porifera",...
		      "choanoflagellates",...
		      "Fungi",...
		      "Microsporidia",...
		      "Aleveolates",...
		      "Stramenopiles",...
		      "Rhodophyta",...
		      "Viridaeplantae",...
		      "crytomonads et al"),...
	       "Archaea ",...
	       list(     "Korarchaeota",...
			 "Crenarchaeota",...
			 "Buryarchaeota"));
  
  function phylogenic_tree_model_append(model,h,iter) 
    for i=1:length(h)
      t = type(h(i),'string');
      // On first call to this recursive function iter is not 
      // a GtkTreeIter
      if t == 'SMat' then 
	if is(iter,%types.GtkTreeIter) then 
	  iter1=model.append[iter,list(h(i))];
	else 
	  iter1=model.append[list(h(i))];
	end
      else
	phylogenic_tree_model_append(model,h(i),iter1);
      end
    end
  endfunction
  
  model = gtktreestore_new(list("name"),%f);
  phylogenic_tree_model_append(model,data,0);
  
endfunction

