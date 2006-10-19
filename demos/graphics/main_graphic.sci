//---------------------------------------------------
// Copyright (C) 2004 Jean-Philippe Chancelier Cermics/Enpc 
// jpc@cermics.enpc.fr 
// NSP 
// Calling demos of nsp graphics through a gtk widget 
// this file can also be considered as a demo of the 
// nsp gtk2 widget access
// it follows in a  nsp implementation the design 
// of the gtk-demo 
//
// The main function in this file is 
// graphics_demo_in_gtk(demo_list) 
// which is used to create a demo widget (see graphic_list.sce)
//--------------------------------------------------- 

function message(t,mess) 
  dialog = gtkmessagedialog_new (flags= GTK.DIALOG_MODAL,type= t,
                                 buttons= GTK.BUTTONS_OK, message = mess );
  dialog.run[];
  dialog.destroy[];
endfunction 

function window_closed_cb (window, data)
// the closed window handler 
endfunction 

function row_activated_cb (tree_view,path,data)
// activated when a row is selected 
  ITALIC_COLUMN=3;
  FUNC_COLUMN=2;
  model = tree_view.get_model[];
  iter = model.get_iter[path];
  func=model.get_value[iter,FUNC_COLUMN];
  italic= model.get_value[iter,ITALIC_COLUMN];
  winid = tree_view.get_data['winid'];
  if length(func)<>0 then 
    model.set[iter,ITALIC_COLUMN,~italic]; 
    str=sprintf("Execute string\n %s\n",func);
    // message(GTK.MESSAGE_INFO,str);
    // here we execute some drawings 
    // FIXME: we need here to know the graphic Id 
    // window to be sure to draw in the good window 
    wincur=xget('window');
    xset('window',winid);
    xclear()
    xset('default');
    execstr(func+"();",errcatch=%t);
    xset('window',wincur);
    model.set[iter,ITALIC_COLUMN,italic]; 
  end
endfunction 

function selection_cb(selection,args)
  FUNC_COLUMN=2
  model=args(1);
  buffer=args(2);
  iter=selection.get_selected[]
  if ~is(iter,%types.GtkTreeIter) then return;end 
  code= model.get_value[iter,FUNC_COLUMN]
  if code=="" then return;end 
  // Only execute somethink for tree leaves
  // code is a call to a funcion 
  // we convert it to string
  execstr("text=pl2s("+code+");")
  buffer.set_text[""];
  t_iter = buffer.get_start_iter[];
  for i=2:(size(text,'*')-1); 
    buffer.insert[t_iter,text[i]+"\n"];
  end
endfunction 

function [scrolled_window,buffer]=create_text(is_source)
//
  scrolled_window = gtkscrolledwindow_new();
  scrolled_window.set_policy[ GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC]
  scrolled_window.set_shadow_type[ GTK.SHADOW_IN]
  
  text_view = gtktextview_new ();
  
  buffer = gtktextbuffer_new ();
  text_view.set_buffer[buffer];
  text_view.set_editable[%f];
  text_view.set_cursor_visible[%f];
  
  scrolled_window.add[text_view]
  
  if is_source then 
    font_desc = pangofontdescription_new ("Courier 12");
    text_view.modify_font[font_desc]
    text_view.set_wrap_mode[GTK.WRAP_NONE]; 
  else
    text_view.set_wrap_mode[GTK.WRAP_WORD]; 
    text_view.set_pixels_above_lines[2];
    text_view.set_pixels_below_lines[2];
  end
endfunction 
  
function tree_view=create_tree(demo_list)
// insert the demo list in the tree_view 
  model = gtktreestore_new(list("title","fname","fname",%t),%f);
  tree_view = gtktreeview_new ();
  tree_view.set_model[model=model];
  selection = tree_view.get_selection[];
  selection.set_mode[ GTK.SELECTION_BROWSE];
  tree_view.set_size_request[  200, -1]
  // walk through demo_list and insert
  tree_model_append(model,demo_list,0,0) 
  cell = gtkcellrenderertext_new ();
  // g_object_set (G_OBJECT (cell), "style", PANGO_STYLE_ITALIC, NULL);
  cell.set_property["style", PANGO.STYLE_ITALIC];
  TITLE_COLUMN=0;
  ITALIC_COLUMN=3;
  attrs=hash_create(text= TITLE_COLUMN,style_set=ITALIC_COLUMN);
  col = gtktreeviewcolumn_new(title="Widget (double click for demo)",renderer=cell,attrs=attrs);
  tree_view.append_column[col];
  tree_view.connect["row_activated",row_activated_cb,list(model)]
  tree_view.expand_all[];
endfunction

function tree_model_append(model,L,iter,count) 
// A recursive function which walk through the given hash 
// table and insert all the elements in the tree 
// when an element is itself a Hash table we enter a 
// recursive call 
  if count== 3 then return;end;
  //printf("In tree_model_append %d,%d\n",count,size(L,0))
  for i=1:size(L,0);
    r=L(i)
    //printf("In for %d\n",count,size(r,0))
    if size(r,0)==3 then 
      r($+1)=%f; 
      if is(iter,%types.GtkTreeIter) then 
	iter1=model.append[iter,r];
      else
	iter1=model.append[r];
      end
    else
      Lc=r($);r($)=%f; 
      if is(iter,%types.GtkTreeIter) then 
	iter1=model.append[iter,r];
      else
	iter1=model.append[r];
      end
      tree_model_append(model,Lc,iter1,count+1);
    end
  end
endfunction

function [simple_demo]=graphics_demos_test_list()
// just return a list which can be used as 
// graphics_demo_in_gtk argument 
  tree_view_children = list(
    list( "Poo", "unused", "''poo''"),
    list( "Foo", "list_store.c", "''foo''" ));
  
  simple_demo = list( 
  list("Plot3d", "appwindow.c", "''poo''" ), 
  list("test", "appwindow.c", "''poo''" ),
  list("Childs1", "", "",tree_view_children ),
  list("Childs1", "", "",tree_view_children ),
  list("test", "appwindow.c", "''poo''" ));
endfunction

function graphics_demo_in_gtk(demo_list,ogl) 
  window = gtkwindow_new();// (GTK.WINDOW_TOPLEVEL);
  window.set_title[  "GTK+ Code Demos"]
  //FIXME: to be added 
  //window.connect[  "destroy", hide];
  hbox = gtkhbox_new(homogeneous=%f,spacing=0);
  window.add[hbox]
  tree = create_tree (demo_list);
  hbox.pack_start[ tree,expand=%f,fill=%f,padding=0]

  notebook = gtknotebook_new ();
  hbox.pack_start[ notebook,expand=%t,fill=%t,padding=0]
  
  //[sw,info_buffer]=create_text ( %f);
  //notebook.append_page[sw, gtklabel_new(mnemonic="_Info")];
  [sw,source_buffer]=create_text ( %t);
  hb = gtkhbox_new();
  notebook.append_page[hb, gtklabel_new(mnemonic="_Graphics")]; 
  notebook.append_page[sw, gtklabel_new(mnemonic="_Source")]; 
  // 
  winid=nsp_graphic_new(window,hb,opengl=ogl);
  // we must keep track of the winid somewhere 
  // just in case other graphics windows are also activated.
  // this is used in row_activated_cb
  tree.set_data[winid=winid];
  
  hb = gtkhbox_new();
  
  selection = tree.get_selection[];
  model = tree.get_model[];
  selection.connect["changed", selection_cb,list(model,source_buffer)]
  // tag = info_buffer.create_tag[ "title", font= "Sans 18"]; 
  tag = source_buffer.create_tag[ "comment",foreground= "red"]; 
  tag = source_buffer.create_tag[ "type",foreground= "ForestGreen"]; 
  tag = source_buffer.create_tag[ "string",foreground= "RosyBrown",weight= PANGO.WEIGHT_BOLD]; 
  tag = source_buffer.create_tag[ "control",foreground= "purple"];
  tag = source_buffer.create_tag[ "preprocessor",style= PANGO.STYLE_OBLIQUE,foreground= "burlywood4"];
  tag = source_buffer.create_tag[ "function",weight=PANGO.WEIGHT_BOLD,foreground= "DarkGoldenrod4"];
  window.set_default_size[  600, 400]
  window.show_all[];
endfunction 



