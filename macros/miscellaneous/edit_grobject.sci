// figure 
//---------------------

function x=edit_object_figure(x,varargopt)
  x=edit_grobject(x,varargopt(:));
  xbasr(x.id);
endfunction

function x=edit_object_axes(x,varargopt)
  x=edit_grobject(x,varargopt(:));
endfunction

function x=edit_object_curve(x,varargopt)
  x=edit_grobject(x,varargopt(:));
endfunction

function x=edit_object_polyline(x,varargopt)
  x=edit_grobject(x,varargopt(:));
endfunction

function x=edit_object_gmatrix(x,varargopt)
  x=edit_grobject(x,varargopt(:));
endfunction

function x=edit_object_spolyhedron(x,varargopt)
  x=edit_grobject(x,varargopt(:));
endfunction

function x=edit_object_objs3d(x,varargopt)
  x=edit_grobject(x,varargopt(:));
endfunction

function L=edit_grobject(L,with_scroll=%t,title="Edit Graphic object",size_request=[],headers=%t,top=[])
  
  function Il =get_nsp_list_path_from_tree_path(tree_view,path)
  // here we must build a list which permits to 
  // get the value of the selected row from a 
  // gtktreepath path. 
  // we cannot use path.get_list_indices[] 
  // because the acces to hash element must be 
  // performed with strings not with indices 
  // (it works with indices but their order 
  //  changes with copy or affectations).
  // 
    model = tree_view.get_model[];
    etype=type(tree_view.user_data,'short');
    // get a path with 0-based indices 
    I=path.get_indices[];
    // walk along the path and build 
    // a list to access to the given element 
    Il=list();
    for p=1:length(I)
      np = gtktreepath_new(I(1:p));
      iter1 = model.get_iter[np];
      name=model.get_value[iter1,0];
      if etype == 'l' then 
	// we search in a list 
	Il($+1)=I(p)+1;
      else 
      	// we search in a hash table or gaphic object 
	Il($+1)=name;
      end
      // update type.
      etype=model.get_value[iter1,1];
    end
  endfunction
  
  function tree_model_append(model,h,iter) 
  // A recursive function which walks through the given graphic 
  // object and build a tree model.
  // The call is recursive.

    h_names = h.get_attribute_names[];
    h_names = sort(h_names,'g','i');
    // just keep the children names 
    if %f then 
      if  h_names.has['children'] then 
	h_names='children';
      else
	h_names=m2s([]);
      end
    end
    for i=1:size(h_names,'*');
      name = h_names(i);
      objname = h.get[name];
      t = type(objname,'short');
      // we need an-other-function here XXXX 
      value = cellstostr({objname});
      if t== 'h' || t == 'l' then 
	osize = sprintf("%d",size(objname,1));
      else
	osize= sprintf("%dx%d",size(objname,1),size(objname,2))
      end
      // On first call to this recursive function iter is not a GtkTreeIter
      if is(iter,%types.GtkTreeIter) then 
	iter1=model.append[iter,list(name,t,osize,value)];
      else 
	iter1=model.append[list(name,t,osize,value)];
      end
      // recursive call we use the current iter1 to enter the given hashtable 
      if t == 'h' || t == 'l' then 
	for j=1:length(objname);
	  name = sprintf("(%d)",j);
	  iter2=model.append[iter1,list(name,type(objname(j),'short'),"","*")];
	  tree_model_append(model,objname(j),iter2);
	end
      end
    end
  endfunction
  
  function message(t,mess) 
    dialog = gtkmessagedialog_new (flags= GTK.DIALOG_MODAL,...
				   type= t,...
				   buttons= GTK.BUTTONS_OK,...
				   message = mess );
    dialog.run[];
    dialog.destroy[];
  endfunction 

  function row_activated_cb (tree_view,path,data)
    // this callback is activated when a row is activated. 
    // i.e double click. 
    model = tree_view.get_model[];
    iter = model.get_iter[path];
    name=model.get_value[iter,0]; // column 0 is the name 
    L = get_nsp_list_path_from_tree_path(tree_view,path);
    L1=L;L1.remove_last[];
    name = L.last[];
    if type(name,'short')=='m' then 
      // we have clicked on a children element 
      stype = type(treeview.user_data(L),'short');
      M = tree_view.user_data(L);
    else
      // we have to follow a path then get 
      // and attribute 
      if length(L1)<> 0 then 
	stype = type(treeview.user_data(L1),'short');
	M = tree_view.user_data(L1).get[name];
      else
	stype = type(treeview.user_data,'short');
	M = tree_view.user_data.get[name];
      end 
    end
    // here we need a generic edit 
    M1=edit_object(M,parent=tree_view);
    if ~M1.equal[M] then 
      if type(name,'short')=='m' then 
	tree_view.user_data(L)=M1;
      else
	if length(L1)<> 0 then 
	  execstr('tree_view.user_data(L1).set['+name+'=M1]');
	else
	  execstr('tree_view.user_data.set['+name+'=M1]');
	end
      end
      xs = cellstostr({M1});
      octype = type(M,'short');
      ctype = type(M1,'short');
      model.set[iter,3,xs];
      model.set[iter,2,sprintf("%dx%d',size(M1,1),size(M1,2))];
    end
  endfunction 
  
  function selection_cb(selection,args)
    // this callback is activated when a row is selected 
    tree_view=args(1);
    model =tree_view.get_model[];
    iter=selection.get_selected[];
    if type(iter,'short')== 'none' then return;end 
    fname= model.get_value[iter,0];
  endfunction 
  
  function cell_edited (cell,path_string,new_text,data)
    // we enter this function after cell edition for 
    // strings or numbers 
      if new_text=="" || new_text=="*" then return;end 
      tree_view = data(1);
      model = tree_view.get_model[];
      path = gtktreepath_new(path_string);
      iter = model.get_iter[path_string];
      octype = model.get_value[iter,1];
      //if ctype == 'l' || ctype == 'h' then return;end 
      // evaluate newtext 
      ok=execstr('val='+new_text',errcatch=%t);
      if ~ok then 
	// just push the text and change the type to string 
	// except if old object type was list or hash 
	if octype == 'l' || octype == 'h' then return;end 
	model.set[iter,3,new_text];
	model.set[iter,2,"1x1"];
	model.set[iter,1,"s"];
      else
	ctype = type(val,'short');
	model.set[iter,1,ctype];
	model.set[iter,2,sprintf("%dx%d",size(val,1),size(val,2))];
	model.set[iter,3,cellstostr({val})];
	// we also need to change the string in the user_data 
	Il =get_nsp_list_path_from_tree_path(tree_view,path);
	tree_view.user_data(Il) = val;
	if octype == 'l' || octype == 'h' || ctype == 'l' || ctype == 'h' then 
	  //  we need to update the treeview 
	  update_model([],list(tree_view))
	end 
      end
  endfunction

  function update_model(button,data)
    tree_view = data(1);
    // this function is to be called when 
    // the model is not in adequation with tree_view.user_data. 
    model = gtktreestore_new(list("var","type","mxn","value"),%f);
    tree_model_append(model,tree_view.user_data,0);
    tree_view.set_model[model=model];
  endfunction
    
  function tree_view=create_tree_view(h)
    model = gtktreestore_new(list("var","type","mxn","value"),%f);
    tree_model_append(model,h,0);
    tree_view = gtktreeview_new ();
    tree_view.set_model[model=model];
    selection = tree_view.get_selection[];
    selection.set_mode[ GTK.SELECTION_BROWSE];
    //tree_view.set_size_request[ -1,-1];
    names=["Name","Type","mxn","value"];
    for i=1:4 
      cell = gtkcellrenderertext_new ();
      col = gtktreeviewcolumn_new(title=names(i),renderer=cell,attrs=hash_create(text=i-1));
      if i==4 then 
	cell.set_property['editable',%t];
	cell.connect["edited",cell_edited,list(tree_view)];
      end 
      tree_view.append_column[col];
    end
    selection.connect["changed", selection_cb,list(tree_view)]
    tree_view.connect["row_activated",row_activated_cb]
    tree_view.user_data=h;
    tree_view.expand_all[];
  endfunction
    
  hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  if top.equal[[]] then 
    // we want a top level windows 
    flags = ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),
    window = gtkdialog_new(title= title,flags = flags,...
			   buttons = ["gtk-ok","gtk-cancel"]);
    ok_rep = 1; // buttons return code is their indice in buttons matrix
    
    // window.set_border_width[  5]
    // window.connect[  "destroy",gtk_widget_destroyed, &window]
    window.vbox.pack_start[hbox,expand=%f,fill=%f,padding=0]
    vbox = window.vbox;
  else
    vbox = top;
  end
  
  stock = gtkimage_new("stock","gtk-edit"  , GTK.ICON_SIZE_DIALOG);
  hbox.pack_start[stock,expand=%f,fill=%f,padding=0]
  label=gtklabel_new(str=catenate(title));
  hbox.pack_start[label,expand=%t,fill=%t,padding=0]
  
  // create tree view 
  treeview = create_tree_view(L); 
  treeview.set_rules_hint[  %t]
  treeview.get_selection[].set_mode[GTK.SELECTION_SINGLE];
  // show column headers 
  treeview.set_headers_visible[headers];
    
  if with_scroll then 
    // insert the matrix edition in a scrolled window 
    sw = gtkscrolledwindow_new();
    sw.set_shadow_type[ GTK.SHADOW_ETCHED_IN]
    sw.set_policy[ GTK.POLICY_AUTOMATIC,  GTK.POLICY_AUTOMATIC]
    sw.add[treeview]
    vbox.pack_start[ sw,expand=%t,fill=%t,padding=0];
    if isempty(size_request) then
      size_request=[400,400];
    end
    treeview.set_size_request[min(300,size_request(1)),size_request(2)]
  else
    vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    vbox.pack_start[treeview,expand=%f,fill=%f,padding=0];
  end
  
  function figure_redraw (button, data)
    xbasr(data(1));
  endfunction
  
  if type(L,'short')== 'figure' then 
    button = gtkbutton_new(label="Redraw");
    button.connect[ "clicked", figure_redraw , list(L.id)] 
    vbox.pack_start[button,expand=%f,fill=%f,padding=0];
  end

  if top.equal[[]] then 
    window.show_all[];
    // treeview.columns_autosize[];
    // a modal window undestroyed at end of run. 
    response = window.run[];
    if response == ok_rep; // GTK.RESPONSE_OK 
      // to get the new value of list/hash 
      L= treeview.user_data;
    end
    window.destroy[];
  end
endfunction 


