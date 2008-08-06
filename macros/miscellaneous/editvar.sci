// Tree View/Editable Cells

function editvar(x,varargopt) 
  if ~exists(x,'callers') then return;end 
  M=acquire(x); 
  if ~varargopt.iskey['title'] then 
    varargopt.title=x+ " of type "+type(M,'string')+".";
  end
  M1=edit_object(M,varargopt(:));
  if M1.equal[[M]] then return;end 
  resume(x=M1);
endfunction
  
function x=edit_object_m(x,varargopt)
  x=edit_matrix(x,varargopt(:));
endfunction

function x=edit_object_b(x,varargopt)
  x=edit_matrix(x,varargopt(:));
endfunction

function x=edit_object_s(x,varargopt)
  x=edit_matrix(x,varargopt(:));
endfunction

function x=edit_object_l(x,varargopt)
  x=edit_object_list_or_hash(x,varargopt(:));
endfunction

function x=edit_object_h(x,varargopt)
  x=edit_object_list_or_hash(x,varargopt(:));
endfunction

function x=edit_object(x,varargopt)
  x=x;
endfunction

// matrices 
//---------

function x=edit_matrix(x,with_scroll=%f,title="Edit matrix",size_request=[],headers=%t,top=[]) 
  
  function on_treeview_button_press_event(treeview, event, args)
    //printf("Button pressed \n");
    //[p,col]=treeview.get_path_at_pos[event.x,event.y];
  endfunction
  
  function entry_toggle_evaluate_str (checkbutton,args)
  
    args(1).set_data[evaluate_str= checkbutton.get_active[]];
  endfunction

  function cell_edited_bool(cell, path_string, data)
  // we enter this function after cell edition for boolean
    model=data(1);
    column_number = cell.get_data["column"];
    // get toggled iter */
    iter=model.get_iter[path_string];
    val = model.get_value[iter,column_number];
    // do something with the value 
    val = ~val;
    // set new value 
    model.set[iter,column_number, val];
  endfunction
  
  function cell_edited (cell,path_string,new_text,data)
  // we enter this function after cell edition for 
  // strings or numbers 
    printf("cell edited\n");
    tree_view = data(1);
    model = tree_view.get_model[];
    col = cell.get_data["column"];
    path = gtktreepath_new(path_string);
    // i = path.get_indices[];
    iter = model.get_iter[path_string];
    if data(2) == 's' then 
      evaluate_str = tree_view.get_data['evaluate_str'];
      if evaluate_str then 
	ok=execstr('val='+new_text',errcatch=%t);
	if ok then 
	  if type(val,'string') == 'SMat' && size(val,'*')==1 then 
	    model.set[iter,col,val];
	  else
	    x_message("Given expression does not evaluate to a string !");
	  end
	else
	  x_message("Given expression does not evaluate to a string !");
	  // x_message(lasterror());
	end
      else
	model.set[iter,col,new_text];
      end
    elseif data(2) == 'm' then 
      ok=execstr('val='+new_text',errcatch=%t);
      if ok then 
	if type(val,'string') == 'Mat' && size(val,'*')==1 then 
	  model.set[iter,col,val];
	else
	  x_message("Given expression does not evaluate to a scalar d"+...
		    "ouble !");
	end
      else
	x_message("Given expression does not evaluate to a scalar d"+...
		  "ouble !");
	// x_message(lasterror());
      end
    end
  endfunction
  
  function res=get_matrix_from_gtkliststore(model,type_x)
  // back create a matrix from the model 
  // this function can be replaced by the get_matrix[] 
  // method added to gtk tree and list store 
    ncol= model.get_n_columns[] ;
    iter=model.get_iter_first[];
    flag=%t;
    select type_x 
     case 'm' then res_c = ones(1,ncol);res=[];
     case 's' then res_c = string(ones(1,ncol));res=m2s([]);
     case 'b' then res_c = ones(1,ncol) >= 0;res=m2b([]);
    end
    while flag
      for i=1:ncol,
	res_c(i)=model.get_value[iter,i-1];
      end
      res=[res;res_c];
      flag = model.iter_next[iter];
    end
  endfunction
  
  function add_columns (treeview,ncol,type_x)
    // used to enter each element in a cell 
    model = treeview.get_model[];
    col_editable = ncol;
    cols="C"+string(1:ncol);
    
    for col= 0:(ncol-1)
      renderer = gtkcellrenderertext_new ();
      renderer.connect[  "edited",  cell_edited,list(treeview,type_x)]
      renderer.set_data[column=col ];
      // direct use of property 
      renderer.set_property['editable',%t];
      //properties of the renderer are transmited through hash table.
      //Note that we can replace text with markup to use the Pango markup 
      //language 
      attrs= hash_create(markup= col);// editable= col_editable);
      tc = gtktreeviewcolumn_new(title=cols[col+1],renderer=renderer,attrs=attrs);
      // can we resize the columns with mouse 
      tc.set_resizable[%t];
      // tc.get_resizable[];
      // can we sort columns 
      tc.set_reorderable[%t];
      tc.set_sort_column_id[col];
      // column can expand (added 2007)
      tc.set_expand[%t]
      //
      treeview.append_column[tc];
    end 
  endfunction
  
  function add_columns_bool (treeview,ncol,type_x)
    // used to enter each element in a cell 
    model = treeview.get_model[];
    col_editable = ncol;
    cols="C"+string(1:ncol);
    for col= 0:(ncol-1)
      renderer = gtkcellrenderertoggle_new ();
      renderer.connect[  "toggled",  cell_edited_bool,list(model,type_x)]
      renderer.set_data[column=col ];
      attrs= hash_create(active= col);
      tc = gtktreeviewcolumn_new(title=cols[col+1],renderer=renderer,attrs=attrs);
      // can we resize the columns with mouse 
      tc.set_resizable[%t];
      // tc.get_resizable[];
      // can we sort columns 
      tc.set_reorderable[%t];
      tc.set_sort_column_id[col];
      // column can expand (added 2007)
      tc.set_expand[%t]
      treeview.append_column[tc];
    end 
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
  
  stock = gtkimage_new("stock","gtk-edit" , GTK.ICON_SIZE_DIALOG);
  hbox.pack_start[stock,expand=%f,fill=%f,padding=0]
  label=gtklabel_new(str=catenate(title));
  hbox.pack_start[label,expand=%t,fill=%t,padding=0]
        
  // last colmun could be used to store an edit flag for each cell.
  edit_flag = ones(size(x,1),1) >= 0
  ncol_x = size(x,2);
  ls = list( x) ;//  edit_flag);
  model = gtkliststore_new(ls) 
  
  // create tree view 
  treeview = gtktreeview_new(model);
  // treeview.connect["button-press-event", on_treeview_button_press_event];
  treeview.set_rules_hint[  %t]
  treeview.get_selection[].set_mode[GTK.SELECTION_SINGLE];
  // show column headers 
  treeview.set_headers_visible[headers];
  type_x = type(x,'short');
  if type_x == 'b' then 
    // for booleans we use check boxes 
    add_columns_bool(treeview,ncol_x,type_x);
  else 
    add_columns(treeview,ncol_x,type_x);
  end

  if type_x == 'b' then 
    if ncol_x >= 60 || size(x,1) >= 60 then with_scroll = %t; end 
  else 
    if ncol_x >= 20 || size(x,1) >= 60 then with_scroll = %t; end 
  end
  
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
    if type_x == 'b' then 
      sw.set_size_request[min(30*size(x,2),size_request(1)),min(30*(size(x,1)+1),size_request(2))]
    else 
      sw.set_size_request[min(60*size(x,2),size_request(1)),min(30*(size(x,1)+1),size_request(2))]
    end
  else
    vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    vbox.pack_start[treeview,expand=%f,fill=%f,padding=0];
  end
  
  
  if type_x == 's' then 
    vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    evaluate_str_check = gtkcheckbutton_new(label="Evaluate entries");
    vbox.pack_start[ evaluate_str_check,expand=%f,fill=%t,padding=0]
    evaluate_str_check.connect[  "toggled",entry_toggle_evaluate_str, list(treeview)]
    val = %f;
    evaluate_str_check.set_active[val];
    treeview.set_data[evluate_str=val];
  end 
  
  if top.equal[[]] then 
    window.show_all[];
    // treeview.columns_autosize[];
    // a modal window undestroyed at end of run. 
    response = window.run[];
    if response == ok_rep; // GTK.RESPONSE_OK 
      //to get the new value of matrix 
      //we can use the above function get_matrix_from_gtkliststore
      //x=get_matrix_from_gtkliststore(model,type_x);
      //or directly use the method get_matrix which 
      //extract a matrix from a model (for which all the values are 
      //of the same type).
      x=model.get_matrix[];
    end
    window.destroy[];
  end
endfunction 


// list and hash tables 
//---------------------

function L=edit_object_list_or_hash(L,with_scroll=%t,title="Edit List",size_request=[],headers=%t,top=[])

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
      if etype == 'h' then 
	// we search in a hash table 
	Il($+1)=name;
      else 
	// we search in a list 
	Il($+1)=I(p)+1;
      end
      // update type.
      etype=model.get_value[iter1,1];
    end
  endfunction
  
  function tree_model_append(model,h,iter) 
  // A recursive function which walks through the given 
  // list or hash table and inserts all the elements in the 
  // tree model. When an element is itself a list or hash table 
  // a recursive call is invoked.
  type_ = type(h,'short');
  select type_ 
   case 'h' then 
    h_names =sort(h.__keys,'g','i')
   case 'l' 
    n=length(h);
    if n<>0 then h_names ='('+string(1:n)+')';else h_names=[];end
  end
  for i=1:size(h_names,'*');
    name = h_names(i);
    select type_ 
     case 'h' then objname = h(h_names(i));
     case 'l' then objname = h(i);
    end
    t = type(objname,'short');
    value="*"
    if size(objname,'*')== 1 then 
      select t 
       case 'm' then value=m2s(objname);
       case 'b' then value=m2s(b2m(objname));
       case 's' then value=objname ;
      end
    end
    if t== 'h' || t == 'l' then 
      value="";
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
      tree_model_append(model,objname,iter1);
    end
  end
  endfunction
  
  function message(t,mess) 
    dialog = gtkmessagedialog_new (flags= GTK.DIALOG_MODAL,
    type= t,
    buttons= GTK.BUTTONS_OK,
    message = mess );
    dialog.run[];
    dialog.destroy[];
  endfunction 

  function row_activated_cb (tree_view,path,data)
    // this callback is activated when a row is activated. 
    // i.e double click. 
    model = tree_view.get_model[];
    iter = model.get_iter[path];
    // Noter que l'on obtient le chemin recursif avec 
    // path.get_indices[] 
    // qui permet d'acceder directo a la valeur par 
    // Il faut changer path.get_indices[]+1 en liste !!
    // tree_view.user_data(path.get_indices[]+1);
    // j'ai rajout�  un get_list_indices[] qui donne des 
    // indices demarrant a 1 (car on peut pas faire L+1).
    // 
    // path.to_string[] donne une version string 
    name=model.get_value[iter,0]; // column 0 is the name 
    //printf("row %s activated\n",name);
    Il = get_nsp_list_path_from_tree_path(tree_view,path);
    M=tree_view.user_data(Il);
    // XXX here we need a generic edit 
    if type(M,'short')=='l' then return;end 
    if type(M,'short')=='h' then return;end 
    rep =edit_matrix(M);
    if ~rep.equal[[M]] then 
      tree_view.user_data(Il)=M;
      value = '*';
      if size(rep,'*')== 1 then 
	select type(rep,'short')  
	 case 'm' then value=m2s(rep);
	 case 'b' then value=m2s(b2m(rep));
	 case 's' then value=rep;
	end
	model.set[iter,3,value];
      end
    end
  endfunction 
  
  function selection_cb(selection,args)
    // this callback is activated when a row is selected 
    model=args(1);
    iter=selection.get_selected[];
    if type(iter,'short')== 'none' then return;end 
    fname= model.get_value[iter,0];
    //printf("row %s selected\n",fname);
  endfunction 

  function cell_edited (cell,path_string,new_text,data)
    // we enter this function after cell edition for 
    // strings or numbers 
      if new_text=="" || new_text=="*" then return;end 
      tree_view = data(1);
      model = tree_view.get_model[];
      path = gtktreepath_new(path_string);
      iter = model.get_iter[path_string];
      ctype = model.get_value[iter,1];
      if ctype == 'l' || ctype == 'h' then return;end 
      // evaluate newtext 
      ok=execstr('val='+new_text',errcatch=%t);
      if ~ok then 
	// just push the text and change the type to string 
	model.set[iter,3,new_text];
	model.set[iter,2,"1x1"];
	model.set[iter,1,"s"];
      else
	ctype = type(val,'short');
	if ctype== 's' then 
	  model.set[iter,1,ctype];
	  model.set[iter,2,sprintf("%dx%d",size(val,1),size(val,2))];
	  if size(val,'*')==1 then 
	    model.set[iter,3,val];
	  else
	    model.set[iter,3,"*"];
	  end
	  // we also need to change the string in the user_data 
	  Il =get_nsp_list_path_from_tree_path(tree_view,path);
	  tree_view.user_data(Il) = val;
	elseif ctype == 'm' then 
	  model.set[iter,1,ctype];
	  model.set[iter,2,sprintf("%dx%d",size(val,1),size(val,2))];
	  if size(val,'*')==1 then 
	    model.set[iter,3,m2s(val)];
	  else
	    model.set[iter,3,"*"];
	  end
	  Il =get_nsp_list_path_from_tree_path(tree_view,path);
	  tree_view.user_data(Il) = val;
	elseif ctype == 'b' then 
	  model.set[iter,1,ctype];
	  model.set[iter,2,sprintf("%dx%d",size(val,1),size(val,2))];
	  if size(val,'*')==1 then 
	    model.set[iter,3,m2s(b2m(val))];
	  else
	    model.set[iter,3,"*"];
	  end
	  Il =get_nsp_list_path_from_tree_path(tree_view,path);
	  tree_view.user_data(Il) = val;
	else
	  // ignore 
	  x_message("result of evaluation is not managed");
	end
      end
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
    selection.connect["changed", selection_cb,list(model)]
    tree_view.connect["row_activated",row_activated_cb,list(model)]
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


// nsp cells 
//---------- 
//XXXX: Attention si la fonction on_treeview_button_press_event
// renvoit %t �a fait planter nsp 
// Il faut corriger 

function x=edit_cells(x,with_scroll=%f,title="Edit matrix",size_request=[],headers=%t,top=[]) 
  
  function on_treeview_button_press_event(treeview, event, args)
    if event.button == 3 then 
      printf("Button pressed \n");
      [path,col]=treeview.get_path_at_pos[event.x,event.y];
      colid= col.get_data['id'];
      row= path.get_indices[];
      printf("we must edit (%d,%d)\n",row+1,colid);
      val = edit_object(treeview.user_data{row+1,colid});      
    end
  endfunction
  
  function entry_toggle_evaluate_str (checkbutton,args)
    args(1).set_data[evaluate_str= checkbutton.get_active[]];
  endfunction
  
  function cell_edited (cell,path_string,new_text,data)
  // we enter this function after cell edition for 
  // strings or numbers 
    printf("cell edited\n");
    tree_view = data(1);
    model = tree_view.get_model[];
    col = cell.get_data["column"];
    path = gtktreepath_new(path_string);
    // i = path.get_indices[];
    iter = model.get_iter[path_string];
    ok=execstr('val='+new_text',errcatch=%t);
    if ok then 
      row= path.get_indices[];
      printf("we must set (%d,%d)\n",row+1,col+1);
      tree_view.user_data{row+1,col+1}= val;
    else
      x_message("Given expression does not evaluate to a nsp object !");
    end
  endfunction
  
  function add_columns (treeview,ncol,type_x)
    // used to enter each element in a cell 
    model = treeview.get_model[];
    col_editable = ncol;
    cols="C"+string(1:ncol);
    
    for col= 0:(ncol-1)
      renderer = gtkcellrenderertext_new ();
      renderer.connect[  "edited",  cell_edited,list(treeview,type_x)]
      renderer.set_data[column=col ];
      // direct use of property 
      renderer.set_property['editable',%t];
      //properties of the renderer are transmited through hash table.
      //Note that we can replace text with markup to use the Pango markup 
      //language 
      attrs= hash_create(markup= col);// editable= col_editable);
      tc = gtktreeviewcolumn_new(title=cols[col+1],renderer=renderer,attrs=attrs);
      // can we resize the columns with mouse 
      tc.set_resizable[%t];
      // tc.get_resizable[];
      // can we sort columns 
      tc.set_reorderable[%t];
      tc.set_sort_column_id[col];
      // column can expand (added 2007)
      tc.set_expand[%t]
      tc.set_data[id=col+1];
      //
      treeview.append_column[tc];
    end 
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
  
  stock = gtkimage_new("stock","gtk-edit" , GTK.ICON_SIZE_DIALOG);
  hbox.pack_start[stock,expand=%f,fill=%f,padding=0]
  label=gtklabel_new(str=catenate(title));
  hbox.pack_start[label,expand=%t,fill=%t,padding=0]
        
  // last colmun could be used to store an edit flag for each cell.
  edit_flag = ones(size(x,1),1) >= 0
  ncol_x = size(x,2);
  xs = cellstostr(x);
  model = gtkliststore_new(list(xs)) 
  
  // create tree view 
  treeview = gtktreeview_new(model);
  treeview.user_data = x;
  treeview.connect["button-press-event", on_treeview_button_press_event];
  treeview.set_rules_hint[  %t]
  treeview.get_selection[].set_mode[GTK.SELECTION_SINGLE];
  // show column headers 
  treeview.set_headers_visible[headers];
  type_x = type(x,'short');
  add_columns(treeview,ncol_x,type_x);
  if ncol_x >= 20 || size(x,1) >= 60 then with_scroll = %t; end 
  
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
    sw.set_size_request[min(60*size(x,2),size_request(1)),min(30*(size(x,1)+1),size_request(2))]
  else
    vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    vbox.pack_start[treeview,expand=%f,fill=%f,padding=0];
  end
    
  if type_x == 's' then 
    vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    evaluate_str_check = gtkcheckbutton_new(label="Evaluate entries");
    vbox.pack_start[ evaluate_str_check,expand=%f,fill=%t,padding=0]
    evaluate_str_check.connect[  "toggled",entry_toggle_evaluate_str, list(treeview)]
    val = %f;
    evaluate_str_check.set_active[val];
    treeview.set_data[evluate_str=val];
  end 
  
  if top.equal[[]] then 
    window.show_all[];
    // treeview.columns_autosize[];
    // a modal window undestroyed at end of run. 
    response = window.run[];
    if response == ok_rep; // GTK.RESPONSE_OK 
      x= treeview.user_data ;
    end
    window.destroy[];
  end
endfunction 
