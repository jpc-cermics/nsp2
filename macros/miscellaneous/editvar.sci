// Tree View/Editable Cells
// To be done:
//  1/ the parent in the dialogs should be set when recursively editing
//  2/ the buttons in list edition must be changed.
//  3/ right-click to edit instead of double click.

function editvar(x,varargopt)
  if nargin == 0 then
    A=who('caller');
    if size(A,'*') == 0 then return;end
    if size(A,'*') > 1  then
      nx=x_choose(A,'choose a variable to edit in the local frame');
      if nx == 0 then return;end
      x= A(nx);
    else
      x= A;
    end
  else
    if ~exists(x,'callers') then return;end
  end
  M=acquire(x);
  if ~varargopt.iskey['title'] then
    varargopt.title=x+ " of type "+type(M,'string')+".";
  end
  M1=edit_object(M,varargopt(:));
  if M1.equal[[M]] then return;end
  execstr('resume('+x+'=M1);');
endfunction

function x=edit_object_m(x,varargopt)
  x=edit_matrix(x,varargopt(:));
endfunction

function x=edit_object_mp(x,varargopt)
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

function x=edit_object_ce(x,varargopt)
  x=edit_cells(x,varargopt(:));
endfunction

function x=edit_object(x,varargopt)
  if is(x, %types.Graphic) then
    // for all the objects which inherits from graphic.
    x=edit_grobject(x,varargopt(:));
  else
    x=x;
  end
endfunction

// matrices
//---------

function x=edit_matrix(x,with_scroll=%f,title="Edit matrix",size_request=[],headers=%t,top=[],parent=[])

  function on_treeview_button_press_event(treeview, event, args)
    //printf("Button pressed \n");
    //[p,col]=treeview.get_path_at_pos[event.x,event.y];
  endfunction

  function entry_toggle_evaluate_str (checkbutton,args)
    // handler for the evaluate string button.
    args(1).set_data[evaluate_str= checkbutton.get_active[]];
  endfunction

  function cell_edited_bool(cell, path_string, data)
  // we enter this function after cell edition for boolean
    tree_view = data(1);
    model = tree_view.get_model[];
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
    elseif data(2) == 'm' || data(2) == 'mp' then
      ok=execstr('val='+new_text',errcatch=%t);
      if ok then
	if type(val,'string') == 'Mat' && size(val,'*')==1 then
	  model.set[iter,col,val];
	else
	  x_message("Given expression does not evaluate to a scalar double !");
	end
      else
	x_message("Given expression does not evaluate to a scalar double !");
	// x_message(lasterror());
      end
    end
  endfunction

  function res=get_matrix_from_gtkliststore(model,type_x)
  // back create a matrix from the model.
  // this function is not used and is replaced by the get_matrix[]
  // method added to gtk tree and list store
  // just kept here as an example.
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
    // col_editable = ncol;
    // enter the model in the treeview
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
      renderer.connect[  "toggled",  cell_edited_bool,list(treeview,type_x)]
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

  function entry_return_handler(w,ev,data)
  // the handler for the entry text
  // when return is entered we evaluate expression.
    if ev.keyval == 0xFF0D then
      treeview=data(1);
      model = treeview.get_model[];
      // printf("Return pressed in entry: %s\n",w.get_text[]);
      ok=execstr('x_new='+w.get_text[],errcatch=%t);
      if ok then
	if is(model,%types.None) then
	  window = data(2);
	  window.set_data[x_new=x_new];
	  window.response[3]; //
	  return;
	end
	x=model.get_matrix[];
	if and(size(x)==size(x_new)) && type(x,'short')== type(x_new,'short') then
	  // change the model
	  // printf("OK  in new_model: %s\n",w.get_text[]);
	  model = gtkliststore_new(list(x_new));
	  treeview.set_model[model=model];
	else
	  // we will return ans start a new editvar
	  //x_message(sprintf("expression should evaluate to a matrix \nof type %s and size"+...
	  // " %dx%d !",type(x,'string'),size(x,1),size(x,2)));
	  // window is a gtk_dialog we force a quit from dialog run
	  window = data(2);
	  window.set_data[x_new=x_new];
	  window.response[3]; //
	end
      else
	x_message("Given expression does not evaluate to a nsp object !");
      end
    end
  endfunction

  // now the code for edit matrix.
  //   if size(x,'*') == 0 then
  //     x_message("Matrix is of null size.")
  //     return;
  //   end

  size_request1 =   size_request;
  if exists('gtk_get_major_version','function') then
    hbox = gtk_box_new(GTK.ORIENTATION_HORIZONTAL,spacing=8);
  else
    hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  end

  if top.equal[[]] then
    // we want a top level windows
    flags = ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),
    window = gtkdialog_new(title= title,flags = flags,...
			   buttons = ["gtk-ok","gtk-cancel"]);
    ok_rep = 1; // buttons return code is their indice in buttons matrix

    // window.set_border_width[  5]
    // window.connect[  "destroy",gtk_widget_destroyed, &window]
    if exists('gtk_get_major_version','function') then
      vbox = window.get_content_area[];
    else
      vbox = window.vbox;
    end
    vbox.pack_start[hbox,expand=%f,fill=%f,padding=0]

  else
    vbox = top;
  end
  if exists('gtk_get_major_version','function') then
    stock = gtkimage_new('icon_name','insert-text', GTK.ICON_SIZE_DIALOG);
  else
    stock = gtkimage_new("stock","gtk-edit" , GTK.ICON_SIZE_DIALOG);
  end
  hbox.pack_start[stock,expand=%f,fill=%f,padding=0]
  label=gtklabel_new(str=catenate(title));
  hbox.pack_start[label,expand=%t,fill=%t,padding=0]

  if size(x,'*') <> 0 then
    ncol_x = size(x,2);
    model = gtkliststore_new(list(x))
    // create tree view
    treeview = gtktreeview_new(model);
    // treeview.connect["button-press-event", on_treeview_button_press_event];
    if ~exists('gtk_get_major_version','function') then
      treeview.set_rules_hint[  %t]
    end
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
      if exists('gtk_get_major_version','function') then
	vbox.pack_start[gtk_separator_new(GTK.ORIENTATION_HORIZONTAL),expand=%f,fill=%t];
      else
	vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
      end
      vbox.pack_start[treeview,expand=%f,fill=%f,padding=0];
    end
  else
    treeview= gtktreeview_new();
    label = gtklabel_new (str="[]");
    vbox.pack_start[ label, expand=%f,fill= %t,padding=0];
  end

  if  type(x,'short') == 's' then
    if exists('gtk_get_major_version','function') then
      vbox.pack_start[gtk_separator_new(GTK.ORIENTATION_HORIZONTAL),expand=%f,fill=%t];
    else
      vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    end
    evaluate_str_check = gtkcheckbutton_new(label="Evaluate entries when edited");
    vbox.pack_start[ evaluate_str_check,expand=%f,fill=%t,padding=0]
    evaluate_str_check.connect[  "toggled",entry_toggle_evaluate_str, list(treeview)]
    val = %f;
    evaluate_str_check.set_active[val];
    treeview.set_data[evaluate_str=val];
  end
  if exists('gtk_get_major_version','function') then
    hbox = gtk_box_new(GTK.ORIENTATION_HORIZONTAL,spacing=8);
  else
    hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  end
  label = gtklabel_new (str="new expression:");
  // label.set_alignment[ 0, 0.5];
  hbox.pack_start[ label, expand=%f,fill= %t,padding=0];

  entry = gtkentry_new ();
  select  type(x,'short')
   case 'm' then   str= sprintf("rand(%d,%d);",size(x,1),size(x,2));
   case 'mp' then  str= sprintf("m2mp(rand(%d,%d));",size(x,1),size(x,2));
   case 's' then   str= sprintf("string(rand(%d,%d));",size(x,1),size(x, 2));
   case 'b' then   str= sprintf("rand(%d,%d)>=0.5;",size(x,1),size(x,2));
  else
     str= sprintf("rand(%d,%d);",size(x,1),size(x,2));
  end
  entry.set_text[str]
  // entry.select_region[ 0, 5];
  entry.connect["key_press_event",entry_return_handler,list(treeview,window)];
  hbox.pack_start[ entry,expand=%t,fill=%t,padding=0]
  vbox.pack_start[ hbox,expand=%f,fill=%t,padding=10]

  if top.equal[[]] then
    window.show_all[];
    // treeview.columns_autosize[];
    // a modal window undestroyed at end of run.
    response = window.run[];
    if response == ok_rep // GTK.RESPONSE_OK
      //to get the new value of matrix
      //we can use the above function get_matrix_from_gtkliststore
      //x=get_matrix_from_gtkliststore(model,type_x);
      //or directly use the method get_matrix which
      //extract a matrix from a model (for which all the values are
      //of the same type).
      // take care that model has maybe changed during interaction
      model = treeview.get_model[];
      if is(model,%types.None) then
	x=x;
      else
	x=model.get_matrix[];
      end
    elseif response == 3 ; // response called by me in gtk_entry
      x=window.get_data['x_new'];
      window.destroy[];
      editvar('x', with_scroll=with_scroll,size_request=size_request1,...
		 headers=headers,top=top,parent=parent);
      return;
    end
    window.destroy[];
  end
endfunction


// list and hash tables
//---------------------

function L=edit_object_list_or_hash(L,with_scroll=%t,title="Edit List",size_request=[],headers=%t,top=[],parent=[])

  function selected_remove (button, data)
    // remove selected line
    treeview=data(1);
    selection = treeview.get_selection[];
    [iter,model] = selection.get_selected[];
    if type(iter,'short') <> 'GtkTreeIter' then return;end
    path=model.get_path[iter];
    L=get_nsp_list_path_from_tree_path(treeview,path)
    model.remove[iter];
    // we must remove the element described by L
    tag=L.last[];
    L.remove_last[];
    treeview.user_data(L).remove[tag];
  endfunction

  function insert_after_selected (button, data)
    // insert after selected line
    treeview=data(1);
    selection = treeview.get_selection[];
    [iter,model] = selection.get_selected[];
    if type(iter,'short') <> 'GtkTreeIter' then return;end
    insert_at_or_after_iter(treeview,model,iter);
  endfunction

  function insert_at_or_after_iter(treeview,model,iter,flag=%t)
    // utility
    path=model.get_path[iter];
    L=get_nsp_list_path_from_tree_path(treeview,path)
    L1=L;L1.remove_last[];
    stype = type(treeview.user_data(L1),'short');
    if stype == 'h' then
      str=get_key_name ();
      if str=="" then return;end
    else
      str="--";
    end
    // model.insert_after[parent=iter or ignored, sibling =iter or
    // ignored, lis(....) to also set the value
    model.insert_after[[],iter,list(str,'m',"1x1","0")];
    if stype == 'h' then
      L($)=str
      treeview.user_data(L)=0;
    else
      if flag then
	tag = L($)+1;
      else
	tag = L($);
	end
      treeview.user_data(L1).add[0,tag];
    end
    update_model([],list(treeview))
    //selection.select_path[path];
  endfunction

  function insert_at_end (button, data)
    // insert at the end
    treeview=data(1);
    model = treeview.get_model[];
    iter = model.get_iter_first[];
    if type(iter,'short') <> 'GtkTreeIter' then
      // model is empty
      iter=model.append[]
      insert_at_or_after_iter(treeview,model,iter,flag=%f);
    else
      // while model.iter_next[iter] ; end
      iter=model.append[]
      insert_at_or_after_iter(treeview,model,iter,flag=%f);
    end
  endfunction

  function str=get_key_name ()
    str="";
    flags = ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),
    window = gtkdialog_new(title= "Enter hash key",flags = flags,...
			   buttons = ["gtk-ok","gtk-cancel"]);
    ok_rep = 1; // buttons return code is their indice in buttons matrix

    if exists('gtk_get_major_version','function') then
      box1 = window.get_content_area[];
    else
      box1 = window.vbox;
    end
    entry = gtkentry_new ();
    entry.set_text["name"]
    entry.select_region[ 0, 5];
    box1.pack_start[ entry,expand=%t,fill=%t,padding=0]
    window.show_all[];
    response = window.run[];
    if response == ok_rep then
      // GTK.RESPONSE_OK
      // print the entry ?
      str = entry.get_text[];
    end
    window.destroy[];
  endfunction

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
    value = cellstostr({objname});
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

  function y=on_treeview_button_press_event(tree_view, event, args)
    // right click on tree view
    // action similar to double-click
    if event.button == 3 then
      // printf("Button pressed \n");
      ok=execstr('[path,col]=treeview.get_path_at_pos[event.x,event.y];',errcatch=%t);
      if ~ok then
	popup_menu=create_menu(list(tree_view,[]),args(1),%f);
	popup_menu.popup[button=3,activate_time=0];
	y=%t;
      else
	sel=tree_view.get_selection[];
	sel.select_path[path];
	popup_menu=create_menu(list(tree_view,path),args(1),%t);
	popup_menu.popup[button=3,activate_time=0];
	y=%t;
      end
    else
      y=%f
    end
  endfunction

  function menuitem_response(w,args)
    // printf("Menu item [%d] activated \n",args(1));
    tree_view = args(2);
    path= args(3);
    select args(1)
     case 0 then edit_at_path(tree_view,path);
     case 1 then insert_after_selected ([], list(tree_view));
     case 2 then insert_at_end ([], list(tree_view));
     case 3 then selected_remove ([],list(tree_view));
    end
  endfunction

  function menu=create_menu(data,dtype,flag)
    menu = gtkmenu_new ();
    if flag then
      menuitem = gtkimagemenuitem_new(stock_id="gtk-edit");
      data1=data;
      data1(0)=0;
      menuitem.connect["activate",menuitem_response,data1];
      menu.append[menuitem]
      menuitem.show[];
      if dtype == 'h' then
	items=["Insert new","Remove"];
	val= [2:3];
      else
	items=["Insert after","Insert at end","Remove"];
	val= [1:3];
      end
      for i=1:size(val,'*');
	menuitem = gtkmenuitem_new(items(i));
	data1=data;
	data1(0)=val(i);
	menuitem.connect["activate",menuitem_response,data1];
	menu.append[menuitem]
	menuitem.show[];
      end
    else
      menuitem = gtkmenuitem_new("New");
      data1=data;
      data1(0)=2;
      menuitem.connect["activate",menuitem_response,data1];
      menu.append[menuitem]
      menuitem.show[];
    end
  endfunction

  function row_activated_cb (tree_view,path,data)
    // this callback is activated when a row is activated.
    // i.e double click.
      edit_at_path(tree_view,path)
  endfunction

  function edit_at_path(tree_view,path)
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
    //printf("row %s activated\n",model.get_value[iter,0]; );
    Il = get_nsp_list_path_from_tree_path(tree_view,path);
    M=tree_view.user_data(Il);
    // here we need a generic edit
    M1=edit_object(M,parent=tree_view);
    if ~M1.equal[M] then
      tree_view.user_data(Il)=M1;
      xs = cellstostr({M1});
      octype = type(M,'short');
      ctype = type(M1,'short');
      model.set[iter,1,ctype];
      model.set[iter,3,xs];
      model.set[iter,2,sprintf("%dx%d',size(M1,1),size(M1,2))];
      if octype == 'l' || octype == 'h' || ctype == 'l' || ctype == 'h' then
	//  we need to update the treeview
	update_model([],list(tree_view))
      end
    end
  endfunction

  function selection_cb(selection,args)
    // this callback is activated when a row is selected
    tree_view=args(1);
    model =tree_view.get_model[];
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
  if exists('gtk_get_major_version','function') then
    hbox = gtk_box_new(GTK.ORIENTATION_HORIZONTAL,spacing=8);
  else
    hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  end
  if top.equal[[]] then
    // we want a top level windows
    flags = ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),
    window = gtkdialog_new(title= title,flags = flags,...
			   buttons = ["gtk-ok","gtk-cancel"]);
    ok_rep = 1; // buttons return code is their indice in buttons matrix

    // window.set_border_width[  5]
    // window.connect[  "destroy",gtk_widget_destroyed, &window]
    if exists('gtk_get_major_version','function') then
      vbox = window.get_content_area[];
    else
      vbox = window.vbox;
    end

      vbox.pack_start[hbox,expand=%f,fill=%f,padding=0]
  else
    vbox = top;
  end
  if exists('gtk_get_major_version','function') then
    stock = gtkimage_new('icon_name','insert-text', GTK.ICON_SIZE_DIALOG);
  else
    stock = gtkimage_new("stock","gtk-edit" , GTK.ICON_SIZE_DIALOG);
  end
  hbox.pack_start[stock,expand=%f,fill=%f,padding=0]
  label=gtklabel_new(str=catenate(title));
  hbox.pack_start[label,expand=%t,fill=%t,padding=0]

  // create tree view
  treeview = create_tree_view(L);
  if ~exists('gtk_get_major_version','function') then
    treeview.set_rules_hint[  %t]
  end
  treeview.get_selection[].set_mode[GTK.SELECTION_SINGLE];
  // show column headers
  treeview.set_headers_visible[headers];
  treeview.connect["button-press-event", on_treeview_button_press_event,list(type(L,'short'))];

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
    if exists('gtk_get_major_version','function') then
      vbox.pack_start[gtk_separator_new(GTK.ORIENTATION_HORIZONTAL),expand=%f,fill=%t];
    else
      vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    end
    vbox.pack_start[treeview,expand=%f,fill=%f,padding=0];
  end

  //   button = gtkbutton_new(label="Remove selection");
  //   button.connect[ "clicked", selected_remove,list(treeview)]
  //   vbox.pack_start[button,expand=%f,fill=%f,padding=0];

  //   button = gtkbutton_new(label="Insert after selection");
  //   button.connect[ "clicked",insert_after_selected,list(treeview)]
  //   vbox.pack_start[button,expand=%f,fill=%f,padding=0];

  //   button = gtkbutton_new(label="Insert at end");
  //   button.connect[ "clicked",insert_at_end,list(treeview)]
  //   vbox.pack_start[button,expand=%f,fill=%f,padding=0];

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

function x=edit_cells(x,with_scroll=%f,title="Edit cell",size_request=[],headers=%t,top=[])

  function y=on_treeview_button_press_event(treeview, event, args)
    // right click on tree view
    if event.button == 3 then
      //printf("Button pressed \n");
      [path,col]=treeview.get_path_at_pos[event.x,event.y];
      colid= col.get_data['id'];
      row= path.get_indices[];
      //printf("we must edit (%d,%d)\n",row+1,colid);
      val = edit_object(treeview.user_data{row+1,colid},parent=treeview);
      if ~val.equal[M] then
	treeview.user_data{row+1,colid}=val;
	xs = cellstostr({val});
	model=treeview.get_model[];
	iter=model.get_iter[path];
	model.set[iter,colid-1,xs];
      end
      y=%t;
    else
      y=%f
    end
  endfunction

  function entry_toggle_evaluate_str (checkbutton,args)
    args(1).set_data[evaluate_str= checkbutton.get_active[]];
  endfunction

  function cell_edited (cell,path_string,new_text,data)
  // we enter this function after cell edition for
  // strings or numbers
  // printf("cell edited\n");
    tree_view = data(1);
    model = tree_view.get_model[];
    col = cell.get_data["column"];
    path = gtktreepath_new(path_string);
    // i = path.get_indices[];
    iter = model.get_iter[path_string];
    ok=execstr('val='+new_text',errcatch=%t);
    if ok then
      row= path.get_indices[];
      //printf("we must set (%d,%d)\n",row+1,col+1);
      tree_view.user_data{row+1,col+1}= val;
      xs = cellstostr({val});
      model.set[iter,col, xs];
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
  if exists('gtk_get_major_version','function') then
    hbox = gtk_box_new(GTK.ORIENTATION_HORIZONTAL,spacing=8);
  else
    hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  end
  if top.equal[[]] then
    // we want a top level windows
    flags = ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),
    window = gtkdialog_new(title= title,flags = flags,...
			   buttons = ["gtk-ok","gtk-cancel"]);
    ok_rep = 1; // buttons return code is their indice in buttons matrix

    // window.set_border_width[  5]
    // window.connect[  "destroy",gtk_widget_destroyed, &window]
    if exists('gtk_get_major_version','function') then
      vbox = window.get_content_area[];
    else
      vbox = window.vbox;
    end

    vbox.pack_start[hbox,expand=%f,fill=%f,padding=0]

  else
    vbox = top;
  end

  if size(x,'*') == 0 then
    x_message("cell is of null size.")
    return;
  end

  if exists('gtk_get_major_version','function') then
    stock = gtkimage_new('icon_name','insert-text', GTK.ICON_SIZE_DIALOG);
  else
    stock = gtkimage_new("stock","gtk-edit" , GTK.ICON_SIZE_DIALOG);
  end
  hbox.pack_start[stock,expand=%f,fill=%f,padding=0]
  label=gtklabel_new(str=catenate(title));
  hbox.pack_start[label,expand=%t,fill=%t,padding=0]

  ncol_x = size(x,2);
  xs = cellstostr(x);
  model = gtkliststore_new(list(xs))
  // create tree view
  treeview = gtktreeview_new(model);
  treeview.user_data = x;
  treeview.connect["button-press-event", on_treeview_button_press_event];
  if ~exists('gtk_get_major_version','function') then
    treeview.set_rules_hint[  %t]
  end
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
    if exists('gtk_get_major_version','function') then
      vbox.pack_start[gtk_separator_new(GTK.ORIENTATION_HORIZONTAL),expand=%f,fill=%t];
    else
      vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    end
    vbox.pack_start[treeview,expand=%f,fill=%f,padding=0];
  end

  if type_x == 's' then
    if exists('gtk_get_major_version','function') then
      vbox.pack_start[gtk_separator_new(GTK.ORIENTATION_HORIZONTAL),expand=%f,fill=%t];
    else
      vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    end
    evaluate_str_check = gtkcheckbutton_new(label="Evaluate entries");
    vbox.pack_start[ evaluate_str_check,expand=%f,fill=%t,padding=0]
    evaluate_str_check.connect[  "toggled",entry_toggle_evaluate_str, list(treeview)]
    val = %f;
    evaluate_str_check.set_active[val];
    treeview.set_data[evaluate_str=val];
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

function x=edit_object_astnode(x,with_scroll=%f,title="Edit astnode",size_request=[],headers=%t,top=[],parent=[])
  if exists('gtk_get_major_version','function') then
    hbox = gtk_box_new(GTK.ORIENTATION_HORIZONTAL,spacing=8);
  else
    hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  end
  if top.equal[[]] then
    // we want a top level windows
    flags = ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),
    window = gtkdialog_new(title= title,flags = flags,...
			   buttons = ["gtk-ok","gtk-cancel"]);
    ok_rep = 1; // buttons return code is their indice in buttons matrix

    // window.set_border_width[  5]
    // window.connect[  "destroy",gtk_widget_destroyed, &window]
    if exists('gtk_get_major_version','function') then
      vbox = window.get_content_area[];
    else
      vbox = window.vbox;
    end
    vbox.pack_start[hbox,expand=%f,fill=%f,padding=0]
  else
    vbox = top;
  end
  if exists('gtk_get_major_version','function') then
    stock = gtkimage_new('icon_name','insert-text', GTK.ICON_SIZE_DIALOG);
  else
    stock = gtkimage_new("stock","gtk-edit" , GTK.ICON_SIZE_DIALOG);
  end

  hbox.pack_start[stock,expand=%f,fill=%f,padding=0]
  label=gtklabel_new(str=catenate(title));
  hbox.pack_start[label,expand=%t,fill=%t,padding=0]

  if with_scroll then
    // insert the matrix edition in a scrolled window
    sw = gtkscrolledwindow_new();
    sw.set_shadow_type[ GTK.SHADOW_ETCHED_IN]
    sw.set_policy[ GTK.POLICY_AUTOMATIC,  GTK.POLICY_AUTOMATIC]
    // sw.add[treeview]
    vbox.pack_start[ sw,expand=%t,fill=%t,padding=0];
    if isempty(size_request) then
      size_request=[400,400];
    end
    sw.set_size_request[min(60*size(x,2),size_request(1)),min(30*(size(x,1)+1),size_request(2))]
  else
    if exists('gtk_get_major_version','function') then
      vbox.pack_start[gtk_separator_new(GTK.ORIENTATION_HORIZONTAL),expand=%f,fill=%t];
    else
      vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    end
    // vbox.pack_start[treeview,expand=%f,fill=%f,padding=0];
  end

  if %f && type_x == 's' then
    if exists('gtk_get_major_version','function') then
      vbox.pack_start[gtk_separator_new(GTK.ORIENTATION_HORIZONTAL),expand=%f,fill=%t];
    else
      vbox.pack_start[gtkhseparator_new(),expand=%f,fill=%t];
    end
    evaluate_str_check = gtkcheckbutton_new(label="Evaluate entries");
    vbox.pack_start[ evaluate_str_check,expand=%f,fill=%t,padding=0]
    evaluate_str_check.connect[  "toggled",entry_toggle_evaluate_str, list(treeview)]
    val = %f;
    evaluate_str_check.set_active[val];
    treeview.set_data[evaluate_str=val];
  end

  if top.equal[[]] then
    window.show_all[];
    // treeview.columns_autosize[];
    // a modal window undestroyed at end of run.
    response = window.run[];
    if response == ok_rep; // GTK.RESPONSE_OK
      //treeview.user_data ;
    end
    window.destroy[];
  end
endfunction
