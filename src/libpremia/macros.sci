
function Lc=premia_var_to_xchoices(L)
// convert a premia var list to a format suitable for x_choices 
  Lc=list();
  for i=1:length(L)
    if type(L(i)(2),'short')=='l' then 
      Lc(i)=list('button',L(i)(1),0,premia_var_to_xchoices(L(i)(2)));
    else 
      if L(i)(3)== %t then tag='entry'; else tag = 'ignore'; end
      str=sprint(L(i)(2),as_read=%t);
      // first line is a header 
      str=str(2);      
      Lc(i)=list(tag,L(i)(1),0,str);
    end
  end
endfunction

function L=x_choices_to_premia(Lc)
// back-convert an x_choice format to premia var description.
//  
  L=list()
  for i=1:length(Lc)
    select Lc(i)(1)
     case 'entry' then  L(i)=list(Lc(i)(2),evstr(Lc(i)(4)),%t);
     case 'ignore' then L(i)=list(Lc(i)(2),evstr(Lc(i)(4)),%f);
     case 'button' then L(i)=list(Lc(i)(2), ...
				  x_choices_to_premia(Lc(i)(4)));
    end
  end
endfunction

function ok=premia_check_basic(L,Lold)
// check that values in L matches types in Lold 
// we assume here that L and Lold have the same shapes 
  ok=%t;
  for i=1:length(L)
    if type(L(i)(2),'short')=='l' then 
      ok = premia_check_basic(L(i)(2),Lold(i)(2));
      if ok== %f then 
	return;
      end 
    else 
      if L(i)(3)== %t then 
	// we have to check here that values matches 
	if type(L(i)(2),'short') <> 'm' then 
	  x_message(L(i)(1)+' should be of Matrix type');
	  ok=%f;
	  return;
	end
	if length(L(i)(2))<>length(Lold(i)(2)) then 
	  x_message(L(i)(1)+' should be of length '+string(length(Lold(i)(2))));
	  ok=%f;
	  return;
	end
      end
    end
  end
endfunction;
  
function premia_model_values(M) 
// interactively set model parameters 
  B=M.get_model_values[];
  Bx=premia_var_to_xchoices(B);
  while %t then 
    [Lres,Bx]=x_choices('Choose model '+M.get_model[]+' parameters',Bx,%t);
    Bn=x_choices_to_premia(Bx);
    if premia_check_basic(Bn,B) then 
      // Now we have to check if model accept parameters 
      M.set_model_values[Bn];
      S=M.model_check[];
      if isempty(S) then 
	break;
      else
	M.set_model_values[B];
	x_message(S(1)+': '+S(2));
      end
    end
  end
endfunction


function [family,option]=select_premia_option(pmodel,family=0,option=0)

  flags = ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),
  window = gtkdialog_new(title= "Option dialog" ,flags = flags,...
                         buttons = ["gtk-ok","gtk-cancel"]);
  ok_rep = 1; // buttons return code is their indice in buttons matrix
  
  //window = gtkwindow_new ();
  //window.set_title[  "ComboBox"]
  
  //window.set_default_size[  -1, 500]
  //window.connect[  "destroy", gtk_widget_destroyed, &window]
  window.set_border_width[  8]

  hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  window.vbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  tmp = gtkframe_new (label="Choose an option");
  hbox.pack_start[ tmp,expand=%f,fill=%f,padding=0]
  boom = gtkvbox_new (homogeneous=%f,spacing=0);
  boom.set_border_width[5];
  tmp.add[boom];

  [ts_model,opts,family_index] = create_premia_option_tree(pmodel)
  combobox = gtkcombobox_new(model=ts_model);
  combobox.set_add_tearoffs[%t];
  //combobox.connect["changed", current_option ];
  //g_object_unref (ts_model);
  boom.add[combobox];
  
  cell_renderer = gtkcellrenderertext_new ();
  combobox.pack_start[ cell_renderer,expand= %t];
  combobox.add_attribute[cell_renderer,"text",0];
  // set active element in the combobox 
  // combobox.set_active[3];
  // set the default value to (0,0) 

  // a treepath is build in get_iter from an nsp vector.
  iter=ts_model.get_iter[[family,option]]
  combobox.set_active_iter[iter];
  
  // the sensitive property will be set in the function 
  // we dont want the family to be selectable 

  function y=macro_set_option_sensitive(cell_view,cell,ts_model,iter,data)
  // only option names can be sensitive 
    sensitive = ts_model.iter_has_child[iter];
    cell.set_property['sensitive',~sensitive]
    y=%t;
  endfunction
  
  combobox.set_cell_data_func[cell_renderer,macro_set_option_sensitive];
  // 
  window.show_all[];

  function [family,option_name]=current_option(combo)
  // this can be used as handler for 
  // combobox.connect["changed", current_option ]
    M= combo.get_model[]
    iter=combo.get_active_iter[]
    option_name = M.get_value[iter,0];
    family = combo.get_active[];
    // printf('Option selected %s %d\n",option_name,family+1);
  endfunction
  
  // a modal window undestroyed at end of run. 
  while %t then 
    response = window.run[];
    if response == ok_rep; // GTK.RESPONSE_OK 
      [family,option_name]=current_option(combobox);
      family=family+1;
      // take care to get back correct family index;
      option=find(opts(family)== option_name) ;
      if ~isempty(option) then 
	// need the family index in the whole families table
	family=family_index(family);
	break;
      end 
    end
  end
  window.destroy[];
endfunction


function [ts_model,options,family_index]= create_premia_option_tree(pmodel)
// select options which are compatible with given
// pmodel. Options are grouped by families. 
  n=1;
  while ~isempty(premia_get_family(n)) then n=n+1;end 
  // n-1 gives the total number of families 
  // extract the sub list for pmodel 
  options = list();
  Top = [];
  for i=1:n-1 
    S=premia_get_family(i,pmodel);
    if ~isempty(S) then 
      options($+1)=S;
      Top = [Top;i];
    end 
  end
  family_index=Top;
  Top = 'Family '+ string(Top);
  ts_model = gtktreestore_new(Top); 
  // fill the ts_model at next level 
  it = ts_model.get_iter_first[] 
  i=1;
  while %t 
    ts_model.insert[it,0,list(options(i))];
    i = i+1;
    if ts_model.iter_next[it] == %f 
      break;
    end
  end
endfunction

function premia_option_values(M) 
// interactively set model parameters 
  B=M.get_option_values[];
  Bx=premia_var_to_xchoices(B);
  while %t then 
    [Lres,Bx]=x_choices('Choose model '+M.get_option[]+' parameters',Bx,%t);
    Bn=x_choices_to_premia(Bx);
    if premia_check_basic(Bn,B) then 
      // Now we have to check if option  accept parameters 
      M.set_option_values[Bn];
      S=M.option_check[];
      if isempty(S) then 
	break;
      else
	M.set_option_values[B];
	x_message(S(1)+': '+S(2));
      end
    end
  end
endfunction

function premia_method_values(M) 
// interactively set model parameters 
  B=M.get_method_values[];
  Bx=premia_var_to_xchoices(B);
  while %t then 
    [Lres,Bx]=x_choices('Choose model '+M.get_method[]+' parameters',Bx,%t);
    Bn=x_choices_to_premia(Bx);
    if premia_check_basic(Bn,B) then 
      // Now we have to check if method  accept parameters 
      M.set_method_values[Bn];
      S=M.method_check[];
      if isempty(S) then 
	break;
      else
	M.set_method_values[B];
	x_message(S(1)+': '+S(2));
      end
    end
  end
endfunction

function premia_method_results(M) 
// show result
  B=M.get_method_results[];
  Bx=premia_var_to_xchoices(B);
  [Lres,Bx]=x_choices(M.get_method[]+' Results',Bx,%t);
endfunction

