
function demo_filter()
// Quelques points a finir 
// -- Il faut une demo de remplissage avec set[..]
//    dans le cas d'un treestore 
// -- Il faut changer 
//    gtk_tree_model_filter_convert_child_iter_to_iter ()
//    gtk_tree_model_filter_convert_iter_to_child_iter ()
//    gtk_tree_model_filter_convert_child_path_to_path ()
//    gtk_tree_model_filter_convert_path_to_child_path ()
// -- Il faut que get_list[] 
//    marche quand une colonne contient des objets gtk 
// -- Il faut que set[] puisse marcher avec une colonne et une liste ? 
// filter a model 
  C1 = [%t;%f;%t];
  C23 = [1,4;2,4;3,5]; // two columns 
  C4 = ["first row";"second row";"third row"];
  C5 = list(10,20,30);
  list_store=list(C1,C23,C4,C5);

  model = gtkliststore_new(list_store);
  
  // to get back the model as a list description. 
  L1=model.get_list[]; 
  
  iter=model.get_iter_first[0];
  res=m2s([]);
  while %t then 
    a=  model.get_value[iter,0];
    b=  model.get_value[iter,1];
    res = [res;a,b];
    if ~model.iter_next[iter] then break;end
  end

  // filter the model 
  
  fmodel = model.filter_new[];
  // get the underlying model i.e model 
  pmodel= fmodel.get_model[];

  fmodel.set_visible_column[0];
  fmodel.refilter[];
  
  res=m2s([]);
  while %t then 
    a=  fmodel.get_value[iter,0];
    b=  fmodel.get_value[iter,1];
    res = [res;a,b];
    if ~fmodel.iter_next[iter] then break;end
  end
  
  iter=fmodel.get_iter_first[0];
  citer=model.get_iter_first[0];
  fmodel.convert_iter_to_child_iter[citer,iter];
  fmodel.convert_child_iter_to_iter[iter,citer];

  // setting values uses the set_value 
  // *   set[iter,col,value] 
  // *   set[iter,list(...)] 

  // Note that we can fill more than one row with set
  
  iter=fmodel.get_iter_first[0];
  // here we set four rows starting at the begining 
  // thus we erase previous values and add one row 
  model.set[iter,list([%t;%t;%f;%f],[6;7;88;90],[8;9;90;90],...
		      ["foo";"goo";"moo";"roo"],list(10,30,89,789))]
  
  // an empty model 
  model = gtkliststore_new(list(%t,1,3,"",list(4)),%f);
  // fill the model with set 
  model.set[list([%t;%t;%f;%f],[6;7;88;90],[8;9;90;90],...
                 ["foo";"goo";"moo";"roo"],list(10,30,89,789))];
  
  model.get_list[];
    
endfunction


	
