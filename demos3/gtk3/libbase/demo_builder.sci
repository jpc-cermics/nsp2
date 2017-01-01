// Builder
//
// Demonstrates an interface loaded from a XML description.

function window= demo_builder(do_widget)
//builder = gtk_builder_new_from_resource ("/builder/demo.ui");
// XXX: if a keyword is not recognized in the ui file 
// gtk_builder_new_from_file may crash as in the given 
// example where primary is not recognized (commented out in 
// demo.ui. A gtk error is raised and function crashed in g_log.
  
  function quit_activate (action,parameter, user_data)
    window = user_data(1);
    window.destroy[];
  endfunction
  
  function about_activate (action,parameter, user_data)
    window = user_data(1);
    builder = window.get_data[ "builder"];
    about_dlg = builder.get_object[ "aboutdialog1"];
    about_dlg.run[];
    about_dlg.hide[];
  endfunction

  function help_activate (action, parameter, user_data)
    printf ("Help not available\n");
  endfunction
  
  ui_demo = getenv("NSP")+"/demos3/gtk3/libbase/demo_builder.ui";
  builder = gtk_builder_new_from_file(ui_demo);
  // builder.connect_signals[table,extra_args]
  window = builder.get_object [ "window1"];
  window.set_screen [do_widget.get_screen []];
  // window.connect [ "destroy", gtk_widget_destroyed, &window];
  toolbar = builder.get_object [ "toolbar1"];
  //gtk_style_context_add_class (toolbar.get_style_context [], "primary-toolbar");

  action_group = g_simple_action_group_new ();
  // add_action_entries in two steps 
  actions_cell = { "quit", quit_activate, "","" ;
		   "about", about_activate, "","";
		   "help", help_activate, "",""};
  // first the names 
  actions=m2s([]);
  for i=1:size(actions_cell,1)
    actions(i,1:3)=[actions_cell{i,1},actions_cell{i,3},actions_cell{i,4}];
  end
  action_group.add_action_entries[actions];
  // now the callbacks 
  for i=1:size(actions_cell,1)
    action= action_group.lookup_action[actions_cell{i,1}];
    action.connect['activate',actions_cell{i,2},list(window)];
  end
  
  window.insert_action_group[ "win", action_group];
    
  accel_group = gtk_accel_group_new ();
  window.add_accel_group[accel_group];

  objects=["new","open","save","quit","copy","cut","paste","help","about"];

  // keys can be found in src/gtk3/codegen-3.0/keysyms.sce 
  // the last ones are F1 and F7 
  keys=[ascii('nosqcxv'),0xFFBE,0xFFC4];
  
  for i=1:size(objects,'*')
    item = builder.get_object [ sprintf('%s_item',objects(i))];
    item.add_accelerator["activate", accel_group, keys(i), ...
		    GDK.CONTROL_MASK, GTK.ACCEL_VISIBLE];
  end
        
  window.set_data[builder= builder];
  window.show_all[];
endfunction

