// A simple demo of GMenuModel 

function demo_gmenumodel_simple()

  function g_menu_names()
  // utility 
    G_MENU_ATTRIBUTE_ACTION ="action";
    G_MENU_ATTRIBUTE_ACTION_NAMESPACE ="action-namespace";
    G_MENU_ATTRIBUTE_TARGET ="target";
    G_MENU_ATTRIBUTE_LABEL ="label";
    G_MENU_ATTRIBUTE_ICON ="icon";
    G_MENU_LINK_SUBMENU ="submenu";
    G_MENU_LINK_SECTION ="section";
  endfunction

  function g_variant_names()
  // utility 
    G_VARIANT_TYPE_BOOLEAN              ="b";
    G_VARIANT_TYPE_BYTE                 ="y";
    G_VARIANT_TYPE_INT16                ="n";
    G_VARIANT_TYPE_UINT16               ="q";
    G_VARIANT_TYPE_INT32                ="i";
    G_VARIANT_TYPE_UINT32               ="u";
    G_VARIANT_TYPE_INT64                ="x";
    G_VARIANT_TYPE_UINT64               ="t";
    G_VARIANT_TYPE_DOUBLE               ="d";
    G_VARIANT_TYPE_STRING               ="s";
    G_VARIANT_TYPE_OBJECT_PATH          ="o";
    G_VARIANT_TYPE_SIGNATURE            ="g";
    G_VARIANT_TYPE_VARIANT              ="v";
    G_VARIANT_TYPE_HANDLE               ="h";
    G_VARIANT_TYPE_UNIT                 ="()";
    G_VARIANT_TYPE_ANY                  ="*";
    G_VARIANT_TYPE_BASIC                ="?";
    G_VARIANT_TYPE_MAYBE                ="m*";
    G_VARIANT_TYPE_ARRAY                ="a*";
    G_VARIANT_TYPE_TUPLE                ="r";
    G_VARIANT_TYPE_DICT_ENTRY           ="{?*}";
    G_VARIANT_TYPE_DICTIONARY           ="a{?*}";
    G_VARIANT_TYPE_STRING_ARRAY         ="as";
    G_VARIANT_TYPE_OBJECT_PATH_ARRAY    ="ao";
    G_VARIANT_TYPE_BYTESTRING           ="ay";
    G_VARIANT_TYPE_BYTESTRING_ARRAY     ="aay";
    G_VARIANT_TYPE_VARDICT              ="a{sv}";
  endfunction 
  
  function action_group= create_action_group ()
  // builds a g_simple_action_group
    
    function activate_action (action, parameter, user_data)
      printf ("Action %s activated\n", action.get_name[]);
    endfunction

    function activate_toggle (action, parameter, user_data)
      old_state = action.get_state[];
      new_state = g_variant_new_boolean (~ old_state.get_boolean[]);
      printf ("Toggle action %s activated, state changes from %d to %d\n",...
	      action.get_name[],old_state.get_boolean[],new_state.get_boolean[]);
      action.set_state[new_state];
    endfunction
    
    function activate_radio (action, parameter, data)
      old_state =  action.get_state[];
      new_state = g_variant_new_string ( parameter.get_string[]);
      printf ("Radio action %s activated, state changes from %s to %s\n",...
	      action.get_name[],old_state.get_string[],new_state.get_string[]);
      action.set_state[new_state];
    endfunction
    
    actions_cell = { "undo",  activate_action, "" , "" ;
		     "redo",  activate_action, "" , "" ;
		     "cut",   activate_action, "" , "" ;
		     "copy",  activate_action, "" , "" ;
		     "paste", activate_action, "" , "" ;
		     "bold",  activate_toggle, "", "true";
		     "lang",  activate_radio,  "s", "''latin''"};

    actions=m2s([]);
    for i=1:size(actions_cell,1)
      actions(i,1:3)=[actions_cell{i,1},actions_cell{i,3},actions_cell{i,4}];
    end

    action_group = g_simple_action_group_new ();
    action_group.add_action_entries[actions];

    for i=1:size(actions_cell,1)
      action= action_group.lookup_action[actions_cell{i,1}];
      action.connect['activate',actions_cell{i,2}];
    end
    
    if %f then
      // test a set of functions
      action.activate[parameter=g_variant_new_string('foo')];
      action_group.list_actions[]
      action_group.has_action["undo"];
      action_group.get_action_enabled["undo"];
      // next gives error when no
      action_group.get_action_parameter_type["undo"];
      action_group.get_action_state_hint["undo"];
      action_group.get_action_state["undo"];
      action_group.get_action_state_type["undo"];
      // define actions
      action = g_simple_action_new("foo",parameter_type=g_variant_type_new("s"));
      state=g_variant_new_string('pipo');
      parameter_type = g_variant_type_new("s");
      action = g_simple_action_new_stateful("foo",parameter_type= parameter_type,state=state);
      typ= action.get_state_type[];
    end
  endfunction
  
  function button_clicked (button,data)
    holder = data(1);
    holder.menu.show_all[];
    holder.menu.popup[activate_time=0];
  endfunction
  
  function y= on_delete_event (widget, event, data)
    widget.destroy[];
    y=%t;
  endfunction

  window = gtk_window_new(type=GTK.WINDOW_TOPLEVEL);
  window.connect["delete-event",on_delete_event];
  box = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=6);
  window.add[box];
  
  // creae an action group 
  action_group = create_action_group ();
  
  // obtain a menu_model
  builder = gtk_builder_new_from_file (getenv("NSP")+"/demos3/gtk3/libbase/demo_gmenumodel_simple.ui");
  menu_model = builder.get_object["edit-menu"];

  // create a menu from model and store data in a hash table 
  // The action group is stored in the menu with name "win" 
  // and actions in the ui file are prefixed with win
  
  holder = hash(10);
  holder.model = menu_model;
  holder.group = action_group;
  holder.menu = gtk_menu_new_from_model(menu_model);
  holder.menu.insert_action_group["win",action_group]
  
  button = gtk_button_new(label = "Click here");
  button.connect["clicked",button_clicked, list(holder)];
  box.add[button];
  window.show_all[];

endfunction

