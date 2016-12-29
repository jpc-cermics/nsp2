// Pour faire des menus il faut utiliser des g_menu_model
// que l'on peut obtenir avec un gtkbuilder
// En voici un example
// Some constants

function g_menu_names()
  G_MENU_ATTRIBUTE_ACTION ="action";
  G_MENU_ATTRIBUTE_ACTION_NAMESPACE ="action-namespace";
  G_MENU_ATTRIBUTE_TARGET ="target";
  G_MENU_ATTRIBUTE_LABEL ="label";
  G_MENU_ATTRIBUTE_ICON ="icon";
  G_MENU_LINK_SUBMENU ="submenu";
  G_MENU_LINK_SECTION ="section";
endfunction

function g_variant_names()
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

// testgmenu.c
// Copyright (C) 2011  Red Hat, Inc.
// Written by Matthias Clasen
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
// License along with this library. If not, see <http://www.gnu.org/licenses/>.
//
// menu_holder is a hash table with model,group,menu and items_changed
// Menumodel callbacks

function connect_to_items_changed (model, callback, data)
  if ~model.check_data["handler_connected"] then
    model.connect["items-changed", callback, list(data)];
    model.set_data[handler_connected= %t];
  end
  n=model.get_n_items[]
  for i = 0:n-1
    iter = model.iterate_item_links[i];
    while iter.next[]
      m = iter.get_value[];
      connect_to_items_changed(m, callback, data);
    end
  end
endfunction

function items_changed (model, position, removed, added, data)
  holder = data(1);
  printf("Received GMenuModel::items-changed\n");
  holder.items_changed = %t;
  connect_to_items_changed (model, items_changed, holder);
endfunction

// action group callbacks
// ----------------------------

function enabled_changed (group,action_name, enabled,data)
  // called when an action has a enabled changed
  widget=data(1);
  widget.set_sensitive[enabled];
endfunction

function toggle_state_changed (group,action_name,state,data)
  // called when a toggle action state changed
  w=data(1)
  a = w.get_data["action"];
  w.handler_block[ a.activate_handler];
  w.set_active[state.get_boolean[]];
  w.handler_unblock[ a.activate_handler];
endfunction

function radio_state_changed (group,action_name,state,data)
// called when a radio action state changed
  w=data(1);
  a = w.get_data["action"];
  w.handler_block[ a.activate_handler];
  b = a.target == state.get_string[];
  w.set_active[b];
  w.handler_unblock[ a.activate_handler];
endfunction

// menuitems callbacks
//------------------------

function item_activated (widget,data)
  a = widget.get_data["action"];
  printf("item was activated associated action is %s and target=""%s""\n",a.name,a.target);
  if a.target=="" then
    a.group.activate_action[a.name];
  else
    parameter = g_variant_new_string (a.target);
    a.group.activate_action[a.name, parameter=parameter];
  end
endfunction

function w=create_menuitem_from_model (model, item,group)
  exec(g_variant_names)
  exec(g_menu_names)
  [tag_label,label]= model.get_item_attribute[item,G_MENU_ATTRIBUTE_LABEL, "s"];
  [tag_action,action]=model.get_item_attribute[item,G_MENU_ATTRIBUTE_ACTION,"s"];
  if tag_action then
    ok= execstr('stype = group.get_action_state_type[action];',errcatch=%t);
    if ok then
      if stype.dup_string[] == G_VARIANT_TYPE_BOOLEAN then
	w = gtk_check_menu_item_new(label=label);
      elseif  stype.dup_string[] == G_VARIANT_TYPE_STRING then
	w = gtk_check_menu_item_new(label=label);
	w.set_draw_as_radio[%t];
      end
    else
      stype="void";
      w = gtk_menu_item_new(mnemonic=label);
    end
  else
    w = gtk_menu_item_new(mnemonic=label);
  end

  if tag_action then
    a= hash(name=action, group=group,target="");
    if ~group.get_action_enabled[action] then
      w.set_sensitive[%f];
    end
    s = "action-enabled-changed::"+action;
    a.enabled_changed_id = group.connect[s, enabled_changed, list(w)];
    a.activate_handler = w.connect["activate", item_activated];
    w.set_data[action= a];
    if stype.equal["void"] then
      // /* all set */
    elseif stype.dup_string[] == G_VARIANT_TYPE_BOOLEAN then
      s = "action-state-changed::" + action;
      a.state_changed_id = group.connect[s,toggle_state_changed, list(w)];
      w.set_data[action= a];
      v = group.get_action_state[action];
      w.set_active[v.get_boolean[]];
    elseif stype.dup_string[] == G_VARIANT_TYPE_STRING then
      s = "action-state-changed::"+ action;
      a.state_changed_id = group.connect[s,radio_state_changed, list(w)];
      [tag_target, target] = model.get_item_attribute[ item, G_MENU_ATTRIBUTE_TARGET, "s"];
      a.target = target;
      w.set_data[action= a];
      v = group.get_action_state[ action];
      w.set_active[ v.get_string[] == target];
    else
      // g_assert_not_reached ();
    end
  end
endfunction

function append_items_from_model (menu,model,group,need_separator, ...
				  heading)
  exec(g_menu_names)
  n = model.get_n_items[];
  if need_separator && n > 0 then
    w = gtk_separator_menu_item_new ();
    w.show[];
    menu.append[w];
    need_separator = %f;
  end

  if ~isempty(heading) then
    // w = gtk_menu_item_new_with_label (heading);
    w = gtk_menu_item_new(heading);
    w.show[];
    w.set_sensitive[%f];
    menu.append[w];
  end
  for i = 0:n-1
    m = model.get_item_link[i, G_MENU_LINK_SECTION];
    if ~type(m,'short').equal['none'] then
      [tag,label]= model.get_item_attribute[ i, G_MENU_ATTRIBUTE_LABEL, "s"];
      append_items_from_model (menu, m, group, need_separator, label);
      if need_separator then
	w = gtk_separator_menu_item_new ();
	w.show[];
	menu.append[w];
	need_separator = %f;
	continue;
      end
    end
    menuitem = create_menuitem_from_model (model, i, group);
    m = model.get_item_link[ i, G_MENU_LINK_SUBMENU];
    if ~type(m,'short').equal['none'] then
      submenu = create_menu_from_model (m, group);
      menuitem.set_submenu[submenu];
    end
    menuitem.show[];
    menu.append[menuitem];
    need_separator = %t;
  end
endfunction

function w=create_menu_from_model (model, group)
  w = gtk_menu_new ();
  need_separator = %f;
  append_items_from_model (w, model, group, need_separator, "");
endfunction

function holder=menu_holder_new (model,group)
  holder = hash(10);
  holder.model = model;
  holder.group = group;
  holder.menu = create_menu_from_model (model, group);
  holder.items_changed = %f;
  connect_to_items_changed (model, items_changed, holder);
endfunction

function menu= menu_holder_get_menu ( holder)
  if %t || holder.items_changed then
    holder.items_changed = %f;
    holder.menu.destroy[];
    holder.menu = create_menu_from_model (holder.model, holder.group);
  end
  menu=holder.menu;
  menu.show_all[];
endfunction

function menu_model=get_model ()
// obtain a menu_model
  builder = gtk_builder_new_from_file (getenv("NSP")+"/demos3/gtk3/libbase/demo_gmenumodel.ui");
  menu_model = builder.get_object["edit-menu"];
endfunction

// group of simple action and their activate handlers

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

function action_group= get_group ()
// build a g_simple_action_group
// demos of g_simple_action methods and objects

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
    // action.activate[]; // XXXX a test
  end
  // test for the last one
  // action.activate[parameter=g_variant_new_string('foo')];

  if %f then
    // test a set of functions
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

// The action treeview

function enabled_cell_func (column,cell,model,iter, data)
  group = data(1);
  name = model.get_value[iter, 0];
  enabled = group.get_action_enabled[name];
  cell.set_active[enabled];
endfunction

function state_cell_func (column,cell,model,iter,data)
  exec(g_variant_names)
  group = data(1);
  name = model.get_value[iter, 0];
  ok=execstr('state = group.get_action_state[name]',errcatch=%t);
  cell.set_visible[%f];
  // g_object_set (cell, "mode", GTK_CELL_RENDERER_MODE_INERT, NULL);
  cell.set_property["mode", GTK.CELL_RENDERER_MODE_INERT];
  if ~ok then return;end
  if state.get_type_string[] == G_VARIANT_TYPE_BOOLEAN &&
    type(cell,'short') == "GtkCellRendererToggle" then
    cell.set_visible[%t];
    // g_object_set (cell, "mode", GTK_CELL_RENDERER_MODE_ACTIVATABLE, NULL);
    cell.set_property[ "mode",GTK.CELL_RENDERER_MODE_ACTIVATABLE];
    cell.set_active[ state.get_boolean[]];
  elseif state.get_type_string[] == G_VARIANT_TYPE_STRING &&
    type(cell,'short') =="GtkCellRendererCombo" then
    cell.set_visible[%t];
    // g_object_set (cell, "mode", GTK_CELL_RENDERER_MODE_EDITABLE, NULL);
    // g_object_set (cell, "text", g_variant_get_string (state, NULL), NULL);
    cell.set_property["mode",  GTK.CELL_RENDERER_MODE_EDITABLE];
    cell.set_property["text", state.get_string[]];
  end
endfunction

function enabled_cell_toggled (cell,path_str,data)
  model=data(1);
  group = model.get_data["group"],
  path = gtk_tree_path_new(path_str);
  iter= model.get_iter[path];
  name = model.get_value[iter, 0];// Take care get is replace by get_value
  enabled = group.get_action_enabled[name];
  action = group.lookup_action[name];
  action.set_enabled[~enabled];
  model.row_changed[path,iter];
endfunction

function state_cell_toggled (cell,path_str,data)
  exec(g_variant_names)
  model = data(1);
  group = model.get_data["group"],
  path = gtk_tree_path_new(path_str);
  iter= model.get_iter[path];
  name = model.get_value[iter, 0];
  ok = execstr('state = group.get_action_state[name]',errcatch=%t);
  action = group.lookup_action[name];
  if ok && state.get_type_string[] == G_VARIANT_TYPE_BOOLEAN then
    b = state.get_boolean[];
    action.set_state[g_variant_new_boolean(~b)];
  end
  model.row_changed[path,iter];
endfunction

function state_cell_edited (cell, path_str, new_text, data)
  model=data(1);
  group = model.get_data["group"];
  path = gtk_tree_path_new (path_str);
  iter= model.get_iter[path];
  name=model.get_value[iter, 0];
  action = group.lookup_action[name];
  action.set_state[g_variant_new_string (new_text)];
  model.row_changed[ path, iter];
endfunction

function tv=create_action_treeview (group)
  store = gtk_list_store_new( list("",""),%f);
  actions = group.list_actions[];
  for i=1:size(actions,'*')
    iter=store.append[];
    store.set[iter,0,actions[i]];
  end
  store.set_data[group= group];
  tv = gtk_tree_view_new ();

  function action_tv_queue_draw(w,name,flag,data)
    tv = data(1);
    tv.queue_draw[];
  endfunction

  group.connect["action-enabled-changed", action_tv_queue_draw, list(tv)];
  group.connect["action-state-changed", action_tv_queue_draw, list(tv)];
  tv.set_model[model=store];

  cell = gtk_cell_renderer_text_new ();
  // column = gtk_tree_view_column_new_with_attributes ("Action", cell, "text", 0);
  // emulated as follows:
  column = gtk_tree_view_column_new ();
  column.set_title [ "Action"];
  column.pack_start [ cell, %f];
  column.add_attribute[cell,"text",0]
  tv.append_column[column];

  column = gtk_tree_view_column_new ();
  column.set_title [ "Enabled"];
  cell = gtk_cell_renderer_toggle_new ();
  column.pack_start [ cell, %f];
  column.set_cell_data_func [ cell, enabled_cell_func, list(group)];
  cell.connect[ "toggled", enabled_cell_toggled, list(store)];
  tv.append_column[column];

  column = gtk_tree_view_column_new ();
  column.set_title [ "State"];
  cell = gtk_cell_renderer_toggle_new ();
  column.pack_start [ cell, %f];
  column.set_cell_data_func [ cell, state_cell_func, list(group)];
  cell.connect["toggled", state_cell_toggled, list(store)];
  cell = gtk_cell_renderer_combo_new ();

  values = gtk_list_store_new (list(""),%f);
  iter=values.append[];
  values.set [ iter, 0, "latin"];
  iter=values.append[];
  values.set [ iter, 0, "greek"];
  iter=values.append[];
  values.set [ iter, 0, "urdu"];
  iter=values.append[];
  values.set [ iter, 0, "sumerian"];
  // g_object_set(cell, "has-entry", %f, "model", values, "text-column",0, "editable", %t);
  cell.set_property["has-entry", %f];
  cell.set_property["model",values];
  cell.set_property["text-column",0];
  cell.set_property["editable",%t];

  column.pack_start[cell,%f];
  column.set_cell_data_func[cell, state_cell_func, list(group)];
  cell.connect["edited",state_cell_edited, list(store)];
  tv.append_column[column];
endfunction

//  Dynamic menu changes

function toggle_sumerian (button, data)
//
  exec(g_menu_names)
  model = button.get_data[ "model"];
  adding = button.get_active[];
  m = model.get_item_link[model.get_n_items[] - 1, G_MENU_LINK_SECTION];
  m = m.get_item_link[ m.get_n_items[]- 1, G_MENU_LINK_SUBMENU];
  if adding then
    m.append["Sumerian", "lang::sumerian"];
  else
    m.remove[m.get_n_items[] - 1];
  end
endfunction

function action_list_add (store, action)
  iter=store.append[];
  store.set[iter,0,action];
endfunction

function action_list_remove (store, action)
  iter = store.get_iter_first[];
  while %t
    text=store.get_value[iter,0];
    if text == action then
      store.remove[iter];
      break;
    end
    if ~ store.iter_next[iter]  then break;end
  end
endfunction

function toggle_italic (button, data)
  exec(g_menu_names)
  tv = data(1);
  model = button.get_data["model"];
  group = button.get_data["group"];
  store = tv.get_model[]; // an action list.
  adding = button.get_active[];
  m = model.get_item_link[model.get_n_items[]-1,G_MENU_LINK_SECTION];
  if  adding then
    action = g_simple_action_new_stateful ("italic",state= g_variant_new_boolean (%f));
    group.add_action[action];
    action.connect["activate", activate_toggle];
    action_list_add( store,"italic")
    m.insert[ 1, "Italic", "italic"];
  else
    group.remove_action["italic"];
    action_list_remove(store,"italic");
    m.remove[1];
  end
endfunction

function toggle_speed (button, data)
  exec(g_menu_names)
  tv = data(1);
  model = button.get_data["model"];
  group = button.get_data["group"];
  store = tv.get_model[]; // an action list.
  adding = button.get_active[];

  m = model.get_item_link[1, G_MENU_LINK_SECTION];

  if adding then
    action = g_simple_action_new ("faster");
    group.add_action[action];
    action.connect["activate", activate_action];

    action = g_simple_action_new ("slower");
    group.add_action[action];
    action.connect["activate", activate_action];

    action_list_add(store,"faster");
    action_list_add(store,"slower");

    submenu = g_menu_new ();
    submenu.append[ "Faster", "faster"];
    submenu.append[ "Slower", "slower"];
    m.append_submenu["Speed",submenu];
  else
    group.remove_action["faster"];
    group.remove_action["slower"];
    action_list_remove(store, "faster");
    action_list_remove(store, "slower");
    m.remove[ m.get_n_items[] - 1];
  end
endfunction

function box= create_add_remove_buttons (group, model,treeview)
  box = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=6);

  // button = gtk_check_button_new_with_label ("Add Italic");
  button = gtk_check_button_new(label = "Add Italic");
  box.add[button];
  button.set_data[ group= group];
  button.set_data[ model= model];
  button.connect["toggled",toggle_italic, list(treeview)];

  button = gtk_check_button_new(label = "Add Sumerian");
  box.add[button];
  button.set_data[ group= group];
  button.set_data[ model= model];
  button.connect["toggled",toggle_sumerian, list(treeview)];

  button = gtk_check_button_new(label ="Add Speed");
  box.add[button];
  button.set_data[ group= group];
  button.set_data[ model= model];
  button.connect["toggled",toggle_speed,list(treeview)];
endfunction

function button_clicked (button,data)
  holder = data(1);
  menu= menu_holder_get_menu(holder);
  menu.popup[activate_time=0];
endfunction

function y= on_delete_event (widget, event, data)
  widget.destroy[];
  y=%t;
endfunction

function demo_gmenumodel()
//  GtkWidget *window;
//  GtkWidget *box;
//  GtkWidget *button;
//  GtkWidget *tv;
//  GtkWidget *buttons;
//  MenuHolder *holder;
//  GMenuModel *model;
//  GActionGroup *group;
//  GDBusConnection *bus;
//  GError *error = NULL;
  do_export = %f
  do_import = %f
  BUS_NAME= "org.gtk.TestMenus"
  OBJ_PATH= "/org/gtk/TestMenus"
  // GOptionEntry entries[] = {
  //   { "export", 0, 0, G_OPTION_ARG_NONE, &do_export, "Export actions and menus over D-Bus", NULL },
  //   { "import", 0, 0, G_OPTION_ARG_NONE, &do_import, "Use exported actions and menus", NULL },
  //   { NULL, }
  // };
  // gtk_init_with_args (&argc, &argv, NULL, entries, NULL, NULL);

  if do_export && do_import then
     error("can''t have it both ways\n");
     return;
  end

  window = gtk_window_new(type=GTK.WINDOW_TOPLEVEL);
  //window.connect["delete-event",on_delete_event];
  box = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=6);
  window.add[box];

  // bus = g_bus_get_sync (G_BUS_TYPE_SESSION, NULL, NULL);

  if do_import then
    //g_print ("Getting menus from the bus...\n");
    //model = (GMenuModel*)g_dbus_menu_model_get (bus, BUS_NAME, OBJ_PATH);
    //g_print ("Getting actions from the bus...\n");
    //group = (GActionGroup*)g_dbus_action_group_get (bus, BUS_NAME, OBJ_PATH);
  else
    group = get_group ();
    model = get_model ();
    tv = create_action_treeview (group);
    box.add[tv];
    buttons = create_add_remove_buttons (group, model, tv);
    box.add[buttons];
  end

  if do_export then
    // g_print ("Exporting menus on the bus...\n");
    //   if (!g_dbus_connection_export_menu_model (bus, OBJ_PATH, model, &error))
    //     {
    //       g_warning ("Menu export failed: %s", error.message);
    //       exit (1);
    //     }
    //   g_print ("Exporting actions on the bus...\n");
    //   if (!g_dbus_connection_export_action_group (bus, OBJ_PATH, group, &error))
    //     {
    //       g_warning ("Action export failed: %s", error.message);
    //       exit (1);
    //     }
    //   g_bus_own_name_on_connection (bus, BUS_NAME, 0, NULL, NULL, NULL, NULL);
    // }
  else
    holder = menu_holder_new (model, group);
    button = gtk_button_new(label = "Click here");
    button.connect["clicked",button_clicked, list(holder)];
    box.add[button];
  end
  window.show_all[];
endfunction
