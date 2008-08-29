// testactions.sce 
// translated to nsp by J.Ph Chancelier (2008)
// from testaction.c (Copyright (C) 2003  Matthias Clasen)
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
//

function demoaction_activate_action(action) 
// 
  name = action.get_name[];
  typename=type(action,'string');
  printf("Action %s (type=%s) activated\n", name, typename);
endfunction

function demoaction_toggle_action(action) 
  name = action.get_name[];
  typename=type(action,'string');
  printf("Action %s (type=%s) activated (active=%d)\n", name, typename, ...
	 action.get_active[]);
endfunction

function demoaction_radio_action (action)
  name = action.get_name[];
  typename=type(action,'string');
  printf("Action %s (type=%s) activated (active=%d) (value %d)\n", name, typename, ...
	 action.get_active[],...
	 action.get_current_value[]);
endfunction

function demoaction_recent_action (action) 
  name = action.get_name[];
  typename=type(action,'string');
  //   gchar *uri = gtk_recent_chooser_get_current_uri (GTK_RECENT_CHOOSER (action));
  printf ("Action %s (type=%s) activated (uri=%s)\n",  name, typename, "XX" );
endfunction 

function demoaction_toggle_cnp_actions(action,args)
  name = action.get_name[];
  action_group=args(1)
  sensitive = action.get_active[];
  typename=type(action,'string');
  printf("Action %s (type=%s) activated (active=%d)\n", name, typename,sensitive);
  action = action_group.get_action["cut"];
  action.set_property["sensitive", sensitive];
  action = action_group.get_action["copy"];
  action.set_property["sensitive", sensitive];
  action = action_group.get_action[ "paste"];
  action.set_property["sensitive", sensitive];
  action = action_group.get_action["toggle-cnp"];
  if sensitive then 
    action.set_property["label", "Disable Cut and paste ops"];
  else
    action.set_property["label", "Enable Cut and paste ops"];
  end
endfunction

function demoaction_show_accel_dialog (action)
  printf ("Sorry, accel dialog not available\n");
endfunction 

function demoaction_toolbar_style (action,args)
  merge=args(1);
  if merge.check_data['toolbar']==%f then return;end 
  toolbar = merge.get_data['toolbar'];
  demoaction_radio_action (action);
  style = action.get_current_value[];
  toolbar.set_style[style];
endfunction 

function demoaction_toolbar_size_small (action,args)
  merge=args(1);
  if merge.check_data['toolbar']==%f then return;end 
  toolbar = merge.get_data['toolbar'];
  toolbar.set_icon_size[GTK.ICON_SIZE_SMALL_TOOLBAR];
endfunction 

function demoaction_toolbar_size_large (action,args)
  merge=args(1);
  if merge.check_data['toolbar']==%f then return;end 
  toolbar = merge.get_data['toolbar'];
  toolbar.set_icon_size[GTK.ICON_SIZE_LARGE_TOOLBAR];
endfunction 

function demoaction_build_actions(action_group,merge)
// XXX we should use the facilities 
// given by 
// gtk_action_group_add_toggle_actions
// gtk_action_group_add_actions
// gtk_action_group_add_radio_actions
// creates a set of actions 
  action = gtkaction_new( "Menu1Action","Menu _1" ,"", "" );
  action_group.add_action[action];
  action = gtkaction_new( "Menu2Action","Menu _2" ,"", "" );
  action_group.add_action[action];
  action = gtkaction_new( "Menu3Action", "_Dynamic Menu" ,"", "");
  action_group.add_action[action];
  action = gtkaction_new( "cut","C_ut","Cut the selected text to the clipboard", "gtk-cut");
  action.connect["activate",demoaction_activate_action];
  action_group.add_action_with_accel[action,"<control>X"];
  action = gtkaction_new( "copy","_Copy","Copy the selected text to the clipboard", "gtk-copy");
  action.connect["activate",demoaction_activate_action];
  action_group.add_action_with_accel[action,"<control>C"];
  action = gtkaction_new( "paste", "_Paste","Paste the text from the clipboard", "gtk-paste");
  action.connect["activate",demoaction_activate_action];
  action_group.add_action_with_accel[action,"<control>V"];
  action = gtkaction_new( "quit", "Quit", "Quit the application", "gtk-quit");
  action_group.add_action[action];
  action = gtkaction_new( "customise-accels","Customise _Accels","Customise keyboard shortcuts","");
  action.connect["activate",demoaction_show_accel_dialog];
  action_group.add_action_with_accel[action,"<control>Q"];
  action = gtkaction_new( "toolbar-small-icons","Small Icons", "Small Icons", "");
  action.connect["activate",demoaction_toolbar_size_small,list(merge)];
  action_group.add_action[action];
  action = gtkaction_new( "toolbar-large-icons", "Large Icons", "Large Icons", "");
  action.connect["activate",demoaction_toolbar_size_large,list(merge)];
  action_group.add_action[action];
  // now some toggle actions
  // -----------------------
  action = gtktoggleaction_new("bold","_Bold","Change to bold face", "gtk-bold");
  action.connect["activate",demoaction_toggle_action];
  action_group.add_action_with_accel[action,"<control>B"];
  action = gtktoggleaction_new("toggle-cnp","Enable Cut/Copy/Paste",...
			       "Change the sensitivity of the cut, copy"+...
			       " and paste actions",""); 
  action.connect["activate",demoaction_toggle_cnp_actions,list(action_group)];
  action_group.add_action[action];
  // now some radio actions 
  // XXXX Attention les radion actions ne sont pas groupées pour l'instant 
  // justify actions 
  // ---------------
  action1 = gtkradioaction_new("justify-left","_Left","Left justify the text", "gtk-justify-left",1);
  action_group.add_action_with_accel[action1,"<control>L"];
  action = gtkradioaction_new("justify-center","C_enter","Center justify t"+...
			      " he text","gtk-justify-center",2);
  // action and action1 are in the same group 
  action.set_group[action1];
  action_group.add_action_with_accel[action,"<control>E"];
  action = gtkradioaction_new("justify-right","_Right","Right justify the text","gtk-justify-right",3);
  action.set_group[action1];
  action_group.add_action_with_accel[action, "<control>R"];
  action = gtkradioaction_new("justify-fill", "_Fill","Fill justify the text", "gtk-justify-fill",4);
  action.set_group[action1];
  action_group.add_action_with_accel[action,"<control>J"];
  // set the active one 
  action1.set_current_value[1];
  // we need to use
  // gtk_radio_action_get_group 
  // gtk_action_set_group 
  // The idea is the following 
  // we use set group with an other action to specify that they share the
  // same group 
  // action.set_group[other_action];
  // in order not to use get.group 
    
  // toolbar actions 
  // XXX revoir les valeurs 
  action1 = gtkradioaction_new("toolbar-icons", "Icons","","",0);
  action_group.add_action[action1];
  action1.connect["activate",demoaction_toolbar_style,list(merge)];
  action = gtkradioaction_new("toolbar-text",  "Text","","",1);
  action.set_group[action1];
  action.connect["activate",demoaction_toolbar_style,list(merge)];
  action_group.add_action[action];
  action = gtkradioaction_new("toolbar-both",  "Both","","",2);
  action.set_group[action1];
  action.connect["activate",demoaction_toolbar_style,list(merge)];
  action_group.add_action[action];
  action = gtkradioaction_new("toolbar-both-horiz", "Both Horizontal","","",3);
  action.set_group[action1];
  action.connect["activate",demoaction_toolbar_style,list(merge)]; 
  action_group.add_action[action];
  // set the active one 
  action1.set_current_value[0];
endfunction 

// XML description of the menus for the test app.  The parser understands
// a subset of the Bonobo UI XML format, and uses GMarkup for parsing 


function demoaction_add_widget(merge,widget,args)
  container=args(1);
  container.pack_start[widget,expand=%f,fill=%f,padding=0];
  widget.show[];
  if type(widget,'short')=='GtkToolbar' then 
    toolbar = widget;
    merge.set_data[toolbar=toolbar];
    // this should be compatible with value selected in radio_actions
    toolbar.set_style[0];
    // toolbar.show_arrow[%t];
  end 
endfunction

function ensure_update(manager)
  manager.ensure_update[];
endfunction

function demoaction_add_cb (button,args)
// add a dynamic menu 
  manager = args(1);
  ui_id=manager.get_data['ui_id'];
  if ui_id <> -1 then return ;end 
  dag = gtkactiongroup_new ("DynamicActions");
  manager.set_data[dag=dag];
  manager.insert_action_group[dag, 0];
  ui_id = manager.new_merge_id[];
  manager.set_data[ui_id= ui_id];
  for i = 0:10 
    name = sprintf("DynAction%u", i);
    label = sprintf ("Dynamic Item %d", i);
    action = gtkaction_new(name,label,"","");
    dag.add_action[action];
    manager.add_ui[ ui_id, "/menubar/DynamicMenu", name, name, ...
		    GTK.UI_MANAGER_MENUITEM,%f];
    manager.ensure_update[];
  end
endfunction 

function demoaction_remove_cb(button, args )
// remove dynamic menu 
  manager = args(1);
  ui_id=manager.get_data['ui_id'];
  if ui_id == -1 then return ;end 
  manager.remove_ui[ui_id];
  manager.ensure_update[];
  dag = manager.get_data['dag'];
  manager.remove_action_group[dag];
  manager.set_data[ui_id=-1];
endfunction

function demo_actions()
//  gtk_accel_map_load ("accels");
// gtk_accel_map_save ("accels");
  
  ui_info = ["  <menubar>\n"
	     "    <menu name=""Menu _1"" action=""Menu1Action"">\n"
	     "      <menuitem name=""cut"" action=""cut"" />\n"
	     "      <menuitem name=""copy"" action=""copy"" />\n"
	     "      <menuitem name=""paste"" action=""paste"" />\n"
	     "      <separator name=""sep1"" />\n"
	     "      <menuitem name=""bold1"" action=""bold"" />\n"
	     "      <menuitem name=""bold2"" action=""bold"" />\n"
	     "      <separator name=""sep2"" />\n"
  //	   "      <menuitem name=""recent"" action=""recent"" />\n"
	     "      <separator name=""sep3"" />\n"
	     "      <menuitem name=""toggle-cnp"" action=""toggle-cnp"" />\n"
	     "      <separator name=""sep4"" />\n"
	     "      <menuitem name=""quit"" action=""quit"" />\n"
	     "    </menu>\n"
	     "    <menu name=""Menu _2"" action=""Menu2Action"">\n"
	     "      <menuitem name=""cut"" action=""cut"" />\n"
	     "      <menuitem name=""copy"" action=""copy"" />\n"
	     "      <menuitem name=""paste"" action=""paste"" />\n"
	     "      <separator name=""sep5""/>\n"
	     "      <menuitem name=""bold"" action=""bold"" />\n"
	     "      <separator name=""sep6""/>\n"
	     "      <menuitem name=""justify-left"" action=""justify-left"" />\n"
	     "      <menuitem name=""justify-center"" action=""justify-center"" />\n"
	     "      <menuitem name=""justify-right"" action=""justify-right"" />\n"
	     "      <menuitem name=""justify-fill"" action=""justify-fill"" />\n"
	     "      <separator name=""sep7""/>\n"
	     "      <menuitem  name=""customise-accels"" action=""customise-accels"" />\n"
	     "      <separator name=""sep8""/>\n"
	     "      <menuitem action=""toolbar-icons"" />\n"
	     "      <menuitem action=""toolbar-text"" />\n"
	     "      <menuitem action=""toolbar-both"" />\n"
	     "      <menuitem action=""toolbar-both-horiz"" />\n"
	     "      <separator name=""sep9""/>\n"
	     "      <menuitem action=""toolbar-small-icons"" />\n"
	     "      <menuitem action=""toolbar-large-icons"" />\n"
	     "    </menu>\n"
	     "    <menu name=""DynamicMenu"" action=""Menu3Action"" />\n"
	     "  </menubar>\n"
	     "  <toolbar name=""toolbar"">\n"
	     "    <toolitem name=""cut"" action=""cut"" />\n"
	     "    <toolitem name=""copy"" action=""copy"" />\n"
	     "    <toolitem name=""paste"" action=""paste"" />\n"
  //	   "    <toolitem name=""recent"" action=""recent"" />\n"
	     "    <separator name=""sep10"" />\n"
	     "    <toolitem name=""bold"" action=""bold"" />\n"
	     "    <separator name=""sep11"" />\n"
	     "    <toolitem name=""justify-left"" action=""justify-left"" />\n"
	     "    <toolitem name=""justify-center"" action=""justify-center"" />\n"
	     "    <toolitem name=""justify-right"" action=""justify-right"" />\n"
	     "    <toolitem name=""justify-fill"" action=""justify-fill"" />\n"
	     "    <separator name=""sep12""/>\n"
	     "    <toolitem name=""quit"" action=""quit"" />\n"
	     "  </toolbar>\n"
	     "  <popup name=""popup"">\n"
	     "    <menuitem name=""popcut"" action=""cut"" />\n"
	     "    <menuitem name=""popcopy"" action=""copy"" />\n"
	     "    <menuitem name=""poppaste"" action=""paste"" />\n"
	     "  </popup>\n"];
  
  str = strcat(ui_info);
  merge = gtkuimanager_new ();
  window = gtkwindow_new();// GTK_WINDOW_TOPLEVEL;
  window.set_default_size[  -1, -1];
  window.set_title[ "Actions Demo"];
  //g_signal_connect_swapped (window, "destroy", G_CALLBACK (g_object_unref), merge);
  //g_signal_connect (window, "destroy", G_CALLBACK (gtk_main_quit), NULL);
  box = gtkvbox_new(homogeneous=%f,spacing=0);
  window.add[box];
  box.show[];
  
  action_group = gtkactiongroup_new("TestActions")
  demoaction_build_actions(action_group,merge);

  merge.insert_action_group[action_group, 0];
  merge.set_data[ui_id=-1];
  merge.connect["add_widget", demoaction_add_widget, list(box)];
  window.add_accel_group[merge.get_accel_group[]];

  // XXXX changer l'interface pour enlever length !!

  rep = merge.add_ui_from_string[str,length(str)];

  // revoir le rep 
  if rep==0 then 
    printf("building menus failed: \n");
  end
  
  hbox = gtkhbox_new(homogeneous=%f,spacing=0);
  box.pack_end[hbox,expand=%f,fill=%f,padding=0];
  hbox.show[];
  
  button = gtkbutton_new(label="Add");
  hbox.pack_start[ button,expand=%f,fill=%f,padding=0];
  button.show[];
  button.connect["clicked", demoaction_add_cb, list(merge)];
  
  button = gtkbutton_new(label="Remove");
  hbox.pack_start[ button,expand=%f,fill=%f,padding=0];
  button.show[];
  button.connect["clicked", demoaction_remove_cb, list(merge)];

  window.show[];
endfunction
