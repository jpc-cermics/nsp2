// testfilechooser
// Copyright (C) 2003  Red Hat, Inc.
// Author: Owen Taylor
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


function demo_file_chooser()
  force_rtl = %f;
  multiple = %t;
  local_only = %f;
    
  function print_current_folder (chooser)
    uri = chooser.get_current_folder_uri [];
    printf("Current folder changed: ""%s""\n", uri);
  endfunction

  function print_selected (chooser)
    uris = chooser.get_uris [];
    printf ("Selection changed :\n");
    for i=1:length(uris)
      printf("  %s\n",uris(i));
    end
  endfunction

  function response_cb (dialog, response_id)
    if response_id == GTK.RESPONSE_OK then 
      list = dialog.get_uris [];
    else
      printf ("Dialog was closed\n");
    end
    gtk_main_quit ();
  endfunction

  function y=no_backup_files_filter(filter_info,data)
    pause no_backup_files_filter
    // //{
    //   gsize len = filter_info->display_name ? strlen (filter_info->display_name) : 0;
    //   if (len > 0 && filter_info->display_name[len - 1] == '~')
    //     return 0;
    //   else
    //     return 1;
    //   end
  endfunction

  function filter_changed (dialog, data)
    printf("file filter changed\n");
  endfunction

  function set_current_folder (chooser, name)
    if chooser.set_current_folder[name] then return;end 
    return;
    // buttons=[ GTK.MESSAGE_ERROR, GTK.BUTTONS_CLOSE],...
    dialog = gtk_message_dialog_new (parent=chooser,...
				     flags=ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),...
				     buttons=GTK.BUTTONS_CLOSE,...
				     message=sprintf("Could not set the folder to %s",name));
    gtk_dialog_run (dialog);
    dialog.destroy[];
  endfunction

  function set_folder_nonexistent_cb (button,   chooser)
    set_current_folder (chooser, "/nonexistent");
  endfunction

  function set_folder_existing_nonexistent_cb (button, chooser)
    set_current_folder (chooser, "/usr/nonexistent");
  endfunction

  function set_filename (chooser, name)
    if chooser.set_filename[name] then return;end
    return;
    // buttons=[ GTK.MESSAGE_ERROR, GTK.BUTTONS_CLOSE],...
    dialog = gtk_message_dialog_new (parent=chooser,...
				     flags=ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),...
				     buttons=GTK.BUTTONS_CLOSE,...
				     message=sprintf("Could not select %s",name));
    dialog.run[];
    dialog.destroy[];
  endfunction

  function set_filename_nonexistent_cb (button,  chooser)
    set_filename (chooser, "/nonexistent");
  endfunction

  function set_filename_existing_nonexistent_cb (button,   chooser)
    set_filename (chooser, "/usr/nonexistent");
  endfunction

  function get_selection_cb (button, chooser)
    selection = chooser.get_uris [];
    if length(selection)== 0 then 
      printf("Selection: empty\n");
      return;
    else
      printf("Selection: \n");
      for i=1:length(selection) do printf('  %s\n',selection(i));end
    end
  endfunction

  function get_current_name_cb (button, chooser)
    name = chooser.get_current_name [];
    printf ("Current name: ""%s""\n", name)
  endfunction

  function unmap_and_remap_cb (button, chooser)
    chooser.hide[];
    chooser.show[];
  endfunction

  function kill_dependent (win, dep)
    dep.destroy[];
    dep.unref[];
  endfunction

  function notify_multiple_cb (dialog, pspec,  button)
    multiple = dialog.get_select_multiple [];
    button.set_sensitive[multiple];
  endfunction

  function conf= confirm_overwrite_cb (chooser, data)

  //GTK.MESSAGE_QUESTION, GTK.BUTTONS_NONE
    
    dialog = gtk_message_dialog_new (parent= chooser.get_toplevel [],...
				     flags= ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),...
				     message="What do you want to do?");
    
    button = gtk_button_new(label="Use the stock confirmation dialog");
    button.show[];
    dialog.add_action_widget[button, 1];

    button = gtk_button_new(label="Type a new file name");
    button.show[];
    dialog.add_action_widget[button,2];

    button = gtk_button_new(label="Accept the file name");
    button.show[];
    dialog.add_action_widget[button,3];

    response = dialog.run[];

    select response 
     case 1
      conf = GTK.FILE_CHOOSER_CONFIRMATION_CONFIRM;
     case 3
      conf = GTK.FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME;
    else
      conf = GTK.FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN;
    end
    dialog.destroy[];
  endfunction
  
  //GOptionEntry options[] = 
  //{ "action", 'a', 0, G_OPTION_ARG_STRING, &action_arg, "Filechooser action", "ACTION" end,
  //{ "multiple", 'm', 0, G_OPTION_ARG_NONE, &multiple, "Select multiple", NULL end,
  //{ "local-only", 'l', 0, G_OPTION_ARG_NONE, &local_only, "Local only", NULL end,
  //{ "right-to-left", 'r', 0, G_OPTION_ARG_NONE, &force_rtl, "Force right-to-left layout.", NULL end,
  //{ "initial-filename", 'f', 0, G_OPTION_ARG_FILENAME, &initial_filename, "Initial filename to select", "FILENAME" end,
  //{ "initial-folder", 'F', 0, G_OPTION_ARG_FILENAME, &initial_folder, "Initial folder to show", "FILENAME" end,
  //{ NULL end
  
  if force_rtl then 
    gtk_widget_set_default_direction (GTK.TEXT_DIR_RTL);
  end
  
  // action = GTK.FILE_CHOOSER_ACTION_OPEN;
  // action = GTK.FILE_CHOOSER_ACTION_SAVE;
  // action = GTK.FILE_CHOOSER_ACTION_SELECT_FOLDER;
  // action = GTK.FILE_CHOOSER_ACTION_CREATE_FOLDER;
  
  action = GTK.FILE_CHOOSER_ACTION_OPEN;
  dialog = gtk_file_chooser_dialog_new( action = action)
  dialog.set_select_multiple[multiple];
  dialog.set_local_only[local_only];
  
  // dialog = dialog.gcast_up[]; //  gtk_file_chooser_dialog_new should
  //  return a dialog ? 
  
  select action 
   case { GTK.FILE_CHOOSER_ACTION_OPEN, GTK.FILE_CHOOSER_ACTION_SELECT_FOLDER} then
    dialog.set_title[ "Select a file"];
    button = gtk_button_new(mnemonic="_Cancel");
    button.show[];
    dialog.add_action_widget[button,GTK.RESPONSE_CANCEL ];
    button = gtk_button_new(mnemonic="_Open");
    button.show[];
    dialog.add_action_widget[button,GTK.RESPONSE_OK ];
   case {GTK.FILE_CHOOSER_ACTION_SAVE, GTK.FILE_CHOOSER_ACTION_CREATE_FOLDER} then
    dialog.set_title[ "Save a file"];
    button = gtk_button_new(mnemonic="_Cancel");
    button.show[];
    dialog.add_action_widget[button,GTK.RESPONSE_CANCEL ];
    button = gtk_button_new(mnemonic="_Save");
    button.show[];
    dialog.add_action_widget[button,GTK.RESPONSE_OK ];
  end
  
  dialog.set_default_response[GTK.RESPONSE_OK];
  dialog.connect[ "selection-changed", print_selected];
  dialog.connect [ "current-folder-changed", print_current_folder];
  dialog.connect [ "response", response_cb];
  dialog.connect [ "confirm-overwrite", confirm_overwrite_cb];
  
  //  Filters  
  filter = gtk_file_filter_new ();
  filter.set_name[ "All Files"];
  filter.add_pattern[ "*"];
  dialog.add_filter[filter];
  //  Make this filter the default  
  dialog.set_filter[filter];

  if %f then 
    filter = gtk_file_filter_new ();
    filter.set_name[ "No backup files"];
    filter.add_custom[ GTK.FILE_FILTER_DISPLAY_NAME, no_backup_files_filter];
    filter.add_mime_type["image/png"];
    dialog.add_filter[filter];
  end
  
  filter = gtk_file_filter_new ();
  filter.set_name[ "Starts with D"];
  filter.add_pattern[ "D*"];
  dialog.add_filter[filter];

  dialog.connect[ "notify::filter", filter_changed];

  filter = gtk_file_filter_new ();
  filter.set_name [ "PNG and JPEG"];
  filter.add_mime_type[ "image/jpeg"];
  filter.add_mime_type[ "image/png"];
  dialog.add_filter[filter];

  filter = gtk_file_filter_new ();
  filter.set_name[ "Images"];
  filter.add_pixbuf_formats[];
  dialog.add_filter[filter];

  //  Extra widget  

  extra = gtk_check_button_new(mnemonic="Lar_t whoever asks about this button");
  extra.set_active[%t];
  dialog.set_extra_widget[extra];

  //  Shortcuts  
  
  dialog.add_shortcut_folder_uri["file:///usr/share/pixmaps"];
  //dialog.add_shortcut_folder[ g_get_user_special_dir (G_USER_DIRECTORY_MUSIC)];
  
  //  Initial filename or folder  
  initial_filename=%f;
  if initial_filename  then 
    dialog.set_filename[initial_filename];
  end
  initial_folder=%f
  if initial_folder then 
    dialog.set_current_folder[ initial_folder];
  end
  
  //  show_all() to reveal bugs in composite widget handling  
  dialog.show_all[];
  
  //  Extra controls for manipulating the test environment
  
  control_window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);

  vbbox = gtk_button_box_new (GTK.ORIENTATION_VERTICAL);
  control_window.add[vbbox];

  function f_gtk_file_chooser_select_all(button,dialog)
    dialog.select_all[];    
  endfunction

  function f_gtk_file_chooser_unselect_all(button,dialog)
    dialog.unselect_all[];    
  endfunction
  
  button = gtk_button_new(mnemonic="_Select all");
  button.set_sensitive[multiple];
  vbbox.add[button];
  button.connect[ "clicked", f_gtk_file_chooser_select_all, dialog];
  dialog.connect[ "notify::select-multiple", notify_multiple_cb, button];
  
  button = gtk_button_new(mnemonic="_Unselect all");
  vbbox.add[button];
  button.connect[ "clicked", f_gtk_file_chooser_unselect_all, dialog];
  
  button = gtk_button_new(label="set_current_folder (""/nonexistent"")");
  vbbox.add[button];
  button.connect[ "clicked", set_folder_nonexistent_cb, dialog];

  button = gtk_button_new(label="set_current_folder (""/usr/nonexistent"")");
  vbbox.add[button];
  button.connect[ "clicked",  set_folder_existing_nonexistent_cb, dialog];

  button = gtk_button_new(label="set_filename (""/nonexistent"")");
  vbbox.add[button];
  button.connect[ "clicked", set_filename_nonexistent_cb, dialog];

  button = gtk_button_new(label="set_filename (""/usr/nonexistent"")");
  vbbox.add[button];
  button.connect[ "clicked",  set_filename_existing_nonexistent_cb, dialog];

  button = gtk_button_new(label="Get selection");
  vbbox.add[button];
  button.connect[ "clicked", get_selection_cb, dialog];

  button = gtk_button_new(label="Get current name");
  vbbox.add[button];
  button.connect [ "clicked",  get_current_name_cb, dialog];

  button = gtk_button_new(label="Unmap and remap");
  vbbox.add[button];
  button.connect [ "clicked", unmap_and_remap_cb, dialog];

  control_window.show_all[];
  // We need to hold a ref until we have destroyed the widgets, just in case
  // someone else destroys them.  We explicitly destroy windows to catch leaks.
  control_window.ref[];
  dialog.connect [ "destroy", kill_dependent, control_window];
  dialog.ref[];
  gtk_main ();
  dialog.destroy[];
  dialog.unref[];
endfunction

