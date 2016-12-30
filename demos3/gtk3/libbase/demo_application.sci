// Application Class
//
// Demonstrates a simple application.
//
// This examples uses GtkApplication, GtkApplicationWindow, GtkBuilder
// as well as GMenu and GResource. Due to the way GtkApplication is structured,
// it is run as a separate process.

function [app]=demo_application(do_widget)
  
  function show_action_dialog (action,user_data)
    name = action.get_name[];
    mes = sprintf("You activated action: ""%s""",name);
    dialog = gtk_message_dialog_new (flags= GTK.DIALOG_DESTROY_WITH_PARENT,...
				     type= GTK.MESSAGE_INFO,...
				     buttons=GTK.BUTTONS_CLOSE,...
				     message= mes);
    dialog.run[];
    dialog.destroy[];
  endfunction
  
  function show_action_infobar (action,parameter, window)
    name = action.get_name[];
    value = parameter.get_string[];
    text = sprintf("You activated radio action: ""%s"".\nCurrent value: %s",...
		   name, value);
    if window.check_data[ "message"] then 
      message = window.get_data[ "message"];
      message.set_text[text];
    end
    if window.check_data["infobar"] then 
      infobar = window.get_data[ "infobar"];
      infobar.show[];
    end
  endfunction

  function activate_action (action,parameter,user_data)
    show_action_dialog (action,user_data);
  endfunction

  function activate_toggle (action,parameter,user_data)
    show_action_dialog (action,user_data);
    state = action.get_state[];
    action.change_state [ g_variant_new_boolean (~state.get_boolean[])];
  endfunction

  function activate_radio (action, parameter, user_data)
    show_action_infobar (action, parameter, user_data);
    action.change_state[parameter];
  endfunction

  function activate_about (action,parameter, user_data)
    window = user_data;

    authors = ["Peter Mattis",
	       "Spencer Kimball",
	       "Josh MacDonald",
	       "and many more..."];
    documentors = ["Owen Taylor",
		   "Tony Gale",
		   "Matthias Clasen <mclasen@redhat.com>",
		   "and many more..."];

    // gtk_show_about_dialog: to be done
    about = gtk_about_dialog_new();
    about.set_program_name["GTK+ Code Demos"];
    
    version = sprintf("Running against GTK+ %d.%d.%d", gtk_get_major_version (),...
		      gtk_get_minor_version (),...
		      gtk_get_micro_version ());
    about.set_version[version];
    about.set_copyright["(C) 1997-2015 The GTK+ Team"];
    about.set_license_type[GTK.LICENSE_LGPL_2_1];
    about.set_website["http://www.gtk.org"];
    about.set_comments["Program to demonstrate GTK+ functions."];
    // XXX about.set_authors[authors];
    // XXX about.set_documenters[documentors];
    // XXX about.set_logo_icon_name["gtk3-demo"];
    about.set_title[ "About GTK+ Code Demos"];
    
    about.show[];
    response = about.run[];
    about.destroy[];
  endfunction

  function activate_quit (action, parameter, user_data)
    app = user_data;
    list = app.get_windows [];
    for i=1:length(list)
      window= list(i);
      window.destroy[];
    end
  endfunction

  function update_statusbar (buffer,statusbar)
  //  clear any previous message, underflow is allowed
    c = statusbar.get_context_id["test"];
    statusbar.pop[c];
    count = buffer.get_char_count [];
    iter = buffer.get_iter_at_mark[buffer.get_insert []];
    row = iter.get_line[];
    col = iter.get_line_offset[];
    msg = sprintf ("Cursor at row %d column %d - %d chars in document",row,col,count);
    statusbar.push[c, msg];
  endfunction

  function changed_callback(buffer, statusbar)
    update_statusbar(buffer,statusbar);
  endfunction
  
  function mark_set_callback (buffer, new_location, mark, statusbar)
    update_statusbar(buffer,statusbar);
  endfunction

  function change_theme_state (action, state, user_data)
    settings = gtk_settings_get_default ();
    // g_object property
    settings.set_property["gtk-application-prefer-dark-theme",state.get_boolean[]];
    action.set_state[state];
  endfunction

  function change_titlebar_state (action, state, user_data)
    window = user_data;
    window.set_hide_titlebar_when_maximized[ state.get_boolean[]];
    action.set_state[state];
  endfunction

  function change_radio_state (action, state, user_data)
    action.set_state[state];
  endfunction

  function clicked_cb (widget, info)
    info.hide[];
  endfunction

  function startup (app)
    uimenus = getenv("NSP")+"/demos3/gtk3/libbase/demo_application_menus.ui";
    builder = gtk_builder_new_from_file(uimenus);
    // builder.add_from_resource[ "/application/menus.ui"];
    appmenu = builder.get_object["appmenu"];
    menubar = builder.get_object["menubar"];
    app.set_app_menu[appmenu];
    app.set_menubar[menubar];
  endfunction

  function activate (app)

    window = gtk_application_window_new (app);
    window.set_title[ "Application Class"];
    window.set_icon_name[ "document-open"];
    window.set_default_size[400, 400];

    win_entries = { "titlebar", activate_toggle, "", "false", change_titlebar_state,
		    "shape", activate_radio, "s", "''oval''", change_radio_state,
		    "bold", activate_toggle, "", "false", "",
		    "about", activate_about, "", "", "" ,
		    "file1", activate_action, "", "", "",
		    "logo", activate_action, "", "", "" };

    // add_action_entries does not set up the callbacks we do it manually
    // here
    actions=m2s([]);
    for i=1:size(win_entries,1)
      actions(i,1:3)=[win_entries{i,1},win_entries{i,3},win_entries{i,4}];
    end

    window.add_action_entries[actions];
    
    for i=1:size(win_entries,1)
      action= window.lookup_action[win_entries{i,1}];
      action.connect['activate',win_entries{i,2}, window];
      if ~(type(win_entries{i,5},'short')== 's') then
	action.connect['change_state',win_entries{i,5}, window];
      end
      action.set_enabled[%t];
    end
    
    uiapp = getenv("NSP")+"/demos3/gtk3/libbase/demo_application.ui";
    builder = gtk_builder_new_from_file(uiapp);
    //builder = gtk_builder_new ();
    //builder.add_from_resource["/application/demo_application.ui"];

    grid = builder.get_object [ "grid"];
    contents = builder.get_object [ "contents"];
    status = builder.get_object [ "status"];
    message = builder.get_object [ "message"];
    button = builder.get_object [ "button"];
    infobar = builder.get_object [ "infobar"];
    menutool = builder.get_object [ "menutool"];
    toolmenu = builder.get_object [ "toolmenu"];

    window.set_data[message=message];
    window.set_data[infobar=infobar];
    window.add[grid];
    menutool.set_menu[ gtk_menu_new_from_model(toolmenu)];

    contents.grab_focus[];
    button.connect[ "clicked", clicked_cb, infobar];

    //  Show text widget info in the statusbar
    buffer = contents.get_buffer [];
    // connect_object is in fact similar to connect 
    // we increment the ref
    // This function should transmit buffer as first argument
    buffer.connect_object["changed", changed_callback, status];
    buffer.connect_object["mark-set",mark_set_callback, status];
    update_statusbar(buffer,status);
    window.show_all[];
  endfunction

  function activate_color(action, parameter, user_data)
    name = action.get_name[];
    value = parameter.get_string[];
    action.change_state[parameter];
    mes = sprintf("You activated radio action: ""%s"".\nCurrent value: %s\n",...
		  name, value);
    dialog = gtk_message_dialog_new (flags= GTK.DIALOG_DESTROY_WITH_PARENT,...
				     type= GTK.MESSAGE_INFO,...
				     buttons=GTK.BUTTONS_CLOSE,...
				     message= mes);
    dialog.run[];
    dialog.destroy[];
  endfunction

  function change_color_state(action, state, user_data)
    action.set_state[state];
  endfunction
  
  app_entries = { "new",  activate_action, "", "", "" ,
		  "open", activate_action, "", "", "" ,
		  "save", activate_action, "", "", "" ,
		  "save-as", activate_action, "", "", "" ,
		  "quit", activate_quit, "", "", "" ,
		  "color", activate_color, "s", "''red''", change_color_state,
		  "dark", activate_toggle, "", "false", change_theme_state};

  app = gtk_application_new ("org.gtk.Demo2", 0);
  settings = g_settings_new ("org.gtk.Demo");

  // prefer to add color in app_entries 
  // action = settings.create_action["color"];
  // app.add_action[action];
  
  // add_action_entries does not set up the callbacks we do it manually here
  actions=m2s([]);
  for i=1:size(app_entries,1)
    actions(i,1:3)=[app_entries{i,1},app_entries{i,3},app_entries{i,4}];
  end

  app.add_action_entries[actions];

  for i=1:size(app_entries,1)
    action= app.lookup_action[app_entries{i,1}];
    action.connect['activate',app_entries{i,2}, app];
    if ~(type(app_entries{i,5},'short')== 's') then
      action.connect['change_state',app_entries{i,5}, app];
    end
  end
  
  app.connect[ "startup", startup];
  app.connect[ "activate", activate];
  app.run[0,""];
endfunction
