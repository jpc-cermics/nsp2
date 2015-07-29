
function app=demo_sunny()

  function new_window (app)
    window = gtk_application_window_new (app);
    window.set_show_menubar[%f];
    window.set_default_size[640, 480];
    window.set_title["Sunny"];
    window.set_icon_name["sunny"];

    header = gtk_header_bar_new ();
    header.show[];
    header.set_title [ "Sunny"];
    header.set_show_close_button[%t];
    window.set_titlebar[header];

    overlay = gtk_overlay_new ();
    window.add[overlay];

    scrolled = gtk_scrolled_window_new ();
    scrolled.set_hexpand[%t];
    scrolled.set_vexpand[%t];
    view = gtk_text_view_new ();

    scrolled.add[view];
    overlay.add[scrolled];

    buffer = view.get_buffer [];
    buffer.set_text["A text buffer ..."];
    window.show_all[];
  endfunction

  function activate (application)
    printf('activate function\n');
    new_window (application);
  endfunction

  function open (application,files,hint)
    for x = size(files,'*')
      new_window (application, files(i));
    end
  endfunction

  function show_about (action, parameter, user_data)
  // gtk_show_about_dialog: to be done
    about = gtk_about_dialog_new();
    about.set_program_name["Sunny"];
    about.set_title[ "About Sunny"];
    about.set_logo_icon_name["sunny"];
    about.set_comments["A cheap Bloatpad clone."];
    about.show[];
    response = about.run[];
    about.destroy[];

  endfunction

  function quit_app (action, parameter, data)
    printf("Going down...\n");
    app = data(1)
    // app= g_application_get_default ();
    list = app.get_windows[];
    for i=1:length(list)
      list(i).destroy[];
    end
  endfunction

  function new_activated (action, parameter, user_data)
    app = user_data(1);
    app.activate[];
  endfunction

  function populate_action_group(application)
    action_group = application;
    actions_cell = { "about", show_about, "", "";
		     "quit", quit_app, "", "";
		     "new", new_activated, "", ""};

    actions=m2s([]);
    for i=1:size(actions_cell,1)
      actions(i,1:3)=[actions_cell{i,1},actions_cell{i,3},actions_cell{i,4}];
    end

    action_group.add_action_entries[actions];

    for i=1:size(actions_cell,1)
      action= action_group.lookup_action[actions_cell{i,1}];
      action.connect['activate',actions_cell{i,2},list(application)];
    end
  endfunction

  function startup (application,data)
  // menu_button_parent_class->startup (application);
    builder = gtk_builder_new ();
    str = ["<interface>"
	   "  <menu id=''app-menu''>"
	   "    <section>"
	   "      <item>"
	   "        <attribute name=''label'' translatable=''yes''>_New Window</attribute>"
	   "        <attribute name=''action''>app.new</attribute>"
	   "      </item>"
	   "      <item>"
	   "        <attribute name=''label'' translatable=''yes''>_About Sunny</attribute>"
	   "        <attribute name=''action''>app.about</attribute>"
	   "      </item>"
	   "      <item>"
	   "        <attribute name=''label'' translatable=''yes''>_Quit</attribute>"
	   "        <attribute name=''action''>app.quit</attribute>"
	   "        <attribute name=''accel''>&lt;Primary&gt;q</attribute>"
	   "      </item>"
	   "    </section>"
	   "  </menu>"
	   "</interface>"];
    builder.add_from_string[str]
    application.set_app_menu [ builder.get_object[ "app-menu"]];
    populate_action_group(application);
    // new_window(application);
  endfunction
  flags = ior(GIO.APPLICATION_NON_UNIQUE,GIO.APPLICATION_HANDLES_OPEN);
  app= gtk_application_new ("org.gtk.Test.SunnyNsp",flags);
  app.connect["open", open,list(app)];
  app.connect["startup", startup,list(app)];
  app.connect["activate", activate ,list(app)];
  app.run[0,m2s([])];
endfunction
