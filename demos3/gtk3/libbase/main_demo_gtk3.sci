

function main_demo_gtk3() 
// The main program 

  function window_closed_cb (window, data)
    STYLE_COLUMN=4;
    tree_model = data.model;
    iter = tree_model.get_iter[data.path];
    style = data.model.get_value[iter, STYLE_COLUMN];
    if style == PANGO.STYLE_ITALIC then 
      data.model.set[iter, STYLE_COLUMN, PANGO.STYLE_NORMAL];
    end
  endfunction 
  
  function run_example_for_row (window, model, iter)
    
    FUNC_COLUMN=3;
    STYLE_COLUMN=4;
    
    // printf("run_example_for_row\n");
    func= model.get_value[iter, FUNC_COLUMN];
    style = model.get_value[iter, STYLE_COLUMN];
    if length(func)<>0 then 
      if style == PANGO.STYLE_ITALIC then 
	new_style = PANGO.STYLE_NORMAL;
      else 
	new_style = PANGO.STYLE_ITALIC;
      end 
      model.set[iter,STYLE_COLUMN,new_style];
      demo=[];
      ok = execstr(sprintf('demo= %s(window)',func),errcatch=%t);
      if ~ok then lasterror();end
      if type(demo,'short')<>'m'  then 
	data = hash(model = model, path = model.get_path[iter]);
	istop=%f;
	ok = execstr('istop=demo.is_toplevel[]',errcatch=%t);
	if ~ok then lasterror();end
	if istop then 
	  demo.set_transient_for[window];
	  demo.set_modal[%t];
	end
	ok = execstr('demo.connect[""destroy"", window_closed_cb, data]',errcatch=%t);
	if ~ok then lasterror();end
      end
    end
  endfunction 

  function load_file (demoname, filename, info_view, source_view);
    persistent(current_file="");
    if filename == "" then return;end
    if current_file == filename then return;end 
    current_file = filename;
    
    info_buffer = gtk_text_buffer_new ();
    info_buffer.create_tag["title", font = "Sans 18", pixels_below_lines= 10];

    source_buffer = gtk_text_buffer_new ();
    source_buffer.create_tag["source",font= "monospace"];
    source_buffer.create_tag["comment","foreground"= "DodgerBlue"]
    source_buffer.create_tag["type", foreground= "ForestGreen"]
    source_buffer.create_tag["string", foreground= "RosyBrown", weight= PANGO.WEIGHT_BOLD];
    source_buffer.create_tag["control", foreground= "purple"];
    source_buffer.create_tag["preprocessor",  style= PANGO.STYLE_OBLIQUE, foreground= "burlywood4"];
    source_buffer.create_tag["function", weight= PANGO.WEIGHT_BOLD, foreground= "DarkGoldenrod4"];
    
    resource_filename = sprintf("%s/demos3/gtk3/libbase/%s",getenv("NSP"), filename);
    if ~file('exists',resource_filename) then 
      printf("Warning: Cannot open source for %s\n", resource_filename);
      source_buffer.set_text [ ' '];
      source_view.set_buffer[source_buffer];
      info_buffer.set_text[ 'Demo for ' + demoname + ' not found!'];
      info_view.set_buffer[info_buffer];
      return;
    end
    // The -gtk-icon-shadow should be un commented depending on gtk version
    
    if %f then 
      S=getfile(resource_filename);
      source_buffer.set_text [ catenate(S,sep='\n')];
    else
      S=pretty_printer(resource_filename,target="gtk");
      iter = source_buffer.get_start_iter[];
      source_buffer.insert_markup[iter, catenate(S,sep='\n'),-1];
    end
    source_view.set_buffer[source_buffer];
    
    S=getfile(resource_filename);
    I=strstr(S,"//");
    J=find(I<>1);
    I=I(1:J(1)-1);
    S=S(1:size(I,'*'));
    S=strsubst(S,'//','');
    info_buffer.set_text[catenate(S,sep='\n')];
    info_view.set_buffer[info_buffer];
  endfunction 

  function selection_cb (selection, data)
    model=data(1);
    headerbar=data(2);
    info_view=data(3);
    source_view=data(4);
    iter = selection.get_selected[];
    name= model.get_value[iter, 0];// NAME_COLUMN];
    title= model.get_value[iter, 1];//TITLE_COLUMN];
    filename= model.get_value[iter, 2];//FILENAME_COLUMN];
    load_file (name, filename,info_view,source_view);
    headerbar.set_title[title];
  endfunction 

  function populate_model (model)

    child0 = { { "css_accordion", "CSS Accordion", "demo_css_accordion.sci", "demo_css_accordion", "" },
	       { "css_basics", "CSS Basics", "demo_css_basics.sci", "demo_css_basics", "" },
	       { "css_multiplebgs", "Multiple Backgrounds", "demo_css_multiplebgs.sci", "demo_css_multiplebgs", "" },
	       { "css_pixbufs", "Animated Backgrounds", "demo_css_pixbufs.sci", "demo_css_pixbufs", "" },
	       { "css_shadows", "Shadows", "demo_css_shadows.sci", "demo_css_shadows", "" },
	       { "theming_style_classes", "Style Classes", "demo_theming_style_classes.sci", "demo_theming_style_classes", "" }};

    child1 = { { "editable_cells", "Editable Cells", "demo_editable_cells.sci", "demo_editable_cells", "" },
	       { "filtermodel", "Filter Model", "demo_filtermodel.sci", "demo_filtermodel", "" },
	       { "list_store", "List Store", "demo_list_store.sci", "demo_list_store", "" },
	       { "tree_store", "Tree Store", "demo_tree_store.sci", "demo_tree_store", "" }	   };
    
    // change temporary 
    child1 = { { "editable_cells", "Editable Cells", "demo_treeview_editable.sci", "demo_treeview_editable", "" },
	       { "list_store_1", "List Store 1", "demo_list_store_1.sci", "demo_list_store_1", "" },
	       { "list_store_2", "List Store 2", "demo_list_store_1.sci", "demo_list_store_2", "" },
	       { "tree_store_1", "Tree Store 1", "demo_tree_store_1.sci", "demo_tree_store_1", "" },
	       { "tree_store_2", "Tree Store 2", "demo_tree_store_2.sci", "demo_tree_store_2", "" }	   };
    
    child2 = { { "entry_buffer", "Entry Buffer", "demo_entry_buffer.sci", "demo_entry_buffer", "" },
	       { "entry_completion", "Entry Completion", "demo_entry_completion.sci", "demo_entry_completion", "" },
	       { "search_entry", "Search Entry", "demo_search_entry.sci", "demo_search_entry", "" },
	       { "search_entry2", "Delayed Search Entry", "demo_search_entry2.sci", "demo_search_entry2", "" } };

    child3 = { { "font_features", "Font Features", "demo_font_features.sci", "demo_font_features", "" },
	       { "rotated_text", "Rotated Text", "demo_rotated_text.sci", "demo_rotated_text", "" },
	       { "textmask", "Text Mask", "demo_textmask.sci", "demo_textmask", "" }  };

    child4 = { { "hypertext", "Hypertext", "demo_hypertext.sci", "demo_hypertext", "" },
	       { "markup", "Markup", "demo_markup.sci", "demo_markup", "" },
	       { "textview", "Multiple Views", "demo_textview.sci", "demo_textview", "" },
	       { "textscroll", "Automatic Scrolling", "demo_textscroll.sci", "demo_textscroll", "" } };

    child5 = { { "iconview", "Icon View Basics", "demo_iconview.sci", "demo_iconview", "" },
	       { "iconview_edit", "Editing and Drag-and-Drop", "demo_iconview_edit.sci", "demo_iconview_edit", "" } };

    child6 = { { "offscreen_window", "Rotated Button", "demo_offscreen_window.sci", "demo_offscreen_window", "" },
	       { "offscreen_window2", "Effects", "demo_offscreen_window2.sci", "demo_offscreen_window2", "" }  };

    child7 = { { "overlay", "Interactive Overlay", "demo_overlay.sci", "demo_overlay", "" },
	       { "overlay2", "Decorative Overlay", "demo_overlay2.sci", "demo_overlay2", "" },
	       { "transparent", "Transparency", "demo_transparent.sci", "demo_transparent", "" }  };

    child8 = { { "printing", "Printing", "demo_printing.sci", "demo_printing", "" }  };

    gtk_demos = {{ "application", "Application Class", "demo_application.sci", "demo_application", "" }, 
		 { "assistant", "Assistant", "demo_assistant.sci", "demo_assistant", "" }, 
		 { "builder", "Builder", "demo_builder.sci", "demo_builder", "" }, 
		 { "button_box", "Button Boxes", "demo_button_box.sci", "demo_button_box", "" }, 
		 { "", "CSS Theming", "", "", child0 }, 
		 { "changedisplay", "Change Display", "demo_changedisplay.sci", "demo_changedisplay", "" }, 
		 { "clipboard", "Clipboard", "demo_clipboard.sci", "demo_clipboard", "" }, 
		 { "colorsel", "Color Chooser", "demo_color_selection.sci", "demo_color_selection", "" }, 
		 { "combobox", "Combo Boxes", "demo_combobox.sci", "demo_combobox", "" }, 
		 { "cursors", "Cursors", "demo_cursors.sci", "demo_cursors", "" }, 
		 { "dialog", "Dialogs and Message Boxes", "demo_dialog.sci", "demo_dialog", "" }, 
		 { "drawingarea", "Drawing Area", "demo_drawingarea.sci", "demo_drawingarea", "" }, 
		 { "", "Entry", "", "", child2 }, 
		 { "event_axes", "Event Axes", "demo_event_axes.sci", "demo_event_axes", "" }, 
		 { "expander", "Expander", "demo_expander.sci", "demo_expander", "" }, 
		 { "flowbox", "Flow Box", "demo_flowbox.sci", "demo_flowbox", "" },  // done
		 { "gestures", "Gestures", "demo_gestures.sci", "demo_gestures", "" }, 
		 { "headerbar", "Header Bar", "demo_headerbar.sci", "demo_headerbar", "" }, 
		 { "", "Icon View", "", "", child5 }, 
		 { "images", "Images", "demo_images.sci", "demo_images", "" }, 
		 { "infobar", "Info Bars", "demo_infobar.sci", "demo_infobar", "" }, 
		 { "links", "Links", "demo_links.sci", "demo_links", "" }, 
		 { "listbox", "List Box", "demo_listbox.sci", "demo_listbox", "" }, 
		 { "menus", "Menus", "demo_menus.sci", "demo_menus", "" }, 
		 { "modelbutton", "Model Button", "demo_modelbutton.sci", "demo_modelbutton", "" }, 
		 { "", "Offscreen Windows", "", "", child6 }, 
		 { "glarea", "OpenGL Area", "demo_glarea.sci", "demo_glarea", "" }, 
		 { "", "Overlay", "", "", child7 }, 
		 { "panes", "Paned Widgets", "demo_panes.sci", "demo_panes", "" }, 
		 { "", "Pango", "", "", child3 }, 
		 { "pickers", "Pickers", "demo_pickers.sci", "demo_pickers", "" }, 
		 { "pixbufs", "Pixbufs", "demo_pixbufs.sci", "demo_pixbufs", "" }, 
		 { "popover", "Popovers", "demo_popover.sci", "demo_popover", "" }, 
		 { "", "Printing", "", "", child8 }, 
		 { "revealer", "Revealer", "demo_revealer.sci", "demo_revealer", "" }, 
		 { "scale", "Scale", "demo_scale.sci", "demo_scale", "" }, 
		 { "sizegroup", "Size Groups", "demo_sizegroup.sci", "demo_sizegroup", "" }, 
		 { "spinbutton", "Spin Button", "demo_spinbutton.sci", "demo_spinbutton", "" }, 
		 { "spinner", "Spinner", "demo_spinner.sci", "demo_spinner", "" }, 
		 { "stack", "Stack", "demo_stack.sci", "demo_stack", "" }, 
		 { "sidebar", "Stack Sidebar", "demo_sidebar.sci", "demo_sidebar", "" }, 
		 { "", "Text View", "", "", child4 }, 
		 { "toolpalette", "Tool Palette", "demo_toolpalette.sci", "demo_toolpalette", "" }, 
		 { "", "Tree View", "", "", child1 }   };
    
    
    for i=1:size(gtk_demos,'*')
      iter = model.append[];
      model.set[iter, 0, gtk_demos{i}{1}];
      model.set[iter, 1, gtk_demos{i}{2}];
      model.set[iter, 2, gtk_demos{i}{3}];
      model.set[iter, 3, gtk_demos{i}{4}]
      model.set[iter, 4, PANGO.STYLE_NORMAL];
      if type(gtk_demos{i}{5},'short')== 'ce';
	children = gtk_demos{i}{5};
	for j=1:size(children,'*')
	  child_iter= model.append[iter];
	  model.set[child_iter, 0, children{j}{1}];
	  model.set[child_iter, 1, children{j}{2}];
	  model.set[child_iter, 2, children{j}{3}];
	  model.set[child_iter, 3, children{j}{4}]
	  model.set[child_iter, 4, PANGO.STYLE_NORMAL];
	end
      end
    end
  endfunction 

  function row_activated_cb (tree_view, path, column)
    window = tree_view.get_toplevel [];
    model = tree_view.get_model [];
    iter = model.get_iter[path];
    run_example_for_row (window, model, iter);
  endfunction 

  
  function add_data_tab (demoname)

    function scrolled_window=create_text (view, is_source)

      scrolled_window = gtk_scrolled_window_new ();
      scrolled_window.set_policy[ GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC];
      scrolled_window.set_shadow_type[ GTK.SHADOW_NONE];
      text_view = gtk_text_view_new ();
      view=text_view;
      text_view.set_property["left-margin", 20,"right-margin", 20,"top-margin", 20,"bottom-margin", 20];
      text_view.set_editable[%f];
      text_view.set_cursor_visible[%f];
      scrolled_window.add[text_view];
      
      if is_source then 
	text_view.set_monospace[%t];
	text_view.set_wrap_mode[GTK.WRAP_NONE];
      else
	//  Make it a bit nicer for text.  
	text_view.set_wrap_mode[GTK.WRAP_WORD];
	text_view.set_pixels_above_lines[2];
	text_view.set_pixels_below_lines[2];
      end
    endfunction
    
    resource_dir = "/"+ demoname;
    resources = g_resources_enumerate_children (resource_dir, 0, NULL);
    for i=1:length(resources) 
      resource_name = resource_dir + "/" +  resources(i);
      widget = gtk_image_new_from_resource (resource_name);
      if (widget.get_pixbuf [] == NULL && widget.get_animation [] == NULL)
	bytes = g_resources_lookup_data (resource_name, 0, NULL);
	g_assert (bytes);
	if (g_utf8_validate (bytes.get_data[NULL], bytes.get_size[], NULL))
	  //  Looks like it parses as text. Dump it into a textview then!  
	  widget = create_text (textview, %f);
	  buffer = gtk_text_buffer_new (NULL);
	  buffer.set_text[ bytes.get_data[NULL], bytes.get_size[]];
	  textview.set_buffer[buffer];
	end
      else
	g_warning ("Don''t know how to display resource ''%s''\n", resource_name);
      end
      widget.show_all[];
      label = gtk_label_new(str=resources[i]);
      label.show[];
      notebook.append_page[widget, label];
      notebook.child_set[widget,"tab-expand", %t];
    end
  endfunction 

  function activate_quit (action, parameter, user_data)
  // printf("activate_quit\n");
    app = user_data;
    windows = app.get_windows [];
    for i =1:length(windows) do windows(i).destroy[];end
  endfunction
  
  function activate_about (action, parameter, user_data)
    app = user_data;
    authors = "The GTK+ Team, nsp version: Jean-Philippe Chancelier";
    version= sprintf ("Running against GTK+ %d.%d.%d", gtk_get_major_version (),...
		      gtk_get_minor_version (),...
		      gtk_get_micro_version ());
    // gtk_show_about_dialog :
    window = app.get_active_window [];
    about = gtk_about_dialog_new();
    about.set_transient_for[window];
    about.set_authors[ authors];
    about.set_program_name["GTK+ Demo"];
    about.set_copyright["(C) 1997-2013 The GTK+ Team"];
    about.set_license_type[GTK.LICENSE_LGPL_2_1];
    about.set_version[ version];
    about.set_title[ "About GTK+ Demo"];
    about.set_logo_icon_name["gtk3-demo"];
    about.set_comments["Nsp version of Program to demonstrate GTK+ widgets."];
    about.set_website[ "http://www.gtk.org"];
    about.show[];
    response = about.run[];
    about.destroy[];
  endfunction 
  
  app_entries = {"about", activate_about, "","";
		 "quit", activate_quit, "",""};
  flags = ior(GIO.APPLICATION_NON_UNIQUE);//,GIO.APPLICATION_HANDLES_COMMAND_LINE);  
  app = gtk_application_new ("org.gtk.Demo",flags);
  
  if %t then 
    // build actions 
    actions=m2s([]);
    for i=1:size(app_entries,1)
      actions(i,1:3)=[app_entries{i,1},app_entries{i,3},app_entries{i,4}];
    end
    app.add_action_entries[actions];
    for i=1:size(app_entries,1)
      action= app.lookup_action[app_entries{i,1}];
      action.connect['activate',app_entries{i,2},app];
    end
    
    //g_action_map_add_action_entries (app,app_entries, app_entries, app);
    //g_application_add_main_option (app, "run", 0, 0, G_OPTION_ARG_STRING, "Run an example", "EXAMPLE");
    //g_application_add_main_option (app, "list", 0, 0, G_OPTION_ARG_NONE, "List examples", NULL);
    //g_application_add_main_option (app, "autoquit", 0, 0, G_OPTION_ARG_NONE, "Quit after a delay", NULL);
  end
  
  function startup (app)
    main_ui = getenv("NSP")+"/demos3/gtk3/libbase/main_demo_gtk3.ui";
    builder = gtk_builder_new_from_file(main_ui);
    appmenu = builder.get_object[ "appmenu"];
    app.set_app_menu[appmenu];
  endfunction
  
  app.connect[ "startup", startup];


  function activate (app)
    
    function activate_run (action, parameter, user_data)
    // printf("In activate_run\n");
      window = user_data(1);
      treeview= user_data(2);
      selection = treeview.get_selection [];
      model= treeview.get_model[];
      ok = execstr('iter = selection.get_selected[model];',errcatch=%t);
      if ~ok then return;end 
      run_example_for_row(window, model, iter);
    endfunction 
    
    win_entries = { "run", activate_run, "",""};
    
    main_ui = getenv("NSP")+"/demos3/gtk3/libbase/main_demo_gtk3.ui";
    builder = gtk_builder_new_from_file(main_ui);

    window = builder.get_object["window"];
    notebook = builder.get_object[ "notebook"];
    info_view = builder.get_object[ "info-textview"];
    source_view =builder.get_object[ "source-textview"];
    headerbar =builder.get_object[ "headerbar"];
    treeview =builder.get_object[ "treeview"];
    model = treeview.get_model [];

    app.add_window[window];
    //  g_action_map_add_action_entries (window, win_entries, win_entries,
    //  window);
    actions=m2s([]);
    for i=1:size(win_entries,1)
      actions(i,1:3)=[win_entries{i,1},win_entries{i,3},win_entries{i,4}];
    end
    window.add_action_entries[actions];
    for i=1:size(win_entries,1)
      action= window.lookup_action[win_entries{i,1}];
      action.connect['activate',win_entries{i,2}, list(window, treeview)];
    end

    sw =builder.get_object[ "source-scrolledwindow"];
    scrollbar = sw.get_vscrollbar [];

    menu = gtk_menu_new ();
    
    item = gtk_menu_item_new(label="Start");
    
    function start_cb (item, scrollbar)
      adj = scrollbar.get_adjustment [];
      adj.set_value[ adj.get_lower[]];
    endfunction 
    
    item.connect[ "activate", start_cb, scrollbar];
    menu.append[item];
    
    function end_cb (item, scrollbar)
      adj = scrollbar.get_adjustment [];
      adj.set_value[ adj.get_upper [] - adj.get_page_size []];
    endfunction 

    item = gtk_menu_item_new(label="End");
    item.connect[ "activate", end_cb, scrollbar];
    menu.append[ item];

    menu.show_all[];

    if %f then 
      function y=scrollbar_popup (scrollbar, menu)
	gtk_menu_popup (menu, NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
      endfunction 
      scrollbar.connect[ "popup-menu", scrollbar_popup, menu];
    end
    
    load_file ("application", "demo_application.sci",  info_view, source_view);
    
    populate_model (model);

    treeview.connect[ "row-activated", row_activated_cb, model];

    widget =builder.get_object[ "treeview-selection"];
    widget.connect[ "changed", selection_cb, list(model,headerbar,info_view,source_view)];

    model = treeview.get_model [];
    iter = model.get_iter_first[];
    widget.select_iter[iter];
    treeview.collapse_all[];
    window.show_all[];
  endfunction 
  
  app.connect[ "activate", activate];
  app.run[0,""];// g_application_run (app, argc, argv);
endfunction 

