// Cursors
//
// Demonstrates a useful set of available cursors.

function demo_cursors (do_widget)
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  // window.set_screen[ do_widget.get_screen []);
  window.set_title[ "Cursors"];
  window.set_default_size[500, 500];
  // window.connect xx[ "destroy", gtk_widget_destroyed,  &window);
  sw = gtk_scrolled_window_new();
  sw.set_policy[ GTK.POLICY_NEVER,  GTK.POLICY_AUTOMATIC];
  window.add[sw];
  box = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=0);
  // g_object_set (box, "margin-start", 20,"margin-end", 20,
  // "margin-bottom", 10);
  box.set_property["margin-start", 20];
  box.set_property["margin-end", 20];
  box.set_property["margin-bottom", 10];
  sw.add[box];
  
  function section = add_section (box, heading)
    label = gtk_label_new(str=heading);
    label.set_xalign[ 0.0];
    label.set_margin_top[10];
    label.set_margin_bottom[10];
    box.pack_start[label, fill=%t, expand=%f,padding=0];
    section = gtk_flow_box_new ();
    section.set_halign[GTK.ALIGN_START];
    section.set_selection_mode[GTK.SELECTION_NONE];
    section.set_min_children_per_line[2];
    section.set_max_children_per_line[20];
    box.pack_start[section,fill=%t, expand=%f,padding=0];
  endfunction

  function add_button (section, css_name)
    display = section.get_display [];
    ok=execstr('cursor = gdk_cursor_new_from_name (display, css_name);',errcatch=%t);
    if ~ok then 
      lasterror();
      image = gtk_image_new_from_icon_name ("image-missing", GTK.ICON_SIZE_MENU);
    else
      css_name = strsubst(css_name,"-","_");
      path = sprintf ("%s%s%s_cursor.png",getenv("NSP"),...
		      "/demos3/gtk3/libbase/demo_cursors/", css_name);
      //printf("searching %s\n",path);
      image = gtk_image_new_from_file (path);
    end
    image.set_size_request[32, 32];
    button = gtk_button_new ();
    button.add[image];
    // gtk_style_context_add_class (button.get_style_context [],
    // "image-button");
    
    function set_cursor (button, data)
      cursor = data;
      toplevel = button.get_toplevel [];
      window = toplevel.get_window [];
      window.set_cursor[cursor=cursor];
    endfunction
    
    button.connect[ "clicked", set_cursor, cursor];
    button.set_tooltip_text[css_name];
    section.add[button];
  endfunction
  
  section = add_section (box, "General");
  add_button (section, "default");
  add_button (section, "none");

  section = add_section (box, "Link & Status");
  add_button (section, "context-menu");
  add_button (section, "help");
  add_button (section, "pointer");
  add_button (section, "progress");
  add_button (section, "wait");
  
  section = add_section (box, "Selection");
  add_button (section, "cell");
  add_button (section, "crosshair");
  add_button (section, "text");
  add_button (section, "vertical-text");
  
  section = add_section (box, "Drag & Drop");
  add_button (section, "alias");
  add_button (section, "copy");
  add_button (section, "move");
  add_button (section, "no-drop");
  add_button (section, "not-allowed");
  add_button (section, "grab");
  add_button (section, "grabbing");

  add_button (section, "dnd-ask")
  add_button (section, "dnd-copy")
  add_button (section, "dnd-link")
  add_button (section, "dnd-move")
  add_button (section, "dnd-none")
  
  section = add_section (box, "Resize & Scrolling");
  add_button (section, "all-scroll");
  add_button (section, "col-resize");
  add_button (section, "row-resize");
  add_button (section, "n-resize");
  add_button (section, "e-resize");
  add_button (section, "s-resize");
  add_button (section, "w-resize");
  add_button (section, "ne-resize");
  add_button (section, "nw-resize");
  add_button (section, "se-resize");
  add_button (section, "sw-resize");
  add_button (section, "ew-resize");
  add_button (section, "ns-resize");
  add_button (section, "nesw-resize");
  add_button (section, "nwse-resize");
  
  section = add_section (box, "Zoom");
  add_button (section, "zoom-in");
  add_button (section, "zoom-out");

  window.show_all[];
endfunction

