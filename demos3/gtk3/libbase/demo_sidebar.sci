// Stack Sidebar
//
// GtkStackSidebar provides an automatic sidebar widget to control
// navigation of a GtkStack object. This widget automatically updates it
// content based on what is presently available in the GtkStack object,
// and using the "title" child property to set the display labels.

function demo_sidebar(do_widget)

  pages = [ "Welcome to GTK+",
	    "GtkStackSidebar Widget",
	    "Automatic navigation",
	    "Consistent appearance",
	    "Scrolling",
	    "Page 6",
	    "Page 7",
	    "Page 8",
	    "Page 9"];
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_resizable[%t];
  window.set_size_request[ 500, 350];
  
  header = gtk_header_bar_new ();
  header.set_show_close_button[%t];
  window.set_titlebar[header];
  window.set_title[ "Stack Sidebar"];
      
  // g_signal_connect (window, "destroy", G_CALLBACK (gtk_widget_destroyed), &window);
  
  box = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 0);
  sidebar = gtk_stack_sidebar_new ();
  box.pack_start[sidebar, expand=%f,fill= %f,padding=0];
      
  stack = gtk_stack_new ();
  stack.set_transition_type[GTK.STACK_TRANSITION_TYPE_SLIDE_UP_DOWN];
  sidebar.set_stack[stack];
  
  // Separator between sidebar and stack */
  widget = gtk_separator_new (GTK.ORIENTATION_VERTICAL);
  box.pack_start[widget,expand=%f,fill= %f,padding=0];
  box.pack_start[stack,expand=%t,fill= %t,padding=0];
      
  for i=1:size(pages,'*')
    if i == 1 then 
      widget = gtk_image_new_from_icon_name("help-about", GTK.ICON_SIZE_MENU);
      widget.set_pixel_size[256];
    else
      widget = gtk_label_new (str=pages(i));
    end
    stack.add_named[widget,pages(i)];
    stack.child_set[ widget, title= pages(i)];
  end
  window.add[box];
  window.show_all[];
endfunction

