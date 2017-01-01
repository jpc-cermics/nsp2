// Stack
//
// GtkStack is a container that shows a single child at a time,
// with nice transitions when the visible child changes.
//
// GtkStackSwitcher adds buttons to control which child is visible.

function demo_stack(do_widget)
  // builder = gtk_builder_new_from_resource ("/stack/stack.ui");
  ui_stack = getenv("NSP")+"/demos3/gtk3/libbase/demo_stack.ui";
  builder = gtk_builder_new_from_file(ui_stack);
  // builder.connect_signals[H,args];
  window = builder.get_object["window1"];
  // window.set_screen[do_widget.get_screen[]];
  // g_signal_connect (window, "destroy", G_CALLBACK (gtk_widget_destroyed), &window);
  window.show_all[];
endfunction


