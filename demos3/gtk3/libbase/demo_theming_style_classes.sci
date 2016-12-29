// Theming/Style Classes
//
// GTK+ uses CSS for theming. Style classes can be associated
// with widgets to inform the theme about intended rendering.
//
// This demo shows some common examples where theming features
// of GTK+ are used for certain effects: primary toolbars,
// inline toolbars and linked buttons.
  

function demo_theming_style_classes (do_widget)

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  // window.set_screen[ do_widget.get_screen []];
  window.set_title[ "Style Classes"];
  window.set_resizable[%f];
  window.set_border_width[12];
  // window.connect [ "destroy",gtk_widget_destroyed, &window);
  builder= gtk_builder_new_from_file (getenv("NSP")+"/demos3/gtk3/libbase/demo_theming.ui");
  grid = builder.get_object[ "grid"];
  grid.show_all[];
  window.add[grid];
  window.show[];
endfunction

