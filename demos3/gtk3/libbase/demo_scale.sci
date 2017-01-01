// Scale
//
// GtkScale is a way to select a value from a range.
// Scales can have marks to help pick special values,
// and they can also restrict the values that can be
// chosen.

function window= demo_scale (do_widget)
// we prefer add_from_file method to gtk_builder_new_from_file 
// since errors are catched in nsp when using add_from_file 
//   
  builder = gtk_builder_new();
  builder.add_from_file[(getenv("NSP")+"/demos3/gtk3/libbase/demo_scale.ui")];
  // builder.connect_signals[H,args];
  window = builder.get_object["window1"];
  if nargin >= 1 then window.set_screen[ do_widget.get_screen []];end
  // window.connect[ "destroy", gtk_widget_destroyed, &window];
  window.show_all[];
endfunction

