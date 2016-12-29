// Revealer
//
// GtkRevealer is a container that animates showing and hiding
// of its sole child with nice transitions.

function demo_revealer()

  function b = reveal_one ( data)

    function change_direction ( revealer, args)
    // args is a gparamspec 
    // not used in the callback
      if revealer.get_mapped[] then 
	revealed = revealer.get_child_revealed[];
	revealer.set_reveal_child[~revealed];
      end
    endfunction

    persistent(count=0);
    window= data(1);
    builder = window.get_data["builder"];
    name = sprintf ("revealer%d", count);
    revealer = builder.get_object[name];
    revealer.set_reveal_child[%t];
    revealer.connect["notify::child-revealed",change_direction];
    count = count+1;
    if (count >= 9) then 
      window.set_data[timeout=0];
      b = %f;
    else
      b = %t;
    end
  endfunction 

  function on_destroy ( window)
    timeout= window.get_data["timeout"];
    if timeout == 0 then return;end 
    g_source_remove (timeout);
    window.set_data[timeout=0];
  endfunction 
  
  revealer_ui = getenv("NSP")+"/demos3/gtk3/libbase/demo_revealer.ui";
  builder = gtk_builder_new_from_file(revealer_ui);
  //  builder = gtk_builder_new_from_resource ("/revealer/revealer.ui");
  // gtk_builder_connect_signals (builder, NULL);
  window = builder.get_object[ "window"];
  //gtk_window_set_screen (GTK_WINDOW (window),
  //gtk_widget_get_screen (do_widget));
  window.connect[ "destroy",on_destroy];
  window.set_data[builder=builder];
  
  if ~window.get_visible[] then 
    count = 0;
    timeout = g_timeout_add (690, reveal_one, list(window));
    window.set_data[timeout=timeout];
    window.show_all[];
  else
    window.destroy[];
  end
endfunction

