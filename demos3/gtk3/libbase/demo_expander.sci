// Expander
//
// GtkExpander allows to provide additional content that is initially hidden.
// This is also known as "disclosure triangle".
//
// This example also shows how to make the window resizable only if the expander
// is expanded.


function window=demo_expander (do_widget)

  function response_cb (dialog, response_id, window)
    window.destroy[];
  endfunction

  function expander_cb (expander, pspec, dialog)
    dialog.set_resizable[ expander.get_expanded []];
  endfunction
  
  function do_not_expand (child, data)
    parent=child.get_parent[];
    parent.child_set[child, expand= %f, fill= %f];
  endfunction
  
  message= sprintf("<big><b>%s</b></big>","Something went wrong");
  if nargin >=1 then 
    toplevel = do_widget.get_toplevel [];
    window = gtk_message_dialog_new (parent=toplevel, ...
				     buttons=GTK.BUTTONS_CLOSE,...
				     flags=ior(GTK.DIALOG_MODAL),...
				     type=GTK.MESSAGE_ERROR);
  else
    window = gtk_message_dialog_new (buttons=GTK.BUTTONS_CLOSE,...
				     flags=ior(GTK.DIALOG_MODAL),...
				     type=GTK.MESSAGE_ERROR);
  end
  window.set_markup[message];
  window.format_secondary_text["Here are some more details \n"+...
		    "but not the full story."];
  area = window.get_message_area [];
  box = area.get_parent [];
  parent= box.get_parent [];
  parent.child_set[box,expand= %t,fill= %t];
  area.foreach[do_not_expand];

  expander = gtk_expander_new ("Details:");
  sw = gtk_scrolled_window_new ();
  sw.set_min_content_height[100];
  sw.set_shadow_type[GTK.SHADOW_IN];
  sw.set_policy[ GTK.POLICY_NEVER, GTK.POLICY_AUTOMATIC];

  tv = gtk_text_view_new ();
  buffer = tv.get_buffer [];
  tv.set_editable[%f];
  tv.set_wrap_mode[GTK.WRAP_WORD];

  text= ["Finally, the full story with all details. "
	 "And all the inside information, including "
	 "error codes, etc etc. Pages of information, "
	 "you might have to scroll down to read it all, "
	 "or even resize the window - it works !\n"
	 "A second paragraph will contain even more "
	 "innuendo, just to make you scroll down or "
	 "resize the window. Do it already !"];
  
  buffer.set_text[catenate(text,sep='\n')];
  sw.add[tv];
  expander.add[sw];
  area.pack_end[expander,expand=%t,fill=%t,padding=0];
  expander.show_all[];
  expander.connect[ "notify::expanded",expander_cb, window];
  window.connect[ "response",response_cb, window];
  window.show_all[];
endfunction
