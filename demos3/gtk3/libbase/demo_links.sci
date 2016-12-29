// Links
//
// GtkLabel can show hyperlinks. The default action is to call
// gtk_show_uri() on their URI, but it is possible to override
// this with a custom handler.

function window= demo_links (do_widget)

  function y= activate_link (label, uri, data)
    function response_cb (dialog, response_id, data)
      dialog.destroy[];
    endfunction

    if uri.equal["keynav"] then 
      parent = label.get_toplevel [];
      dialog = gtk_message_dialog_new(parent=parent,...
				      flags=GTK.DIALOG_DESTROY_WITH_PARENT,...
				      type= GTK.MESSAGE_INFO,...
				      buttons= GTK.BUTTONS_OK,...
				      markup=%t,
      message = "The term <i>keynav</i> is a shorthand for " + ...
		"keyboard navigation and refers to the process of using " + ...
		"a program (exclusively) via keyboard input.");
      dialog.set_modal[%t];
      dialog.present[];
      dialog.connect[ "response", response_cb];
      y= %t;
      return;
    end
    y=%f;
  endfunction
  
  window = gtk_window_new (type = GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then window.set_screen[do_widget.get_screen []];end
  window.set_title[ "Links"];
  window.set_border_width[12];
  // window.connect[ "destroy", gtk_widget_destroyed, &window);

  text = ["Some <a href=""http://en.wikipedia.org/wiki/Text"" title=""plain text"">text</a> may be marked up\n"
	  "as hyperlinks, which can be clicked\n"
	  "or activated via <a href=""keynav"">keynav</a>\n"
	  "and they work fine with other markup, like when\n"
	  "searching on <a href=""http://www.google.com/"">"
	  "<span color=""#0266C8"">G</span><span color=""#F90101"">o</span>"
	  "<span color=""#F2B50F"">o</span><span color=""#0266C8"">g</span>"
	  "<span color=""#00933B"">l</span><span color=""#F90101"">e</span>"
	  "</a>."];

  
  label = gtk_label_new(str=catenate(text));
  label.set_use_markup[%t];
  label.connect[ "activate-link", activate_link];
  window.add[label];
  label.show[];
  window.show[];
endfunction 
