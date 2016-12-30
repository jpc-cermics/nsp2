//  Text View/Hypertext
//
// Usually, tags modify the appearance of text in the view, e.g. making it
// bold or colored or underlined. But tags are not restricted to appearance.
// They can also affect the behavior of mouse and key presses, as this demo
// shows.

function demo_hypertext_show_page (buffer, page)
//  Fills the buffer with text and interspersed links. In any real
//  hypertext app, this method would parse a file to identify the links.
  
  function insert_link (buffer, iter, text, page)
  //  Inserts a piece of text into the buffer, giving it the usual
  // appearance of a hyperlink in a web browser: blue and underlined.
  // Additionally, attaches some data on the tag, to make it recognizable
  // as a link.
    persistent(itag=0);
    itag = itag + 1;
    tag_name = sprintf("tag%0d",itag);
    tag = buffer.create_tag[tag_name, foreground= "blue", underline= PANGO.UNDERLINE_SINGLE];
    tag.set_data[ page = page];
    buffer.insert_with_tags[iter, text, tag];
  endfunction

  persistent(iweight=0);

  buffer.set_text[ ""]
  iter = buffer.get_iter_at_offset[0];
  if (page == 1)
    buffer.insert[iter, "Some text to show that simple "];
    insert_link (buffer, iter, "hypertext", 3);
    buffer.insert[iter , " can easily be realized with "];
    insert_link (buffer, iter, "tags", 2);
    buffer.insert[iter, "."];
  elseif (page == 2)
    text = ["A tag is an attribute that can be applied to some range of text. "
	    "For example, a tag might be called ""bold"" and make the text inside "
	    "the tag bold. However, the tag concept is more general than that; "
	    "tags don''t have to affect appearance. They can instead affect the "
	    "behavior of mouse and key presses, ""lock"" a range of text so the "
	    "user can''t edit it, or countless other things.\n"];
    buffer.insert[iter, catenate(text,sep='\n')];
    insert_link (buffer, iter, "Go back", 1);
  elseif (page == 3)
    iweight =     iweight +1;
    tag = buffer.create_tag[sprintf("weight%0d",iweight), weight= PANGO.WEIGHT_BOLD];
    buffer.insert_with_tags[iter, "hypertext:\n", tag];
    buffer.insert[iter,
		  "machine-readable text that is not sequential but is organized \n"+...
		  "so that related items of information are connected.\n"];
    insert_link (buffer, iter, "Go back", 1);
  end
endfunction

// Looks at all tags covering the position of iter in the text view,
// and if one of them is a link, follow it by showing the page identified
// by the data attached to it.

function demo_hypertext_follow_if_link (text_view,iter)
  tags = iter.get_tags [];
  for i=1:size(tags,'*')
    tag = tags(i);
    page = tag.get_data["page"];
    if (page <> 0)
      demo_hypertext_show_page (text_view.get_buffer [], page);
      break;
    end
  end
endfunction

function window=demo_hypertext (do_widget)

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title[ "Hypertext"];
  if nargin >= 1 then 
    window.set_screen[ do_widget.get_screen []];
  end
  window.set_default_size[450, 450];
  // window.connect[ "destroy", gtk_widget_destroyed, &window];

  window.set_border_width[0];

  view = gtk_text_view_new ();
  view.set_wrap_mode[GTK.WRAP_WORD];
  
  
  //  Links can be activated by pressing Enter.

  function y=key_press_event (text_view, event)
    select  event.keyval 
     case {GDK.KEY_Return, GDK.KEY_KP_Enter} then 
      buffer = text_view.get_buffer [];
      iter = buffer.get_iter_at_mark[ buffer.get_insert []];
      demo_hypertext_follow_if_link (text_view, iter);
    end
    y = %f;
  endfunction
  
  view.connect[ "key-press-event", key_press_event];
  
  
  //  Links can also be activated by clicking or tapping.

  function y= event_after (text_view, ev)
    GDK_BUTTON_PRIMARY = 1
    if (ev.type == GDK.BUTTON_RELEASE) then 
      event = ev;
      if (event.button <> GDK_BUTTON_PRIMARY) then y=%f;return;end 
      ex = event.x;
      ey = event.y;
    elseif (ev.type == GDK.TOUCH_END)
      ex = ev.x;
      ey = ev.y;
    else
      y=%f; return;
    end
    buffer = text_view.get_buffer [];
    //  we shouldn't follow a link if the user has selected something  
    ok = execstr('[start,iend]= buffer.get_selection_bounds[];',errcatch=%t);
    if ok then 
      if ( start.get_offset[] <> iend.get_offset[]) then y=%f; return;end 
    else
      lasterror();
    end
    xy=  text_view.window_to_buffer_coords[GTK.TEXT_WINDOW_WIDGET, ex, ey];
    iter = text_view.get_iter_at_location[ xy(1), xy(2) ];
    demo_hypertext_follow_if_link (text_view, iter);
    y = %t; 
  endfunction
    
  view.connect[ "event-after", event_after];
  
  //  Update the cursor image if the pointer moved.

  function y= motion_notify_event (text_view,  event)

    function set_cursor_if_appropriate (text_view, x, y)
      
      hand_cursor= text_view.get_data[ "hand_cursor"];
      regular_cursor=text_view.get_data[ "regular_cursor"];

      persistent(hovering_over_link = %f);
      hovering = %f;
      iter = text_view.get_iter_at_location[ x, y];
      tags = iter.get_tags[];
      for i=1:size(tags,'*') 
	tag = tags(i);
	if tag.check_data["page"] then 
	  page = tag.get_data[ "page"];
	  if (page <> 0) then hovering = %t; break;end
	end
      end
      
      if (hovering <> hovering_over_link)
	hovering_over_link = hovering;
	window = text_view.get_window[GTK.TEXT_WINDOW_TEXT];
	if (hovering_over_link)
	  window.set_cursor[cursor = hand_cursor];
	else
	  window.set_cursor[cursor = regular_cursor];
	end
      end
    endfunction
    
    xy= text_view.window_to_buffer_coords[GTK.TEXT_WINDOW_WIDGET, event.x, event.y];
    set_cursor_if_appropriate (text_view, xy(1), xy(2));
    y = %f;
  endfunction
  
  view.connect[ "motion-notify-event", motion_notify_event];

  display = view.get_display [];
  hand_cursor = gdk_cursor_new_from_name (display, "pointer");
  regular_cursor = gdk_cursor_new_from_name (display, "text");

  view.set_data[ hand_cursor = hand_cursor ];
  view.set_data[regular_cursor = regular_cursor];
  
  buffer = view.get_buffer [];

  sw = gtk_scrolled_window_new ();
  sw.set_policy[ GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC];
  window.add[sw];
  sw.add[view];
  demo_hypertext_show_page (buffer, 1);
  sw.show_all[];
  window.show[];
endfunction
