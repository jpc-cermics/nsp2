// Text View/Automatic Scrolling
//
// This example demonstrates how to use the gravity of
// GtkTextMarks to keep a text view scrolled to the bottom
// while appending text.
  
function window = demo_textscroll (do_widget)

  //  Scroll to the end of the buffer.
  
function y= scroll_to_end (data)
  textview = data(1)
  count = textview.get_data["count"];
  buffer = textview.get_buffer [];

  // Get "end" mark. It's located at the end of buffer because
  // of right gravity
    
  mark = buffer.get_mark[ "end"];
  iter = buffer.get_iter_at_mark[ mark];
  
  // and insert some text at its position, the iter will be
  // revalidated after insertion to point to the end of inserted text
  
  spaces = catenate(smat_create(1,count," "));
  count = count+1;
  buffer.insert [ iter, "\n"];
  buffer.insert [ iter, spaces];
  text = sprintf("Scroll to end scroll to end scroll to end scroll to end %d", count);
  buffer.insert [ iter, text];

  //  Now scroll the end mark onscreen.
  
  textview.scroll_mark_onscreen[ mark];

  // Emulate typewriter behavior, shift to the left if we
  // are far enough to the right.
    
  if (count > 150) then   count = 0;end 
  textview.set_data[count=count];
  y=%t; // G_SOURCE_CONTINUE;
endfunction

//  Scroll to the bottom of the buffer.
  
function y= scroll_to_bottom (data)
  textview = data(1)
  count = textview.get_data["count"];
  buffer = textview.get_buffer [];
  //  Get end iterator  
  iter = buffer.get_end_iter[];

  // and insert some text at it, the iter will be revalidated
  // after insertion to point to the end of inserted text
    
  spaces = catenate(smat_create(1,count," "));
  count = count+1;
  buffer.insert [ iter, "\n"];
  buffer.insert [ iter, spaces];
  text = sprintf ("Scroll to bottom scroll to bottom scroll to bottom scroll to bottom %d", count);
  buffer.insert [ iter, text];

  //  Move the iterator to the beginning of line, so we don't scroll
  // in horizontal direction
    
  iter.set_line_offset[0];

  //  and place the mark at iter. the mark will stay there after we
  // insert some text at the end because it has left gravity.
    
  mark = buffer.get_mark[ "scroll"];
  buffer.move_mark [ mark, iter];

  //  Scroll the mark onscreen.
    
  textview.scroll_mark_onscreen[ mark];

  //  Shift text back if we got enough to the right.
    
  if (count > 40) then     count = 0;end 
  
  textview.set_data[count=count];
  y=%t; // G_SOURCE_CONTINUE;
endfunction

function y = setup_scroll (textview, to_end)

  buffer = textview.get_buffer [];
  iter = buffer.get_end_iter[];
  if (to_end)
    //  If we want to scroll to the end, including horizontal scrolling,
    // then we just create a mark with right gravity at the end of the
    // buffer. It will stay at the end unless explicitly moved with
    // gtk_text_buffer_move_mark.
    buffer.create_mark [mark_name= "end",where= iter,left_gravity= %f];
    //  Add scrolling timeout.  
    y = g_timeout_add (50, scroll_to_end, list(textview));
  else
    //  If we want to scroll to the bottom, but not scroll horizontally,
    // then an end mark won't do the job. Just create a mark so we can
    // use it with gtk_text_view_scroll_mark_onscreen, we'll position it
    // explicitly when needed. Use left gravity so the mark stays where
    // we put it after inserting new text.
    buffer.create_mark [mark_name = "scroll",  where= iter,left_gravity=%t];
    //  Add scrolling timeout.  
    y = g_timeout_add (100, scroll_to_bottom, list(textview));
  end
endfunction

  
  function remove_timeout (window, timeout)
  g_source_remove (timeout);
endfunction
  
  function create_text_view (hbox, to_end)

  swindow = gtk_scrolled_window_new ()
  hbox.pack_start[swindow, expand=%t, fill= %t, padding= 0];
  textview = gtk_text_view_new ();
  swindow.add[textview];

  textview.set_data[count=0];
  timeout = setup_scroll (textview, to_end);

  //  Remove the timeout in destroy handler, so we don't try to
  // scroll destroyed widget.
    
  textview.connect[ "destroy", remove_timeout, timeout];
endfunction

  
  window = gtk_window_new (type = GTK.WINDOW_TOPLEVEL);
  window.set_title[ "Automatic Scrolling"];
  // window.connect[ "destroy", gtk_widget_destroyed, &window];
  window.set_default_size[600, 400];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing = 6);
  hbox.set_homogeneous[%t];
  window.add[hbox];

  create_text_view (hbox, %t);
  create_text_view (hbox, %f);
  window.show_all[];
endfunction
