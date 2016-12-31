// testtooltips.c: Test application for GTK+ >= 2.12 tooltips code
//
// Copyright (C) 2006-2007  Imendio AB
// Contact: Kristian Rietveld <kris@imendio.com>
//
// This work is provided "as is"; redistribution and modification
// in whole or in part, in any medium, physical or electronic is
// permitted without restriction.
//
// This work is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// In no event shall the authors or contributors be liable for any
// direct, indirect, incidental, special, exemplary, or consequential
// damages (including, but not limited to, procurement of substitute
// goods or services; loss of use, data, or profits; or business
// interruption) however caused and on any theory of liability, whether
// in contract, strict liability, or tort (including negligence or
// otherwise) arising in any way out of the use of this software, even
// if advised of the possibility of such damage.


function demo_tooltips() 

  function y=query_tooltip_cb (widget, x, y, keyboard_tip, tooltip, data)
    tooltip.set_markup[ widget.get_label []];
    tooltip.set_icon_from_icon_name [ "edit-delete", GTK.ICON_SIZE_MENU];
    y=%t;
  endfunction 

  function y=query_tooltip_custom_cb (widget, x, y, keyboard_tip, tooltip, data)
    function y=draw_tooltip (widget,cr, unused)
      cairo_set_source_rgb (cr, 0, 0, 1);
      cairo_paint (cr);
      y=%f
    endfunction
    window = widget.get_tooltip_window [];
    window.set_app_paintable[%t];
    window.connect[ "draw", draw_tooltip];
    y=%t;
  endfunction

  function y=query_tooltip_text_view_cb (widget,x, y, keyboard_tip, tooltip, data)
    tag = data;
    text_view = widget;
    buffer = text_view.get_buffer [];
    if keyboard_tip  then 
      // g_object_get (buffer, "cursor-position", &offset, NULL);
      pos = buffer.get_property[   "cursor-position"];
      iter = buffer.get_iter_at_offset[pos];
    else
      bcoords=text_view.window_to_buffer_coords[GTK.TEXT_WINDOW_TEXT,x, y];
      [iter,trailing]=text_view.get_iter_at_position[ bcoords(1), bcoords(2)];
    end
    if iter.has_tag[tag] then 
      tooltip.set_text["Tooltip on text tag"];
    else
      y=%f;return;
    end
    y=%t;
  endfunction

  function y=query_tooltip_tree_view_cb (widget, x, y,keyboard_tip, tooltip, data)
    tree_view = widget;
    model = tree_view.get_model [];
    [ok,x,y,iter,path]=tree_view.get_tooltip_context[x,y,keyboard_tip,model];
    if ~ok then y=%f;return;end 
    // gtk_tree_model_get (model, &iter, 0, &tmp, -1);
    str = model.get_value[iter,0];
    pathstring = path.to_string[];
    str = sprintf ("<b>Path %s:</b> %s",pathstring,str);
    tooltip.set_markup[str];
    tree_view.set_tooltip_row[tooltip, path];
    y=%t;
  endfunction 

  function model = create_model ()
    names=["File Manager"
	   "Gossip"
	   "System Settings"
	   "The GIMP"
	   "Terminal"
	   "Word Processor"]
    model = gtk_tree_store_new(list(names));
  endfunction

  function selection_changed_cb (selection, tree_view)
    tree_view.trigger_tooltip_query[];
  endfunction 

  function y=query_tooltip_drawing_area_cb (widget, x, y, keyboard_tip, tooltip, rects)
    if keyboard_tip then 
      y=%f;return;
    end
    for i =1:size(rects,'*')
      rect= rects{i}; rx=rect{1};ry=rect{2};[r,g,b]=rect{3:5},str=rect{6};
      if rx < x && x < rx + 50 && ry < y && y < ry + 50 then 
	tooltip.set_markup[ str];
	y=%t;
	return;
      end
    end
    y=%f;
  endfunction 

  function y=drawing_area_draw (drawing_area, cr, rects)
    cairo_set_source_rgb (cr, 1.0, 1.0, 1.0);
    cairo_paint (cr);
    for i =1:size(rects,'*')
      rect= rects{i}; x=rect{1};y=rect{2};[r,g,b]=rect{3:5};
      cairo_rectangle (cr, x,y, 50, 50);
      cairo_set_source_rgb (cr, r,g,b);
      cairo_stroke (cr);
      cairo_rectangle (cr, x,y, 50, 50);
      cairo_set_source_rgba (cr, r,g,b, 0.5);
      cairo_fill (cr);
    end
    y=%f
  endfunction

  function y=query_tooltip_label_cb (widget, x, y, keyboard_tip, tooltip, data)
    custom = data;
    tooltip.set_custom[custom];
    y=%t;
  endfunction

  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title[ "Tooltips test"];
  window.set_border_width[10];
  // window.connect[ "delete_event",  gtk_main_quit, NULL);
  box = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=3);
  window.add[box];
  
  //  A check button using the tooltip-markup property  
  button = gtk_check_button_new(label="This one uses the tooltip-markup property");
  button.set_tooltip_text[ "Hello, I am a static tooltip."];
  box.pack_start[button, fill= %f, padding= 0];
  
  text = button.get_tooltip_text [];
  markup = button.get_tooltip_markup [];

  // g_assert (g_str_equal ("Hello, I am a static tooltip.", text));
  // g_assert (g_str_equal ("Hello, I am a static tooltip.", markup));
  
  //  A check button using the query-tooltip signal  
  button = gtk_check_button_new(label="I use the query-tooltip signal");
  // g_object_set (button, "has-tooltip", %t, NULL);
  button.set_property["has-tooltip", %t];
  button.connect[ "query-tooltip", query_tooltip_cb];
  box.pack_start[button, fill= %f, padding= 0];

  //  A label  
  button = gtk_label_new(str="I am just a label");
  button.set_selectable[%f];
  button.set_tooltip_text[ "Label & and tooltip"];
  box.pack_start[button, fill= %f, padding= 0];

  text = button.get_tooltip_text [];
  markup = button.get_tooltip_markup [];
  // g_assert (g_str_equal ("Label & and tooltip", text));
  // g_assert (g_str_equal ("Label &amp; and tooltip", markup));
  
  //  A selectable label  
  button = gtk_label_new(str="I am a selectable label");
  button.set_selectable[%t];
  button.set_tooltip_markup[ "<b>Another</b> Label tooltip"];
  box.pack_start[button, fill= %f, padding= 0];

  text = button.get_tooltip_text [];
  markup = button.get_tooltip_markup [];
  //g_assert (g_str_equal ("Another Label tooltip", text));
  // g_assert (g_str_equal ("<b>Another</b> Label tooltip", markup));

  //  Another one, with a custom tooltip window  
  button = gtk_check_button_new(label="This one has a custom tooltip window!");
  box.pack_start[button, fill= %f, padding= 0];

  tooltip_window = gtk_window_new (type=GTK.WINDOW_POPUP);
  tooltip_button = gtk_label_new(str="blaat!");
  tooltip_window.add[tooltip_button];
  tooltip_button.show[];

  button.set_tooltip_window[tooltip_window];
  button.connect[ "query-tooltip", query_tooltip_custom_cb];
  button.set_property["has-tooltip", %t];
  
  //  An insensitive button  
  button = gtk_button_new(label="This one is insensitive");
  button.set_sensitive[%f];
  button.set_property["tooltip-text","Insensitive!"];
  box.pack_start[button, fill= %f, padding= 0];

  //  Testcases from Kris without a tree view don't exist.  
  tree_view = gtk_tree_view_new_with_model (create_model ());
  tree_view.set_size_request[200, 240];
  
  renderer = gtk_cell_renderer_text_new ();
  col = gtk_tree_view_column_new(title="Col 0",renderer=renderer,attrs=hash(text= 0));
  tree_view.append_column[col];
  
  // tree_view.insert_column_with_attributes 0, "Test",
  // 					       gtk_cell_renderer_text_new (),
  // 					       "text", 0,
  // 					       NULL);

  tree_view.set_property["has-tooltip", %t];
  tree_view.connect[ "query-tooltip", query_tooltip_tree_view_cb];
  selection =tree_view.get_selection [];
  selection.connect["changed", selection_changed_cb, tree_view];
  //  Set a tooltip on the column  
  column = tree_view.get_column[0];
  column.set_clickable[%t];
  button=column.get_button [];
  button.set_property["tooltip-text", "Header"];
  box.pack_start[tree_view, fill= %f, padding= 2];
  
  //  And a text view for Matthias  
  buffer = gtk_text_buffer_new ();
  iter = buffer.get_end_iter[];
  buffer.insert[iter, "Hello, the text "];

  tag = buffer.create_tag["bold", weight= PANGO.WEIGHT_BOLD];
  // tag = buffer.create_tag["bold"];
  // tag.set_property[ "weight", PANGO.WEIGHT_BOLD];
  iter = buffer.get_end_iter[];
  buffer.insert_with_tags[iter, "in bold",tag];
  // buffer.insert_with_tags_by_name[iter, "in bold", "bold"];
  
  iter = buffer.get_end_iter[];
  buffer.insert[iter," has a tooltip!"];

  text_view = gtk_text_view_new_with_buffer (buffer);
  text_view.set_size_request[200, 50];

  text_view.set_property["has-tooltip", %t];
  text_view.connect[ "query-tooltip", query_tooltip_text_view_cb, tag];
  box.pack_start[text_view, fill= %f, padding= 2];

  //  Drawing area  
  drawing_area = gtk_drawing_area_new ();
  drawing_area.set_size_request[320, 240];
  drawing_area.set_property["has-tooltip", %t];
  
  rects = {{ 10, 10, 0.0, 0.0, 0.9, "Blue box!"},
	   { 200, 170, 1.0, 0.0, 0.0, "Red thing"},
	   { 100, 50, 0.8, 0.8, 0.0, "Yellow thing"}};
  
  drawing_area.connect[ "draw", drawing_area_draw,rects];
  drawing_area.connect[ "query-tooltip", query_tooltip_drawing_area_cb,rects];
  box.pack_start[drawing_area, fill= %f, padding= 2];

  button = gtk_label_new(str="Custom tooltip I");
  label = gtk_label_new(str="See, custom");
  label.set_property["has-tooltip", %t];
  button.connect[ "query-tooltip",  query_tooltip_label_cb, label];
  box.pack_start[button,  fill= %f, padding= 2];

  button = gtk_label_new(str="Custom tooltip II");
  label = gtk_label_new(str="See, custom, too");
  button.set_property["has-tooltip", %t];
  box.pack_start[button,  fill= %f, padding= 2];
  button.connect[ "query-tooltip", query_tooltip_label_cb, label];
  window.show_all[];
endfunction



