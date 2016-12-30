//  Pango/Rotated Text
//
// This demo shows how to use PangoCairo to draw rotated and transformed
// text.  The right pane shows a rotated GtkLabel widget.
//
// In both cases, a custom PangoCairo shape renderer is installed to draw
// a red heard using cairo drawing operations instead of the Unicode heart
// character.

text = "I ♥ GTK+";

function fancy_shape_renderer (cr, attr, do_path, data)
  printf("fancy_shape_renderer\n")
  xy=cairo_get_current_point (cr);
  cairo_translate (cr, x, y);
  cairo_scale (cr, attr.ink_rect.width  / PANGO_SCALE,attr.ink_rect.height / PANGO_SCALE);
  
  select attr.data 
   case 0x2665 
    //  U+2665 BLACK HEART SUIT  
    cairo_move_to (cr, .5, .0);
    cairo_line_to (cr, .9, -.4);
    cairo_curve_to (cr, 1.1, -.8, .5, -.9, .5, -.5);
    cairo_curve_to (cr, .5, -.9, -.1, -.8, .1, -.4);
    cairo_close_path (cr);
  end
  
  if ~do_path then 
    cairo_set_source_rgb (cr, 1., 0., 0.);
    cairo_fill (cr);
  end
endfunction 

function attrs =create_fancy_attr_list_for_layout (layout)
//  Get font metrics and prepare fancy shape size  
  context = layout.get_context[];
  metrics = context.get_metrics[layout.get_font_description[]];
  
  ascent = metrics.get_ascent[];
  logical_rect.x = 0;
  logical_rect.width = ascent;
  logical_rect.y = -ascent;
  logical_rect.height = ascent;
  ink_rect = logical_rect;
  
  //  Set fancy shape attributes for all hearts  
  attrs = pango_attr_list_new ();
  return;
  
  for p = text;
    attr = pango_attr_shape_new_with_data (ink_rect,
    logical_rect,
    GUINT_TO_POINTER (g_utf8_get_char (p)),
    NULL, NULL);
    attr.start_index = p - text;
    attr.end_index = attr.start_index + strlen (HEART);
    pango_attr_list_insert (attrs, attr);
  end
endfunction 

function y= rotated_text_draw (widget, cr, data)
  printf("rotated_text_draw\n");
  RADIUS= 150
  N_WORDS= 5
  FONT= "Serif 18"
  //  Create a cairo context and set up a transformation matrix so that the user
  // space coordinates for the centered square where we draw are [-RADIUS, RADIUS],
  // [-RADIUS, RADIUS].
  // We first center, then change the scale.  
  
  width = widget.get_allocated_width [];
  height = widget.get_allocated_height [];
  device_radius = min (width, height) / 2.;
  cairo_translate (cr,
                   device_radius + (width - 2 * device_radius) / 2,
                   device_radius + (height - 2 * device_radius) / 2);
  cairo_scale (cr, device_radius / RADIUS, device_radius / RADIUS);

  //  Create and a subtle gradient source and use it. 
  if %f then 
    pattern = cairo_pattern_create_linear (-RADIUS, -RADIUS, RADIUS, RADIUS);
    cairo_pattern_add_color_stop_rgb (pattern, 0., .5, .0, .0);
    cairo_pattern_add_color_stop_rgb (pattern, 1., .0, .0, .5);
    cairo_set_source (cr, pattern);
  end
  //  Create a PangoContext and set up our shape renderer  
  context = widget.create_pango_context[];
  // pango_cairo_context_set_shape_renderer (context, fancy_shape_renderer);
  
  //  Create a PangoLayout, set the text, font, and attributes  
  layout = pango_layout_new (context);
  layout.set_text[ text];
  desc = pango_font_description_new(FONT);
  layout.set_font_description[desc];
  
  //attrs = create_fancy_attr_list_for_layout (layout);
  //pango_layout_set_attributes (layout, attrs);
    
  //  Draw the layout N_WORDS times in a circle  
  for i = 1:N_WORDS
    //  Inform Pango to re-layout the text with the new transformation matrix  
    pango_cairo_update_layout (cr, layout);
    wh= layout.get_pixel_size[]
    cairo_move_to (cr, - wh(1) / 2, - RADIUS * .9);
    cairo_show_text (cr, "x");
    pango_cairo_show_layout (cr, layout);
    //  Rotate for the next turn  
    cairo_rotate (cr, %pi*2 / N_WORDS);
  end
  y=%f
endfunction 

function window = demo_rotated_text (do_widget)
  RADIUS= 150
  N_WORDS= 5
  FONT= "Serif 18"

  window = gtk_window_new (type = GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then window.set_screen[ do_widget.get_screen []];end
  window.set_title[ "Rotated Text"];
  window.set_default_size[4 * RADIUS, 2 * RADIUS];
  // window.connect[ "destroy", gtk_widget_destroyed, window);

  box = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing=0);
  box.set_homogeneous[%t];
  window.add[box];

  //  Add a drawing area  
  drawing_area = gtk_drawing_area_new ();
  box.add[drawing_area];
  context= drawing_area.get_style_context []
  context.add_class ["view"]; // GTK.STYLE_CLASS_VIEW);

  drawing_area.connect[ "draw", rotated_text_draw];
  
  //  And a label  
  label = gtk_label_new(str=str=text);
  box.add[label];
  label.set_angle[45];
  //  Set up fancy stuff on the label  
  layout = label.get_layout [];
  desc = pango_font_description_new(FONT);
  layout.set_font_description[desc];
  context= layout.get_context[];
  //pango_cairo_context_set_shape_renderer(context, fancy_shape_renderer);
  attrs = create_fancy_attr_list_for_layout (layout);
  //label.set_attributes[attrs];
  window.show_all[];
endfunction

