//  Overlay/Transparency
//
// Use transparent background on GdkWindows to create a shadow effect on a GtkOverlay widget.

function window = demo_transparent (do_widget)

  function y = draw_callback (widget,cr, data)
  // the drawing callback 
    function draw_shadow_box (cr, rect, radius, transparency)
      
      x0 = rect.x;
      x1 = rect.x + radius;
      x2 = rect.x + rect.width - radius;
      x3 = rect.x + rect.width;

      y0 = rect.y;
      y1 = rect.y + radius;
      y2 = rect.y + rect.height - radius;
      y3 = rect.y + rect.height;

      //  Fill non-border part  
      cairo_set_source_rgba (cr, 0, 0, 0, transparency);
      cairo_rectangle (cr, x1, y1, x2 - x1, y2 - y1);
      cairo_fill (cr);

      //  Upper border  
      pattern = cairo_pattern_create_linear (0, y0, 0, y1);

      cairo_pattern_add_color_stop_rgba (pattern, 0.0, 0.0, 0, 0, 0.0);
      cairo_pattern_add_color_stop_rgba (pattern, 1.0, 0.0, 0, 0, transparency);

      cairo_set_source (cr, pattern);
      cairo_pattern_destroy (pattern);

      cairo_rectangle (cr, x1, y0, x2 - x1, y1 - y0);
      cairo_fill (cr);

      //  Bottom border  
      pattern = cairo_pattern_create_linear (0, y2, 0, y3);

      cairo_pattern_add_color_stop_rgba (pattern, 0.0, 0.0, 0, 0, transparency);
      cairo_pattern_add_color_stop_rgba (pattern, 1.0, 0.0, 0, 0, 0.0);

      cairo_set_source (cr, pattern);
      cairo_pattern_destroy (pattern);

      cairo_rectangle (cr,x1, y2, x2 - x1, y3 - y2);
      cairo_fill (cr);

      //  Left border  
      pattern = cairo_pattern_create_linear (x0, 0, x1, 0);

      cairo_pattern_add_color_stop_rgba (pattern, 0.0, 0.0, 0, 0, 0.0);
      cairo_pattern_add_color_stop_rgba (pattern, 1.0, 0.0, 0, 0, transparency);

      cairo_set_source (cr, pattern);
      cairo_pattern_destroy (pattern);

      cairo_rectangle (cr, x0, y1, x1 - x0, y2 - y1);
      cairo_fill (cr);

      //  Right border  
      pattern = cairo_pattern_create_linear (x2, 0, x3, 0);

      cairo_pattern_add_color_stop_rgba (pattern, 0.0, 0.0, 0, 0, transparency);
      cairo_pattern_add_color_stop_rgba (pattern, 1.0, 0.0, 0, 0, 0.0);

      cairo_set_source (cr, pattern);
      cairo_pattern_destroy (pattern);

      cairo_rectangle (cr, x2, y1, x3 - x2, y2 - y1);
      cairo_fill (cr);

      //  NW corner  
      pattern = cairo_pattern_create_radial (x1, y1, 0, x1, y1, radius);

      cairo_pattern_add_color_stop_rgba (pattern, 0.0, 0.0, 0, 0, transparency);
      cairo_pattern_add_color_stop_rgba (pattern, 1.0, 0.0, 0, 0, 0.0);

      cairo_set_source (cr, pattern);
      cairo_pattern_destroy (pattern);

      cairo_rectangle (cr, x0, y0, x1 - x0, y1 - y0);
      cairo_fill (cr);

      //  NE corner  
      pattern = cairo_pattern_create_radial (x2, y1, 0, x2, y1, radius);

      cairo_pattern_add_color_stop_rgba (pattern, 0.0, 0.0, 0, 0, transparency);
      cairo_pattern_add_color_stop_rgba (pattern, 1.0, 0.0, 0, 0, 0.0);

      cairo_set_source (cr, pattern);
      cairo_pattern_destroy (pattern);

      cairo_rectangle (cr, x2, y0, x3 - x2, y1 - y0);
      cairo_fill (cr);

      //  SW corner  
      pattern = cairo_pattern_create_radial (x1, y2, 0, x1, y2, radius);

      cairo_pattern_add_color_stop_rgba (pattern, 0.0, 0.0, 0, 0, transparency);
      cairo_pattern_add_color_stop_rgba (pattern, 1.0, 0.0, 0, 0, 0.0);

      cairo_set_source (cr, pattern);
      cairo_pattern_destroy (pattern);

      cairo_rectangle (cr, x0, y2, x1 - x0, y3 - y2);
      cairo_fill (cr);

      //  SE corner  
      pattern = cairo_pattern_create_radial (x2, y2, 0, x2, y2, radius);

      cairo_pattern_add_color_stop_rgba (pattern, 0.0, 0.0, 0, 0, transparency);
      cairo_pattern_add_color_stop_rgba (pattern, 1.0, 0.0, 0, 0, 0.0);

      cairo_set_source (cr, pattern);
      cairo_pattern_destroy (pattern);

      cairo_rectangle (cr, x2, y2, x3 - x2, y3 - y2);
      cairo_fill (cr);
    endfunction

    SHADOW_OFFSET_X=7
    SHADOW_OFFSET_Y=7
    SHADOW_RADIUS=5
    rect = widget.get_allocation[];
    rect1 = gdk_rectangle_new([rect.x+ SHADOW_OFFSET_X, rect.y+ SHADOW_OFFSET_Y,...
		    rect.width-SHADOW_OFFSET_X, rect.height- SHADOW_OFFSET_Y]);
    draw_shadow_box (cr, rect1, SHADOW_RADIUS, 0.4);
    y = %f;
  endfunction 
  
  window = gtk_window_new (type = GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then window.set_screen[do_widget.get_screen []];end
  window.set_default_size[450, 450];
  // window.connect[ "destroy", gtk_widget_destroyed, &window);

  window.set_title[ "Transparency"];
  window.set_border_width[0];
  
  view = gtk_text_view_new ();
  sw = gtk_scrolled_window_new ();
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  sw.add[view];

  overlay = gtk_overlay_new ();
  overlay.add[sw];
  window.add[overlay];

  entry = gtk_entry_new ();
  provider = gtk_css_provider_new ();
  SHADOW_OFFSET_X=7
  SHADOW_OFFSET_Y=7

  css = sprintf ("* { border-width: 0px %dpx %dpx 0px; }",
  SHADOW_OFFSET_X, SHADOW_OFFSET_Y);
  provider.load_from_data[css,-1];
  context = entry.get_style_context []
  context.add_provider[provider, 600];//GTK.STYLE_PROVIDER_PRIORITY_APPLICATION];
  entry.connect[ "draw", draw_callback];
  overlay.add_overlay[entry];
  entry.set_halign[GTK.ALIGN_CENTER];
  entry.set_valign[GTK.ALIGN_START];
  overlay.show_all[];
  window.show[];
endfunction

