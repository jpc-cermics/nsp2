// Drawing with cairo 

function demo_cairo()

  function rep=on_draw (widget,cr)
// Draw a red, green, and blue circle equally spaced inside
// the larger circle of radius r at (xc, yc)

  function draw_3circles (cr, xc, yc, radius, alpha)
    subradius = radius * (2 / 3. - 0.1);
    cairo_set_source_rgba (cr, 1., 0., 0., alpha);
    oval_path (cr,
    xc + radius / 3. * cos (%pi * (0.5)),
    yc - radius / 3. * sin (%pi * (0.5)),
    subradius, subradius);
    cairo_fill (cr);

    cairo_set_source_rgba (cr, 0., 1., 0., alpha);
    oval_path (cr,
    xc + radius / 3. * cos (%pi * (0.5 + 2/.3)),
    yc - radius / 3. * sin (%pi * (0.5 + 2/.3)),
    subradius, subradius);
    cairo_fill (cr);

    cairo_set_source_rgba (cr, 0., 0., 1., alpha);
    oval_path (cr,
    xc + radius / 3. * cos (%pi * (0.5 + 4/.3)),
    yc - radius / 3. * sin (%pi * (0.5 + 4/.3)),
    subradius, subradius);
    cairo_fill (cr);
  endfunction
  
  function oval_path (cr, xc, yc, xr, yr)
    cairo_save (cr);
    cairo_translate (cr, xc, yc);
    cairo_scale (cr, 1.0, yr / xr);
    cairo_move_to (cr, xr, 0.0);
    cairo_arc (cr, 0, 0, xr, 0, 2 * %pi)
    cairo_close_path (cr);
    cairo_restore (cr);
  endfunction

  function fill_checks (cr, x, y, width, height)
    CHECK_SIZE=32
    cairo_rectangle (cr, x, y, width, height);
    cairo_set_source_rgb (cr, 0.4, 0.4, 0.4);
    cairo_fill (cr);
    // Only works for CHECK_SIZE a power of 2
    for j=(iand(x, -CHECK_SIZE)):CHECK_SIZE:height
      i = iand( y , (-CHECK_SIZE));
      for i=(iand( y , (-CHECK_SIZE))):CHECK_SIZE:width;
	if modulo(i / CHECK_SIZE + j / CHECK_SIZE,2) == 0 then
	  cairo_rectangle (cr, i, j, CHECK_SIZE, CHECK_SIZE);
	end
      end
    end
    cairo_set_source_rgb (cr, 0.7, 0.7, 0.7);
    cairo_fill (cr);
  endfunction
  
  // Fill the background
  width = widget.get_allocated_width[];
  height = widget.get_allocated_height[];
  radius = 0.5 * min(width,height) - 10;
  xc = width / 2.;
  yc = height / 2.;
  overlay = cairo_surface_create_similar (cairo_get_target (cr),
					  CAIRO.CONTENT_COLOR_ALPHA,
					  width, height);
  punch = cairo_surface_create_similar (cairo_get_target (cr),
					CAIRO.CONTENT_ALPHA,
					width, height);
  circles = cairo_surface_create_similar (cairo_get_target (cr),
					  CAIRO.CONTENT_COLOR_ALPHA,
					  width, height);
  fill_checks (cr, 0, 0, width, height);
  // Draw a black circle on the overlay
  overlay_cr = cairo_create (overlay);
  cairo_set_source_rgb (overlay_cr, 0., 0., 0.);
  oval_path (overlay_cr, xc, yc, radius, radius);
  cairo_fill (overlay_cr);
  // Draw 3 circles to the punch surface, then cut
  // that out of the main circle in the overlay
  punch_cr = cairo_create (punch);
  draw_3circles (punch_cr, xc, yc, radius, 1.0);
  cairo_destroy (punch_cr);

  cairo_set_operator (overlay_cr, CAIRO.OPERATOR_DEST_OUT);
  cairo_set_source_surface (overlay_cr, punch, 0, 0);
  cairo_paint (overlay_cr);

  // Now draw the 3 circles in a subgroup again
  // at half intensity, and use OperatorAdd to join up
  // without seams.
  circles_cr = cairo_create (circles);
  cairo_set_operator (circles_cr, CAIRO.OPERATOR_OVER);
  draw_3circles (circles_cr, xc, yc, radius, 0.5);
  cairo_destroy (circles_cr);

  cairo_set_operator (overlay_cr, CAIRO.OPERATOR_ADD);
  cairo_set_source_surface (overlay_cr, circles, 0, 0);
  cairo_paint (overlay_cr);

  cairo_destroy (overlay_cr);

  cairo_set_source_surface (cr, overlay, 0, 0);
  cairo_paint (cr);

  cairo_surface_destroy (overlay);
  cairo_surface_destroy (punch);
  cairo_surface_destroy (circles);
  rep=%f
endfunction
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_default_size[400, 400];
  window.set_title["cairo: Knockout Groups"];
  darea = gtk_drawing_area_new ();
  window.add[darea];
  darea.connect["draw",on_draw];
  window.show_all[];
endfunction
