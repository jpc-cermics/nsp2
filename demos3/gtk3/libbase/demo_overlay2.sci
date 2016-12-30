// Overlay/Decorative Overlay
//
// Another example of an overlay with some decorative
// and some interactive controls.

function window = demo_overlay2 (do_widget)

  function margin_changed (adjustment, L)
    text=L(1);tag=L(2);
    value =adjustment.get_value[];
    text.set_left_margin[value];
    tag.set_property[ "pixels-above-lines",value];
  endfunction
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_default_size[500, 510];
  window.set_title ["Decorative Overlay"];

  overlay = gtk_overlay_new ();
  sw = gtk_scrolled_window_new ();
  sw.set_policy[GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC];
  text = gtk_text_view_new ();
  buffer = text.get_buffer [];

  buffer.set_text[ "Dear diary..."];

  tag = buffer.create_tag["top-margin", pixels_above_lines = 0];
  start = buffer.get_start_iter[];
  bend = start;
  bend.forward_word_end[];
  buffer.apply_tag[tag, start, bend];

  window.add[overlay];
  overlay.add[sw];
  sw.add[text];

  //window.connect[ "destroy", gtk_widget_destroyed, &window);
  fname = getenv('NSP')+'/demos3/gtk3/libbase/demo_overlay2/decor1.png";
  image = gtk_image_new('file',fname);
  overlay.add_overlay[ image];
  overlay.set_overlay_pass_through[image, %t];
  image.set_halign[GTK.ALIGN_START];
  image.set_valign[GTK.ALIGN_START];

  fname = getenv('NSP')+'/demos3/gtk3/libbase/demo_overlay2/decor2.png";
  image = gtk_image_new('file',fname);
  overlay.add_overlay[ image];
  overlay.set_overlay_pass_through[image, %t];
  image.set_halign[GTK.ALIGN_END];
  image.set_valign[GTK.ALIGN_END];
  
  adjustment = gtk_adjustment_new (  value=0, lower=0, upper=100, step_incr=1, page_incr=1, page_size=0);
  adjustment.connect[ "value-changed",margin_changed, list(text,tag)];

  scale = gtk_scale_new (GTK.ORIENTATION_HORIZONTAL, adjustment=adjustment);
  scale.set_draw_value[%f];
  scale.set_size_request[ 120, -1];
  scale.set_margin_start[20];
  scale.set_margin_end[20];
  scale.set_margin_bottom[20];
  overlay.add_overlay[scale];
  scale.set_halign[GTK.ALIGN_START];
  scale.set_valign[GTK.ALIGN_END];
  scale.set_tooltip_text[ "Margin"];

  adjustment.set_value[100];
  overlay.show_all[];
  window.show[];
endfunction

