// Translated to nsp by J.Ph Chancelier
// from:
// Copyright (C) 1998 Cesar Miquel, Shawn T. Amundson, Mattias Grönlund
// Copyright (C) 2000 Tony Gale
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

// XXXX implementation is not finished: (See set_detail_func in gtk.c) 
// the data associated to calendar.set_detail_func[calendar_detail_cb];
// is not cleared.

function demo_calendar()

  window = gtk_window_new ();// GTK.WINDOW_TOPLEVEL);
  window.set_title[ "GtkCalendar Example"];
  window.set_border_width[12];
  //window.connect[ "destroy",   gtk_main_quit];
  // window.connect[ "delete-event", gtk_false];
  
  hpaned = gtk_paned_new (GTK.ORIENTATION_HORIZONTAL);
  //  Calendar widget  
  calendar = gtk_calendar_new ();
  frame = create_frame ("<b>Calendar</b>", calendar, GTK.ALIGN_CENTER, GTK.ALIGN_CENTER);
  hpaned.pack1[ frame,resize= %t, shrink=%f];
  
  calendar.set_data[window=calendar,details_table=hash(10)];
  calendar.mark_day[19];
  calendar.connect[ "month_changed", calendar_month_changed,calendar];
  calendar.connect[ "day_selected", calendar_day_selected,calendar];
  calendar.connect[ "day_selected_double_click", calendar_day_selected_double_click,calendar];
  calendar.connect[ "prev_month", calendar_prev_month,calendar];
  calendar.connect[ "next_month", calendar_next_month,calendar];
  calendar.connect[ "prev_year", calendar_prev_year, calendar];
  calendar.connect[ "next_year", calendar_next_year, calendar];
  
  rpane = gtk_box_new (GTK.ORIENTATION_VERTICAL);// DEF_PAD_SMALL;
  hpaned.pack2[rpane, resize=%f,shrink= %f];
  
  //  Build the right font-button  

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL);// DEF_PAD_SMALL);
  frame = create_frame ("<b>Options</b>", vbox, GTK.ALIGN_FILL, GTK.ALIGN_CENTER);
  rpane.pack_start[frame,expand= %f, fill=%t, padding=0];
  size_g = gtk_size_group_new (GTK.SIZE_GROUP_HORIZONTAL);
  
  //gtk_style_context_get (context, GTK.STATE_FLAG_NORMAL,GTK.STYLE_PROPERTY_FONT,font_desc);
  //replaced by get_property method 
  
  context = calendar.get_style_context [];
  // The GTK_STYLE_PROPERTY_VALUE can be replaced by "value" 
  // GTK_STYLE_PROPERTY_FONT -> "font"
  font_desc = context.get_property["font",GTK.STATE_FLAG_NORMAL];
  // font = pango_font_description_to_string (font_desc);
  font = font_desc.to_string[];
  button = gtk_font_button_new_with_font (font);
  button.connect["font-set", calendar_select_font, calendar];
  
  button= gtk_font_button_new();
    
  label = gtk_label_new(mnemonic="_Font:");
  label.set_mnemonic_widget[button];
  label.set_halign[GTK.ALIGN_START];
  label.set_valign[GTK.ALIGN_CENTER];
  size_g.add_widget[label];
  
  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL);//, DEF_PAD_SMALL);
  hbox.pack_start[label,expand= %f, fill=%t, padding=0];
  hbox.pack_start[button,expand= %f, fill=%t, padding=0];
  vbox.pack_start[hbox,expand= %f, fill=%t, padding=0];

  //  Build the width entry  

  button = gtk_spin_button_new_with_range (0, 127, 1);
  button.set_value[ calendar.get_detail_width_chars []];

  button.connect[ "value-changed", detail_width_changed, calendar];
  
  label = gtk_label_new(mnemonic="Details W_idth:");
  label.set_mnemonic_widget[button];
  label.set_halign[GTK.ALIGN_START];
  label.set_valign[GTK.ALIGN_CENTER];
  size_g.add_widget[label];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL);// DEF_PAD_SMALL);
  hbox.pack_start[label,expand= %f, fill=%t, padding=0];
  hbox.pack_start[button,expand= %f, fill=%t, padding=0];
  vbox.pack_start[hbox,expand= %f, fill=%t, padding=0];

  //  Build the height entry  

  button = gtk_spin_button_new_with_range (0, 127, 1);
  button.set_value[ calendar.get_detail_height_rows []];
  button.connect[ "value-changed",detail_height_changed,calendar];

  label = gtk_label_new(mnemonic ="Details H_eight:");
  label.set_mnemonic_widget[button];
  label.set_halign[GTK.ALIGN_START];
  label.set_valign[GTK.ALIGN_CENTER];
  size_g.add_widget[label];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL);// DEF_PAD_SMALL);
  hbox.pack_start[label,expand= %f, fill=%t, padding=0];
  hbox.pack_start[button,expand= %f, fill=%t, padding=0];
  vbox.pack_start[hbox,expand= %f, fill=%t, padding=0];

  //  Build the right details frame  

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL);// DEF_PAD_SMALL);
  frame = create_frame ("<b>Details</b>", vbox, GTK.ALIGN_FILL, GTK.ALIGN_FILL);
  rpane.pack_start[frame,expand= %f, fill=%t, padding=0];

  details = gtk_text_view_new();
  details_buffer = details.get_buffer [];
  details_changed = details_buffer.connect[ "changed", calendar_details_changed, calendar];
  calendar.set_data[details_buffer= details_buffer];
    
  scroller = gtk_scrolled_window_new();//NULL, NULL);
  scroller.add[details];

  scroller.set_shadow_type[ GTK.SHADOW_IN];
  scroller.set_policy[ GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC];

  vbox.pack_start[scroller,expand= %f, fill=%t, padding=0];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL);// DEF_PAD_SMALL);
  hbox.set_halign[GTK.ALIGN_START];
  hbox.set_valign[GTK.ALIGN_CENTER];
  vbox.pack_start[hbox,expand= %f, fill=%t, padding=0];

  button = gtk_button_new(mnemonic="Demonstrate _Details");
  button.connect[ "clicked",demonstrate_details, calendar];

  hbox.pack_start[button,expand= %f, fill=%t, padding=0];

  button = gtk_button_new(mnemonic="_Reset Details");

  button.connect["clicked", reset_details, calendar];

  hbox.pack_start[button,expand= %f, fill=%t, padding=0];

  toggle = gtk_check_button_new(mnemonic="_Use Details");
  toggle.connect[ "toggled", calendar_toggle_details, calendar];
  vbox.pack_start[toggle,expand= %f, fill=%t, padding=0];
  
  //  Build the Right frame with the flags in   

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=0);
  frame = create_expander ("<b>Flags</b>", vbox, GTK.ALIGN_FILL, GTK.ALIGN_CENTER);
  rpane.pack_start[frame,expand= %t, fill=%t, padding=0];
  
  // { %f, "Week Start _Monday" },
  
  flags = {{ %t,  "Show _Heading"      ,GTK.CALENDAR_SHOW_HEADING},
	   { %t,  "Show Day _Names"    ,GTK.CALENDAR_SHOW_DAY_NAMES},
	   { %f,  "No Month _Change"   ,GTK.CALENDAR_NO_MONTH_CHANGE},
	   { %t,  "Show _Week Numbers" ,GTK.CALENDAR_SHOW_WEEK_NUMBERS},
	   { %t,  "Show De_tails"      ,GTK.CALENDAR_SHOW_DETAILS}};
  
  settings=[];
  
  for i = 1:size(flags,'*')
    label = flags{i}{2};
    active = flags{i}{1};
    settings[i]= b2m(active);
    toggle = gtk_check_button_new(mnemonic=label);
    vbox.pack_start[toggle,expand= %f, fill=%t, padding=0];
    toggle.set_data[number=i];
    toggle.set_active[active];
    toggle.connect[ "toggled", calendar_toggle_flag, calendar];
  end
  
  calendar.set_data[settings=settings];
  calendar_set_flags(calendar,settings);
    
  // Build the Signal-event part.
  
  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL);// DEF_PAD_SMALL);
  vbox.set_homogeneous[%t];
  frame = create_frame ("<b>Signal Events</b>", vbox, GTK.ALIGN_FILL, GTK.ALIGN_CENTER);
  
  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing=3);
  vbox.pack_start[hbox,expand= %f, fill=%t, padding=0];
  label = gtk_label_new(str="Signal:");
  hbox.pack_start[label,expand= %f, fill=%t, padding=0];
  last_sig = gtk_label_new(str="");
  hbox.pack_start [ last_sig, expand=%f, fill=%t, padding=0];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing= 3);
  vbox.pack_start[hbox,expand= %f, fill=%t, padding=0];
  label = gtk_label_new(str="Previous signal:");
  hbox.pack_start[label,expand= %f, fill=%t, padding=0];
  prev_sig = gtk_label_new(str="");
  hbox.pack_start[ prev_sig,expand= %f, fill=%t, padding=0];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing= 3);
  vbox.pack_start[hbox,expand= %f, fill=%t, padding=0];
  label = gtk_label_new(str="Second previous signal:");
  hbox.pack_start[label,expand= %f, fill=%t, padding=0];
  prev2_sig = gtk_label_new(str="");
  hbox.pack_start [ prev2_sig,expand= %f, fill=%t, padding=0];
  
  calendar.set_data[prev_sig=prev_sig];
  calendar.set_data[prev2_sig=prev2_sig];
  calendar.set_data[last_sig=last_sig];
  
  // Glue everything together
  
  bbox = gtk_button_box_new (GTK.ORIENTATION_HORIZONTAL);
  bbox.set_layout[GTK.BUTTONBOX_END];

  button = gtk_button_new(label="Close");
  // button.connect[ "clicked", gtk_main_quit];
  bbox.add[button];

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL);// DEF_PAD_SMALL);

  vbox.pack_start[ hpaned,expand= %t,  fill=%t, padding=0];
  vbox.pack_start[ gtk_separator_new (GTK.ORIENTATION_HORIZONTAL),
                    expand=  %f, fill=%t, padding=0];
  vbox.pack_start[ frame, expand=%f, fill=%t, padding=0];
  vbox.pack_start[ gtk_separator_new (GTK.ORIENTATION_HORIZONTAL),expand=%f, fill=%t, padding=0];
  vbox.pack_start[ bbox,expand= %f, fill=%t, padding=0];

  window.add[vbox];

  button.set_can_default[%t];
  button.grab_default[];
  window.set_default_size[600,0];
  window.show_all[];
endfunction

function calendar_select_font (button, calendar)
// unchecked 
  window = calendar.get_window[];
  if window.check_data["css-provider"] then 
    provider = window.get_data["css-provider"];
  else
    provider = gtk_css_provider_new ();
    st = window.get_style_context[];
    st.add_provider[provider,GTK.STYLE_PROVIDER_PRIORITY_APPLICATION];
    window.set_data["css-provider", provider];
  end
  font = button.get_font_name [];
  data = sprintf ("GtkCalendar { font: %s; }", font);
  provider.load_from_data[data];
endfunction 

function calendar_set_detail (calendar, year, month, day, detail)
  key = sprintf("%04d-%02d-%02d", year, month + 1, day);
  H=calendar.get_data['details_table'];
  H(key) = detail;
  calendar.set_data[details_table=H];
  printf("In set detail: %s -> %s\n",key,detail);
endfunction

function str=calendar_get_detail (calendar, year, month, day)
  key = sprintf ("%04d-%02d-%02d", year, month + 1, day);
  H=calendar.get_data['details_table'];
  str = H.find[key,def=""];
endfunction 

function calendar_update_details (calendar)
  date = calendar.get_date[];
  detail = calendar_get_detail(calendar, date(1),date(2),date(3));
  details_buffer = calendar.get_data["details_buffer"];
  //g_signal_handler_block (data->details_buffer, data->details_changed);
  details_buffer.set_text[detail];
  //g_signal_handler_unblock (data->details_buffer, data->details_changed);
  //g_free (detail);
endfunction

function y=calendar_detail_cb (calendar, ymd)
  y= calendar_get_detail (calendar, ymd(1), ymd(2), ymd(3));
  key = sprintf ("%04d-%02d-%02d",  ymd(1), ymd(2), ymd(3));
  printf("Dans le callback avec %s-> ""%s""\n",key, y);
endfunction

function calendar_details_changed (buffer, calendar)
// callback entered when the textview is changed 
  iter_start = buffer.get_start_iter [];
  iter_end = buffer.get_end_iter[];
  date = calendar.get_date[];
  detail = buffer.get_text[iter_start,iter_end];
  calendar_set_detail(calendar, date(1),date(2),date(3),detail);
  calendar.queue_resize[];
endfunction 

function demonstrate_details (button, calendar)
  rainbow = [ "#900", "#980", "#390", "#095", "#059", "#309", "#908" ];
  date = calendar.get_date[];
  for day = 0:29
    detail = sprintf ("<span color=''%s''>yadda\\n(%04d-%02d-%02d)</span>",...
		      rainbow(modulo(day,7)+1), date(1),date(2), day);
    calendar_set_detail(calendar, date(1), date(2), day, detail);
  end
  calendar.queue_resize[];
  calendar_update_details (calendar);
endfunction 

function reset_details (button,calendar)
  calendar.set_data[details_table=hash(10)];
  calendar.queue_resize[];
  calendar_update_details (calendar);
endfunction 

function calendar_toggle_details (widget, calendar)
  if widget.get_active[] then 
    calendar.set_detail_func[calendar_detail_cb];
  else
    calendar.set_detail_func[];
  end
endfunction 

function expander=create_expander (caption, child, halign, valign)
  expander = gtk_expander_new ("");
  label = expander.get_label_widget [];
  frame.set_halign[halign];
  frame.set_valign[valign];
  label.set_markup[caption];
  expander.add[child];
endfunction 

function frame= create_frame (caption, child, halign, valign)
  frame = gtk_frame_new (label="");
  label = frame.get_label_widget [];
  frame.set_halign[halign];
  frame.set_valign[valign];
  frame.set_shadow_type[GTK.SHADOW_NONE];
  label.set_markup[caption];
  frame.add[child];
endfunction

function detail_width_changed (button, calendar)
  value = button.get_value [];
  calendar.set_detail_width_chars[value];
endfunction 

function detail_height_changed (button, calendar)
  value = button.get_value [];
  calendar.set_detail_height_rows[value];
endfunction

function str=calendar_date_to_string(calendar)
  date = calendar.get_date[]
  str=sprintf("%2.2d/%d/%d",date(2)+1,date(3),date(1));
endfunction

function calendar_set_signal_strings (sig_str,calendar)
  prev_sig=calendar.get_data['prev_sig'];
  prev2_sig=calendar.get_data['prev2_sig'];
  last_sig=calendar.get_data['last_sig'];
  prev2_sig.set_text[prev_sig.get_text[]];
  prev_sig.set_text[last_sig.get_text[]];
  last_sig.set_text[sig_str];
endfunction

function calendar_month_changed (calendar)
  str="month_changed: " + calendar_date_to_string(calendar);
  calendar_set_signal_strings (str,calendar);
endfunction

function calendar_day_selected (calendar)
  str= "day_selected: " + calendar_date_to_string (calendar)
  calendar_set_signal_strings (str, calendar);
endfunction

function calendar_day_selected_double_click (calendar)
  str= "day_selected_double_click: "  + calendar_date_to_string (calendar)
  calendar_set_signal_strings (str, calendar);
  date = calendar.get_date[]
  if calendar.get_day_is_marked[date(3)] then
    calendar.unmark_day[date(3)]
  else
    calendar.mark_day[date(3)]
  end
endfunction

function calendar_prev_month (calendar)
  str="prev_month: " + calendar_date_to_string(calendar);
  calendar_set_signal_strings (str,calendar);
endfunction

function calendar_next_month(calendar)
  str="next_month: " + calendar_date_to_string(calendar);
  calendar_set_signal_strings (str,calendar);
endfunction

function calendar_prev_year(calendar)
  str="prev_year: " + calendar_date_to_string(calendar);
  calendar_set_signal_strings (str,calendar);
endfunction

function calendar_next_year(calendar)
  str="next_year: " + calendar_date_to_string(calendar);
  calendar_set_signal_strings (str,calendar);
endfunction

function calendar_set_flags(calendar,settings)
  flags=[ GTK.CALENDAR_SHOW_HEADING;
	  GTK.CALENDAR_SHOW_DAY_NAMES;
	  GTK.CALENDAR_NO_MONTH_CHANGE;
	  GTK.CALENDAR_SHOW_WEEK_NUMBERS;
	  GTK.CALENDAR_SHOW_DETAILS];
  options = sum(flags .* settings(:));
  calendar.set_display_options[options];
endfunction

function calendar_toggle_flag(toggle, calendar)
  settings=calendar.get_data['settings'];
  i = toggle.get_data["number"];
  settings(i) =b2m( ~ m2b(settings(i)));
  printf("%d --> %d\n",i,settings(i))
  calendar.set_data[settings=settings]
  calendar_set_flags(calendar,settings);
endfunction


