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
 
function  demo_calendar()

  TM_YEAR_BASE=1900
  
  // names of toggle buttons 
  
  flags=[ "Show Heading"
	  "Show Day Names"
	  "No Month Change"
	  "Show Week Numbers"
	  "Week Start Monday"]; 
  
  window = gtkwindow_new();
  window.set_title["GtkCalendar Example"]
  window.set_border_width[  5]
  window.set_resizable[%f]
  vbox = gtkvbox_new(homogeneous=%f, spacing= 10);
  window.add[vbox]

  //
  // The top part of the window, Calendar, flags and fontsel.
  //

  hbox = gtkhbox_new(homogeneous=%f,spacing=10);
  vbox.pack_start[ hbox,expand=%t,fill=%t,padding=10]
  hbbox = gtkhbuttonbox_new();
  hbox.pack_start[ hbbox,expand=%f,fill=%f,padding=10]
  hbbox.set_layout[  GTK.BUTTONBOX_SPREAD]
  hbbox.set_spacing[  5]

  // Calendar widget */
  frame = gtkframe_new(label="Calendar");
  hbbox.pack_start[ frame,expand=%f,fill=%t,padding=10]
  calendar=gtkcalendar_new();
  
  // we store data in calendar 
  settings=0*ones(1,5);
  calendar.set_data[window=calendar,settings=settings]
  calendar_set_flags(calendar,settings);
  calendar.mark_day[19];	
  frame.add[calendar]

  calendar.connect["month_changed",calendar_month_changed]
  calendar.connect["day_selected", calendar_day_selected]
  calendar.connect["day_selected_double_click",calendar_day_selected_double_click];
  calendar.connect["prev_month", calendar_prev_month]
  calendar.connect["next_month", calendar_next_month]
  calendar.connect["prev_year", calendar_prev_year]
  calendar.connect["next_year", calendar_next_year]

  separator = gtkvseparator_new ();
  hbox.pack_start[ separator,expand=%f,fill=%t,padding=0]

  vbox2 = gtkvbox_new(homogeneous=%f,spacing=10);
  hbox.pack_start[ vbox2,expand=%f,fill=%f,padding=10]
  
  // Build the Right frame with the flags in */ 

  frame = gtkframe_new(label="Flags");
  vbox2.pack_start[ frame,expand=%t,fill=%t,padding=10]
  vbox3 = gtkvbox_new(homogeneous=%t, spacing=5);
  frame.add[  vbox3]

  for i=1:5 
    toggle = gtkcheckbutton_new(label=flags[i]);
    toggle.set_data[number=i];
    toggle.connect[ "toggled", calendar_toggle_flag, list(calendar)]
    vbox3.pack_start[ toggle,expand=%t,fill=%t,padding=0]
  end
      
  // Build the right font-button */ 
  button = gtkbutton_new(label="Font...");
  //button.connect[ "clicked", calendar_select_font, list(calendar_data)]
  vbox2.pack_start[ button,expand=%f,fill=%f,padding=0]

  //  Build the Signal-event part.

  frame = gtkframe_new(label="Signal events");
  vbox.pack_start[ frame,expand=%t,fill=%t,padding=10]

  vbox2 = gtkvbox_new(homogeneous=%t,spacing=5);
  frame.add[  vbox2]
  
  hbox = gtkhbox_new(homogeneous=%f,spacing=3);
  vbox2.pack_start[ hbox,expand=%f,fill=%t,padding=0]
  label = gtklabel_new(str="Signal:");
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]
  
  last_sig = gtklabel_new(str="");
  hbox.pack_start[ last_sig,expand=%f,fill=%t,padding=0]

  hbox = gtkhbox_new(homogeneous=%f,spacing=3);
  vbox2.pack_start[ hbox,expand=%f,fill=%t,padding=0]
  label = gtklabel_new(str="Previous signal:");
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]
  prev_sig = gtklabel_new(str="");
  hbox.pack_start[ prev_sig,expand=%f,fill=%t,padding=0]

  hbox = gtkhbox_new(homogeneous=%f,spacing=3);
  vbox2.pack_start[ hbox,expand=%f,fill=%t,padding=0]
  label = gtklabel_new(str="Second previous signal:");
  hbox.pack_start[ label,expand=%f,fill=%t,padding=0]
  prev2_sig = gtklabel_new(str="");
  hbox.pack_start[ prev2_sig,expand=%f,fill=%t,padding=0]

  calendar.set_data[prev_sig=prev_sig];
  calendar.set_data[prev2_sig=prev2_sig];
  calendar.set_data[last_sig=last_sig];
  
  
  bbox = gtkhbuttonbox_new ();
  vbox.pack_start[ bbox,expand=%f,fill=%f,padding=0]
  bbox.set_layout[  GTK.BUTTONBOX_END]

  button = gtkbutton_new(label="Close");
  button.connect[  "clicked", button_destroy_win,list(window)];
  
  bbox.add[  button]
  button.set_flags[GTK.CAN_DEFAULT];
  button.grab_default[];
  window.show_all[];
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
  if calendar.is_marked[date(3)] then 
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

// -------------------


function calendar_set_flags(calendar,settings)
  options = ((2).^[0:4])*settings';
  calendar.set_display_options[options];
endfunction 

function calendar_toggle_flag(toggle,args)
  calendar=args(1);
  settings=calendar.get_data['settings'];
  i = toggle.get_data["number"];
  settings(i) =b2m( ~ m2b(settings(i)));
  //printf("%d --> %d\n",i,settings(i))
  calendar.set_data[settings=settings]
  calendar_set_flags(calendar,settings);
endfunction  

function calendar_font_selection_ok(button,data)
//   GtkRcStyle *style;
//   char *font_name;
//   if (calendar->window)
//     {
//       font_name = gtk_font_selection_dialog_get_font_name (GTK.FONT_SELECTION_DIALOG(calendar->font_dialog));
//       if (font_name) 
// 	{
// 	  style = gtkrcstyle_new ();
// 	  pango_font_description_free (style->font_desc);
// 	  style->font_desc = pango_font_description_from_string (font_name);
// 	  gtk_widget_modify_style (calendar->window, style);
// 	  g_free (font_name);
// 	}
//     }
//   gtk_widget_destroy (calendar->font_dialog);
endfunction 

function calendar_select_font(button,data)
//   GtkWidget *window;

//   if (!calendar->font_dialog) {
//     window = gtkfontselectiondialog_new ("Font Selection Dialog");
//     g_return_if_fail(window);
//     calendar->font_dialog = window;
    
//     window.set_position[  GTK.WIN_POS_MOUSE]
    
//     window.connect[  "destroy",
// 		      gtk_widget_destroyed,
// 		      &calendar->font_dialog]
    
//     g_signal_connect (window->ok_button,
// 		      "clicked", calendar_font_selection_ok,
// 		      calendar);
//     g_signal_connect_swapped (window->cancel_button,
// 			     "clicked", gtk_widget_destroy, 
// 			     calendar->font_dialog);
//   }
//   window=calendar->font_dialog;
//   if (!window)
//     window.show[];
//   else
//     window.destroy[];
// }
endfunction 

