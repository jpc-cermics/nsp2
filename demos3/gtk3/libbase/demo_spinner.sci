// Spinner
//
// GtkSpinner allows to show that background activity is on-going.
//

function window = demo_spinner (do_widget)

  function on_play_clicked (button, user_data)
    for i=1:length(user_data)
      spinner= user_data(i);
      spinner.start[];
    end
  endfunction

  function on_stop_clicked (button, user_data)
    for i=1:length(user_data)
      spinner= user_data(i);
      spinner.stop[];
    end
  endfunction
  
  // parent=do_widget,...
  window = gtk_dialog_new(title="Spinner",...
			  flags= 0,...
			  buttons= "_Close")
  
  window.set_resizable[%f];
  //window.connect[ "response", gtk_widget_destroy];
  // window.connect[ "destroy", gtk_widget_destroyed, &window);

  content_area = window.get_content_area [];
  
  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 5);
  content_area.pack_start[vbox,expand= %t,fill= %t, padding= 0];
  vbox.set_border_width[5];

  //  Sensitive  
  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 5);
  spinner = gtk_spinner_new ();
  hbox.add[spinner];
  hbox.add[gtk_entry_new ()];
  vbox.add[hbox];
  spinner_sensitive = spinner;

  //  Disabled  
  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 5);
  spinner = gtk_spinner_new ();
  hbox.add[spinner];
  hbox.add[gtk_entry_new ()];
  vbox.add[hbox];
  spinner_unsensitive = spinner;
  hbox.set_sensitive[%f];
  spinners= list(spinner_sensitive,spinner_unsensitive);
  
  button = gtk_button_new(label="Play");
  button.connect[ "clicked", on_play_clicked, spinners];
  vbox.add[button];

  button = gtk_button_new(label="Stop");
  button.connect[ "clicked", on_stop_clicked, spinners];
  vbox.add[button];

  //  Start by default to test for:
  // https://bugzilla.gnome.org/show_bug.cgi?id=598496  
  //on_play_clicked (spinners);
  window.show_all[];
  window.run[];
  window.destroy[];
endfunction 

