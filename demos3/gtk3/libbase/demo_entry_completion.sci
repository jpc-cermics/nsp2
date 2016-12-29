// Entry/Entry Completion
//
// GtkEntryCompletion provides a mechanism for adding support for
// completion in GtkEntry.
//

function window=demo_entry_completion (do_widget)
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then   window.set_screen[ do_widget.get_screen []];end
  
  window.set_title[ "Entry Completion"];
  window.set_resizable[%f];

  // window.connect[ "destroy", gtk_widget_destroyed, &window];

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 5);
  window.add[vbox];
  vbox.set_border_width[5];

  label = gtk_label_new();
  label.set_markup[ "Completion demo, try writing <b>total</b> or <b>gnome</b> for example."];
  vbox.pack_start[label, expand=%f, fill=%f,padding=0];

  //  Create our entry  
  entry = gtk_entry_new ();
  vbox.pack_start[entry, expand=%f, fill=%f,padding=0];

  //  Create the completion object  
  completion = gtk_entry_completion_new ();
  
  //  Assign the completion to the entry  
  entry.set_completion[completion];

  //  Create a tree model and use it as the completion model  
  
  completion_model = gtk_list_store_new (list("str"),%f)
  words = ["GNOME","total","totally"];
  for i=1:size(words,'*') 
    iter = completion_model.append[];
    completion_model.set[iter, 0, words(i)];
  end
  completion.set_model[model= completion_model];
  //  Use model column 0 as the text column  
  completion.set_text_column[0];
  
  window.show_all[];
endfunction
