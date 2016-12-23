//  Assistant
//
// Demonstrates a sample multi-step assistant. Assistants are used to divide
// an operation into several simpler sequential steps, and to guide the user
// through these steps.

function assistant = demo_assistant(do_widget)

  function y=apply_changes_gradually (data)
  //  Work, work, work...
    progress_bar=data(1)
    fraction = progress_bar.get_fraction [];
    fraction = fraction+ 0.05;
    if (fraction < 1.0) then
      progress_bar.set_fraction[fraction];
      y= %t;// G.SOURCE_CONTINUE;
    else
      //  Close automatically once changes are fully applied.
      data(2).destroy[];
      y= %f;// G.SOURCE_REMOVE;
    end
  endfunction

  function on_assistant_apply (widget, data)
  //  Start a timer to simulate changes taking a few seconds to apply.
    g_timeout_add (100, apply_changes_gradually,data)
  endfunction

  function on_assistant_close_cancel (widget, data)
    assistant = data(1);
    assistant.destroy[];
  endfunction

  function on_assistant_prepare (widget, page, data)
    current_page = widget.get_current_page [];
    n_pages = widget.get_n_pages [];
    title = sprintf ("Sample assistant (%d of %d)", current_page + 1, n_pages);
    widget.set_title[title];
    //  The fourth page (counting from zero) is the progress page.  The
    // user clicked Apply to get here so we tell the assistant to commit,
    // which means the changes up to this point are permanent and cannot
    // be cancelled or revisited.
    if (current_page == 3) then
      widget.commit[];
    end
  endfunction

  function on_entry_changed (widget, data)
    assistant = data(1);
    page_number = assistant.get_current_page [];
    current_page = assistant.get_nth_page[page_number];
    text = widget.get_text [];
    if ~text.equal[""] then
      assistant.set_page_complete[current_page, %t];
    else
      assistant.set_page_complete[current_page, %f];
    end
  endfunction

  function create_page1 (assistant)
    box = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing=12);
    box.set_border_width[12];
    label = gtk_label_new(str="You must fill out this entry to continue:");
    box.pack_start[label,expand=%f, fill=%f,padding=0];

    entry = gtk_entry_new ();
    entry.set_activates_default[%t];
    box.pack_start[entry,expand=%t, fill=%t,padding=0];
    entry.connect[ "changed", on_entry_changed, list(assistant)];

    box.show_all[];
    assistant.append_page[box];
    assistant.set_page_title[ box, "Page 1"];
    assistant.set_page_type[box, GTK.ASSISTANT_PAGE_INTRO];
  endfunction

  function create_page2 (assistant)
    box = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 12);
    box.set_border_width[12];
    checkbutton = gtk_check_button_new(label="This is optional data, you may continue "+...
				       "even if you do not check this");
    box.pack_start[checkbutton, expand=%f, fill=%f, padding=0];
    box.show_all[];
    assistant.append_page[box];
    assistant.set_page_complete[box, %t];
    assistant.set_page_title [ box, "Page 2"];
  endfunction

  function create_page3 (assistant)
    label = gtk_label_new(str="This is a confirmation page, press ''Apply'' to apply changes");
    label.show[];
    assistant.append_page[label];
    assistant.set_page_type[label, GTK.ASSISTANT_PAGE_CONFIRM];
    assistant.set_page_complete[label, %t];
    assistant.set_page_title [ label, "Confirmation"];
  endfunction

  function progress_bar=create_page4 (assistant)
    progress_bar = gtk_progress_bar_new ();
    progress_bar.set_halign[GTK.ALIGN_CENTER];
    progress_bar.set_valign[GTK.ALIGN_CENTER];

    progress_bar.show[];
    assistant.append_page[progress_bar];
    assistant.set_page_type[progress_bar, GTK.ASSISTANT_PAGE_PROGRESS];
    assistant.set_page_title[ progress_bar, "Applying changes"];
    //  This prevents the assistant window from being
    // closed while we're "busy" applying changes.
    assistant.set_page_complete[progress_bar, %f];
  endfunction
  
  assistant = gtk_assistant_new ();
  assistant.set_default_size [ -1, 300];
  assistant.set_screen[ do_widget.get_screen []];
  create_page1 (assistant);
  create_page2 (assistant);
  create_page3 (assistant);
  progress_bar=create_page4 (assistant);
  assistant.connect [ "cancel",  on_assistant_close_cancel, list(assistant)];
  assistant.connect [ "close",   on_assistant_close_cancel, list(assistant)];
  assistant.connect [ "apply",   on_assistant_apply,list(progress_bar,assistant)];
  assistant.connect [ "prepare", on_assistant_prepare];
  assistant.show[];
endfunction
