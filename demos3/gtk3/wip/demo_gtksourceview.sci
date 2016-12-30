//  -*- Mode: C;

//  Private data structures  
MARK_TYPE_2= "two"

//  Actions & UI definition ----------------------------------------------------  

//static GtkActionEntry buffer_action_entries[] = //////////{
  //////////{ "Open", GTK.STOCK_OPEN, "_Open", "<control>O", "Open a file", open_file_cb end,
  //////////{ "Save", GTK.STOCK_SAVE, "Save", NULL,NULL, save_cbend,
  //////////{ "SaveAs", GTK.STOCK_SAVE_AS, "Save _As...", NULL,NULL, save_as_cbend,
  //////////{ "Close", GTK.STOCK_CLOSE, "_Close", NULL, "Close edit window", close_cb end,
  //////////{"Execute",NULL, "_Execute...", "<control>l" ,NULL, execute_cbend,
  //////////{"ExecuteSelection",NULL, "Execute Selection","<control>y" ,NULL, execute_selection_cbend,

//static GtkActionEntry view_action_entries[] = //////////{
  //////////{ "FileMenu", NULL, "_File", NULL, NULL, NULL end,
  //////////{ "Print", GTK.STOCK_PRINT, "_Print", NULL // "<control>P"  , "Print the current file", print_file_cb end,
  //////////{ "ViewMenu", NULL, "_View", NULL, NULL, NULL end,
  //////////{ "ExecMenu", NULL, "Tools", NULL, NULL, NULL end,
  //////////{ "TabWidth", NULL, "_Tab Width", NULL, NULL, NULL end,
  //////////{ "IndentWidth", NULL, "I_ndent Width", NULL, NULL, NULL end,
  //////////{ "SmartHomeEnd", NULL, "_Smart Home/End", NULL, NULL, NULL end,
  //////////{ "Find", GTK.STOCK_FIND, "_Find", NULL //  "<control>F  , "Find", find_cb end,
  //////////{ "Replace", GTK.STOCK_FIND_AND_REPLACE, "Search and _Replace", "<control>R", "Search and Replace", replace_cb end,
  //////////{ "ForwardString", NULL, "_Forward to string toggle", "<control>S", "Forward to the start or end of the next string", forward_string_cb end,
  //////////{ "BackwardString", NULL, "_Backward to string toggle", "<control><shift>S", "Backward to the start or end of the next string", backward_string_cb end

//static GtkToggleActionEntry toggle_entries[] = //////////{
  //////////{ "HlBracket", NULL, "Highlight Matching _Bracket", NULL, "Toggle highlighting of matching bracket", hl_bracket_toggled_cb, %f end,
  //////////{ "ShowNumbers", NULL, "Show _Line Numbers", NULL, "Toggle visibility of line numbers in the left margin",  numbers_toggled_cb, %f end,
  //////////{ "ShowMarks", NULL, "Show Line _Marks", NULL, "Toggle visibility of marks in the left margin", marks_toggled_cb, %f end,
  //////////{ "ShowMargin", NULL, "Show Right M_argin", NULL,    "Toggle visibility of right margin indicator",    margin_toggled_cb, %f end,
  //////////{ "HlLine", NULL, "_Highlight Current Line", NULL,    "Toggle highlighting of current line",    hl_line_toggled_cb, %f end,
  //////////{ "DrawSpaces", NULL, "_Draw Spaces", NULL,    "Draw Spaces",    draw_spaces_toggled_cb, %f end,
  //////////{ "WrapLines", NULL, "_Wrap Lines", NULL,    "Toggle line wrapping",    wrap_lines_toggled_cb, %f end,
  //////////{ "AutoIndent", NULL, "Enable _Auto Indent", NULL,    "Toggle automatic auto indentation of text",    auto_indent_toggled_cb, %f end,
  //////////{ "InsertSpaces", NULL, "Insert _Spaces Instead of Tabs", NULL,    "Whether to insert space characters when inserting tabulations",    insert_spaces_toggled_cb, %f end,
  //////////{ "SilentExecute", NULL, "Execute code silently", NULL,    "Toggle silent execution code option",    silent_execute_toggled_cb, %t end

//static GtkRadioActionEntry tabs_radio_entries[] = //////////{
  //////////{ "TabWidth2", NULL, "2", NULL, "Set tabulation width to 2 spaces", 2 end,
  //////////{ "TabWidth4", NULL, "4", NULL, "Set tabulation width to 4 spaces", 4 end,
  //////////{ "TabWidth6", NULL, "6", NULL, "Set tabulation width to 6 spaces", 6 end,
  //////////{ "TabWidth8", NULL, "8", NULL, "Set tabulation width to 8 spaces", 8 end,
  //////////{ "TabWidth10", NULL, "10", NULL, "Set tabulation width to 10 spaces", 10 end,
  //////////{ "TabWidth12", NULL, "12", NULL, "Set tabulation width to 12 spaces", 12 end

//static GtkRadioActionEntry indent_radio_entries[] = //////////{
  //////////{ "IndentWidthUnset", NULL, "Use Tab Width", NULL, "Set indent width same as tab width", -1 end,
  //////////{ "IndentWidth2", NULL, "2", NULL, "Set indent width to 2 spaces", 2 end,
  //////////{ "IndentWidth4", NULL, "4", NULL, "Set indent width to 4 spaces", 4 end,
  //////////{ "IndentWidth6", NULL, "6", NULL, "Set indent width to 6 spaces", 6 end,
  //////////{ "IndentWidth8", NULL, "8", NULL, "Set indent width to 8 spaces", 8 end,
  //////////{ "IndentWidth10", NULL, "10", NULL, "Set indent width to 10 spaces", 10 end,
  //////////{ "IndentWidth12", NULL, "12", NULL, "Set indent width to 12 spaces", 12 end

//static GtkRadioActionEntry smart_home_end_entries[] = //////////{
  //////////{ "SmartHomeEndDisabled", NULL, "Disabled", NULL,    "Smart Home/End
  //disabled", GTK.SOURCE_SMART_HOME_END_DISABLED end,  
  //////////{ "SmartHomeEndBefore", NULL, "Before", NULL, "Smart Home/End before", GTK.SOURCE_SMART_HOME_END_BEFORE end,
  //////////{ "SmartHomeEndAfter", NULL, "After", NULL,  "Smart Home/End after", GTK.SOURCE_SMART_HOME_END_AFTER end,
  //////////{ "SmartHomeEndAlways", NULL, "Always", NULL, "Smart Home/End always", GTK.SOURCE_SMART_HOME_END_ALWAYS end

view_ui_description = [...
  "<ui>"
  "  <menubar name=''MainMenu''>"
  "    <menu action=''FileMenu''>"
  "      <!--"
  "      <menuitem action=''PrintPreview''/>"
  "      -->"
  "    </menu>"
  "    <menu action=''ViewMenu''>"
  "      <menuitem action=''HlBracket''/>"
  "      <menuitem action=''ShowNumbers''/>"
  "      <menuitem action=''ShowMarks''/>"
  "      <menuitem action=''ShowMargin''/>"
  "      <menuitem action=''HlLine''/>"
  "      <menuitem action=''DrawSpaces''/>"
  "      <menuitem action=''WrapLines''/>"
  "      <separator/>"
  "      <menuitem action=''AutoIndent''/>"
  "      <menuitem action=''InsertSpaces''/>"
  "      <separator/>"
  "      <menu action=''TabWidth''>"
  "        <menuitem action=''TabWidth2''/>"
  "        <menuitem action=''TabWidth4''/>"
  "        <menuitem action=''TabWidth6''/>"
  "        <menuitem action=''TabWidth8''/>"
  "        <menuitem action=''TabWidth10''/>"
  "        <menuitem action=''TabWidth12''/>"
  "      </menu>"
  "      <menu action=''IndentWidth''>"
  "        <menuitem action=''IndentWidthUnset''/>"
  "        <menuitem action=''IndentWidth2''/>"
  "        <menuitem action=''IndentWidth4''/>"
  "        <menuitem action=''IndentWidth6''/>"
  "        <menuitem action=''IndentWidth8''/>"
  "        <menuitem action=''IndentWidth10''/>"
  "        <menuitem action=''IndentWidth12''/>"
  "      </menu>"
  "      <separator/>"
  "      <menu action=''SmartHomeEnd''>"
  "        <menuitem action=''SmartHomeEndDisabled''/>"
  "        <menuitem action=''SmartHomeEndBefore''/>"
  "        <menuitem action=''SmartHomeEndAfter''/>"
  "        <menuitem action=''SmartHomeEndAlways''/>"
  "      </menu>"
    "      <separator/>"
  "      <menuitem action=''ForwardString''/>"
  "      <menuitem action=''BackwardString''/>"
  "    </menu>"
  "  </menubar>"
  "</ui>"];

buffer_file_ui_description =[...
  "<ui>"
  "  <menubar name=''MainMenu''>"
  "    <menu action=''FileMenu''>"
  "      <menuitem action=''Open''/>"
  "      <menuitem action=''Save''/>"
  "      <menuitem action=''SaveAs''/>"
  "      <menuitem action=''Print''/>"
  "      <menuitem action=''Find''/>"
  "      <menuitem action=''Replace''/>"
  "      <separator/>"
  "      <menuitem action=''Close''/>"
  "    </menu>"
  "    <menu action=''ViewMenu''>"
  "    </menu>"
  "    <menu action=''ExecMenu''>"
  "      <menuitem action=''Execute'' />\n"
  "      <menuitem action=''ExecuteSelection'' />\n"
  "      <menuitem action=''SilentExecute'' />\n"
  "    </menu>"
  "  </menubar>"
  "</ui>"];

buffer_smatrix_ui_description =[...
  "<ui>"
  "  <menubar name=''MainMenu''>"
  "    <menu action=''FileMenu''>"
  "      <menuitem action=''Print''/>"
  "      <menuitem action=''Find''/>"
  "      <menuitem action=''Replace''/>"
  "      <separator/>"
  "      <menuitem action=''Close''/>"
  "    </menu>"
  "    <menu action=''ViewMenu''>"
  "    </menu>"
  "  </menubar>"
  "</ui>"];

//  File loading code -----------------------------------------------------------------  

function error_dialog (parent) 
  va_start (ap, msg);
  tmp = g_strdup_vprintf (msg, ap);
  va_end (ap);
  dialog = gtk_message_dialog_new (parent,
  GTK.DIALOG_DESTROY_WITH_PARENT,
  GTK.MESSAGE_ERROR,
  GTK.BUTTONS_OK,
  "%s", tmp);
  gtk_dialog_run (dialog);
  dialog.destroy[];
endfunction

function y = gtk_source_buffer_load_file (source_buffer,filename) 
// g_return_val_if_fail (source_buffer, %f);
// g_return_val_if_fail (filename != NULL, %f);

  if ( ~filename.get_contents[buffer, NULL] )
    error_dialog (NULL, "%s\nFile %s", error_here.message, filename);
    g_propagate_error (error, error_here);
    y= %f;return
  end
  buffer_utf8= nsp_string_to_utf8(buffer);
  if buffer_utf8 then
    Sciprintf("File %s is not utf8 \n",filename);
    g_free(buffer);
    y= %f;return
  end
  
  gtk_source_buffer_begin_not_undoable_action (source_buffer);
  source_buffer.set_text[ buffer_utf8, -1];
  gtk_source_buffer_end_not_undoable_action (source_buffer);
  source_buffer.set_modified[%f];

  //  move cursor to the beginning  
  iter = source_buffer.get_start_iter[]
  source_buffer.place_cursor[iter];

  [start,bend]=source_buffer.get_bounds[];
  text = source_buffer.get_text[ start, bend, %t];
  g_assert (~strcmp (text, buffer));
  g_free (text);
  
  g_free (buffer);
  if ( buffer_utf8 ~= buffer) then 
    g_free (buffer_utf8);
    y= %f;return
  end
endfunction 

function remove_all_marks (buffer)
  [s,e]=buffer.get_bounds[];
  buffer.remove_source_marks[s,e,""];
endfunction 

function language=get_language_for_file (buffer, filename)

  start = buffer.get_start_iter[];
  if buffer.get_char_count [] < 1024 then
  end_iter = buffer.get_end_iter[];
  else
  end_iter = buffer.get_iter_at_offset[ 1024];
  end
  text = buffer.get_slice[ start, end_iter, %t];

  content_type = g_content_type_guess (filename,text, strlen (text), result_uncertain);
  if (result_uncertain) then 
    g_free (content_type);
    content_type = NULL;
  end
  manager = get_default [];
  language = gtk_source_language_manager_guess_language (manager, filename,content_type);
  // 
  g_message ("Detected ''%s'' mime type for file %s, chose language %s",
  content_type,
  filename,
  language.get_id []);
  g_free (content_type);
  g_free (text);
endfunction

function language = get_language_by_id ( id)
  manager;
  manager = get_default [];
  language = manager.get_language[id];
endfunction

function language = get_language (buffer, filename)
  start = buffer.get_start_iter[];
end_iter = start;
end_iter.forward_line[];
text = start.get_slice[end_iter];
lang_string = strstr (text, LANG_STRING);
if (lang_string ~= NULL) then 

  lang_string = lang_string +  strlen (LANG_STRING);
  g_strchug (lang_string);
  
  tokens = g_strsplit_set (lang_string, " \t\n", 2);

  if (tokens ~= NULL && tokens[0] ~= NULL)
    language = get_language_by_id (tokens[0]);
  end
  g_strfreev (tokens);
end

if (~language && filename ~= NULL )
  language = get_language_for_file (buffer, filename);
end
ext= nsp_get_extension(filename);
if (~language || strcmp(ext,".sci")== 0 || strcmp(ext,".sce")==0 )
  language = nsp_gtksource_language ();
end
g_free (text);
endfunction 

function res = open_file (buffer, filename)
  pause open_file 
  success = %f;
  if filename.equal[""] then 
    // nsp language is the default 
    remove_all_marks (buffer);
    language = nsp_gtksource_language ();
    buffer.set_language[language];
    res = %t;
    return;
  end
  if (~g_path_is_absolute (filename)) 
    curdir = g_get_current_dir ();
    freeme = g_build_filename (curdir, filename, NULL);
    filename = freeme;
    g_free (curdir);
  end
  remove_all_marks (buffer);
  success = gtk_source_buffer_load_file (buffer, filename, NULL);
  //  this will set language to nsp if filename == NULL  
  language = get_language (buffer, filename);
  if (language == NULL)
    g_print ("No language found for file `%s''\n", filename);
  end
  pausex
  buffer.set_language[language];

  if ( success )
    buffer.set_data[filename = filename];
    view =  buffer.get_data[ "buffer_view"];
    if ( view)
      view_set_title (view, %f);
    end
  end
  res = success;
endfunction 

//  S is assumed to be a utf8 matrix

function y= gtk_source_buffer_load_smatrix (source_buffer, NspSMatrix *S)

  g_return_val_if_fail (source_buffer, %f);
  g_return_val_if_fail (S ~= NULL, %f);

  gtk_source_buffer_begin_not_undoable_action (source_buffer);
  source_buffer.get_start_iter[ iter];
  for i = 1:size(S,'*') 
    //////////{
    gtk_text_buffer_insert(source_buffer,iter, S.S[i],-1);
    gtk_text_buffer_insert(source_buffer,iter, "\n",-1);
  end
  gtk_source_buffer_end_not_undoable_action (source_buffer);
  source_buffer.set_modified[%f];
  //  move cursor to the beginning  
  source_buffer.get_start_iter[ iter];
  gtk_text_buffer_place_cursor (source_buffer, iter);
  y = %t;
endfunction 

function success = open_smatrix (buffer, title, S)
  success = %f;
  remove_all_marks (buffer);
  language = nsp_gtksource_language ();
  buffer.set_language[language];
  success = gtk_source_buffer_load_smatrix(buffer,S);
  //  to keep track that this is a smatrix buffer  
  buffer.set_data[smatrix = 1];
  if ( success == %f ) then return;end 
  window =  buffer.get_data[ "buffer_window"];
  if ( window) then window.set_title[title];end
endfunction 

//  get buffer content in a SMatrix   

function smat = save_buffer_in_smatrix(buffer)
  start = buffer.get_iter_at_offset[0];
end_iter = buffer.get_end_iter[];
chars = buffer.get_slice[start,end_iter, %f];
buffer.set_modified[%f];
S=m2s([])
while ( chars ~= '\0')
  //////////{
  tag = strstr(chars,"\n");
  if(tag ~= NULL) then tag ='\0';end 
  if ( nsp_row_smatrix_append_string(S,chars) == FAIL)
    return;
  end
  if ( tag == NULL) then break; end 
  chars = tag+1;
end
g_free (chars1);
//  return a non-empty object if buffer is empty  
if (S.mn==0) then //////////{
  if ( nsp_row_smatrix_append_string(S,"") == FAIL) then 
    return;
  end
end
// if ( S ~= NULL) then nsp_smatrix_destroy(S);end 
endfunction 

//  View action callbacks --------------------------------------------------------  

function execute_cb (action, user_data)
  buffer = user_data;
  display=%f,echo =%f,errcatch=%t,pausecatch=%t,mtlb=%f;
  // g_return_if_fail (user_data);

  //  save first  
  //  push_active_window (GTK.WINDOW (view.window));  
  remove_all_marks (buffer);
  [start,end_iter]=buffer.get_bounds[];
  filename = buffer.get_data[ "filename"];

  if ( filename ) then 
    if (  save_buffer (buffer,filename)== %f) then return;end 
    //  execute the file contents  
    if ( execute_silently == %f ) then 
      display = %t; echo = %t;
    end
    rep =nsp_parse_eval_file(filename,display,echo,errcatch, ~pausecatch ...
			     , mtlb);
    if ( rep < 0 ) then
	  //  get the line number of the Error  
	  line=-1,i;
	  error_msg = nsp_lasterror_get();
	  for i=0:error_msg.mn
	    Sciprintf("%s",error_msg.S[i]);
	    if ( sscanf(error_msg.S[i],"\tline %d of file %s",line,fname)==2) ...
	    then
	      if ( strcmp(fname,filename) == 0) then
		execute_tag_error(buffer,line);
		break;
	      end
	    end
	  end
	  // 
	  if ( sscanf(error_msg.S[error_msg.mn-1],"Error: at line %d of file",line)==1)
	    //////////{
	    execute_tag_error(view,line);
	  end
	  nsp_lasterror_clear();
    end
  else
    //  execute the file contents through matrix  
    buf_str= start.get_text[end_iter];
    nsp_eval_str_in_terminal(buf_str, execute_silently);
  end
  pop_active_window ();
endfunction 

function execute_selection_cb (action, user_data)
  buffer = user_data;
  g_return_if_fail (user_data);
  //  save first  
  //  push_active_window (GTK.WINDOW (view.window));  
  remove_all_marks (buffer);
  if (~ buffer.get_selection_bounds[ start, end_iter]) then
    return;
  end 
  str = start.get_visible_text[end_iter];
  nsp_eval_str_in_terminal(str,execute_silently);
  //  pop_active_window ();  
endfunction 

//  put line in red

function execute_tag_error(buffer,line)
  istart = buffer.get_iter_at_line[line-1];
  view =  buffer.get_data[ "buffer_view"];
  if ( view ) then
    gtk_text_view_scroll_to_iter(view,istart,0.0,%f,0,0);
  end
  //  get the marks already in the line  
  mark_list = buffer.get_source_marks_at_line [line-1, MARK_TYPE_2];
  if (mark_list == NULL) then
    //////////{
    //  no mark found: create one  
    gtk_source_buffer_create_source_mark (buffer, NULL, MARK_TYPE_2, istart);
  end
  g_slist_free (mark_list);
endfunction 

function numbers_toggled_cb (action, user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_show_line_numbers (
					 user_data,
					 action.get_active []);
endfunction

function marks_toggled_cb (action, user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_show_line_marks (
				       user_data,
				       action.get_active []);
endfunction

function margin_toggled_cb (action, user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_show_right_margin (
					 user_data,
					 action.get_active []);
endfunction

function hl_bracket_toggled_cb (action, user_data)
//////////{
  buffer;
  g_return_if_fail (action && user_data);
  buffer = user_data.get_buffer [];
  gtk_source_buffer_set_highlight_matching_brackets (
						     buffer,
						     action.get_active []);
endfunction

function hl_line_toggled_cb (action, user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_highlight_current_line (
					      user_data,
					      action.get_active []);
endfunction

function draw_spaces_toggled_cb (action, user_data)
  g_return_if_fail (action && user_data);
  draw_spaces = action.get_active [];

  if (draw_spaces) then
    user_data.set_draw_spaces[ GTK.SOURCE_DRAW_SPACES_ALL];
  else
    user_data.set_draw_spaces[ 0];
  end
endfunction

function wrap_lines_toggled_cb (action, user_data)
//////////{
  g_return_if_fail (action && user_data);
  if action.get_active [] then 
    user_data.set_wrap_mode[ GTK.WRAP_WORD];
  else
    user_data.set_wrap_mode[ GTK.WRAP_NONE];
  end
endfunction

function auto_indent_toggled_cb (action,
			user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_auto_indent (
				   user_data,
				   action.get_active []);
endfunction

function insert_spaces_toggled_cb (action,
			  user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_insert_spaces_instead_of_tabs (
						     user_data,
						     action.get_active []);
endfunction

function silent_execute_toggled_cb (action,
			   user_data)
//////////{
  g_return_if_fail (action && user_data);
  execute_silently = action.get_active [];

endfunction

function tabs_toggled_cb (action,
		 current,
		 user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_tab_width (
				 user_data,
				 action.get_current_value []);
endfunction

function indent_toggled_cb (action,
		   current,
		   user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_indent_width (
				    user_data,
				    action.get_current_value []);
endfunction

function smart_home_end_toggled_cb (action,
			   current,
			   user_data)
//////////{
  g_return_if_fail (action && user_data);
  gtk_source_view_set_smart_home_end (
				      user_data,
				      action.get_current_value []);
endfunction

//  2.10.0 ? version  

function forward_string_cb (action, user_data)

  g_return_if_fail (user_data);
  view = user_data;
  buffer = GTK.SOURCE_BUFFER (view.get_buffer []);
  insert = buffer.get_insert [];

  buffer.get_iter_at_mark[ iter, insert];
  
  if (gtk_source_buffer_iter_forward_to_context_class_toggle (buffer,...
						  iter,...
						  "string")) then
    gtk_text_buffer_place_cursor (buffer, iter);
    gtk_text_view_scroll_mark_onscreen (view, insert);
  end
endfunction

function backward_string_cb (action, user_data)

  g_return_if_fail (user_data);
  view = user_data;
  buffer = GTK.SOURCE_BUFFER (view.get_buffer []);
  insert = buffer.get_insert [];

  buffer.get_iter_at_mark[ iter,insert];

  if (gtk_source_buffer_iter_backward_to_context_class_toggle (buffer,
							       iter,
							       "string")) ...
								   then
							       gtk_text_buffer_place_cursor (buffer, iter);
							       gtk_text_view_scroll_mark_onscreen (view, insert);
  end
endfunction 

//  Buffer action callbacks ------------------------------------------------------------  

// static struct //////////{
//   char *what;
//   char *replacement;
//   GtkSourceSearchFlags flags;
// end search_data = //////////{
//   NULL,
//   NULL,

//   GTK.SOURCE_SEARCH_CASE_INSENSITIVE
// end;

function y = search_dialog (widget, replace, what_p, replacement_p, flags_p)
  if replace then title = "Replace"; else title =  "Find";end 
  dialog = gtk_dialog_new_with_buttons (title,
					GTK.WINDOW (widget.get_toplevel []),
					GTK.DIALOG_MODAL,
					GTK.STOCK_CANCEL,
					GTK.RESPONSE_CANCEL,
					GTK.STOCK_OK,
					GTK.RESPONSE_OK,
					NULL);
  dialog.set_default_response[GTK.RESPONSE_OK];
  
  entry1 = g_object_new (GTK.TYPE_ENTRY,
			 "visible", %t,
			 "text", search_data.what,
			 "activates-default", %t,
			 NULL);
  gtk_box_pack_start (GTK.BOX (dialog.get_content_area []),
		      GTK.WIDGET (entry1), %t, %t, 0);
  entry2 = g_object_new (GTK.TYPE_ENTRY,
			 "visible", replace,
			 "text", search_data.replacement,
			 "activates-default", %t,
			 NULL);
  gtk_box_pack_start (GTK.BOX (dialog.get_content_area []),
		      GTK.WIDGET (entry2), %t, %t, 0);

  case_sensitive = g_object_new (GTK.TYPE_CHECK_BUTTON,
				 "visible", %t,
				 "label", "Case sensitive",
				 "active", ~(search_data.flags & GTK.SOURCE_SEARCH_CASE_INSENSITIVE),
				 NULL);
  gtk_box_pack_start (GTK.BOX (dialog.get_content_area []),
		      case_sensitive, %f, %f, 0);
  while (%t)
  
      if (gtk_dialog_run (dialog) ~= GTK.RESPONSE_OK) then
	dialog.destroy[];
	y = %f;
	return;
      end
      if (gtk_entry_get_text (entry1)) then
	break;
      end
  end

  g_free (search_data.what);
  search_data.what = g_strdup (gtk_entry_get_text (entry1));
  what_p = search_data.what;
  g_free (search_data.replacement);
  search_data.replacement = g_strdup (gtk_entry_get_text (entry2));
  replacement_p = search_data.replacement 
  if case_sensitive.get_active [] then 
    search_data.flags = 0 ;
  else 
    search_data.flags = GTK.SOURCE_SEARCH_CASE_INSENSITIVE;
  end
  flags_p = search_data.flags ;
  dialog.destroy[];
  y = %t;
endfunction 

function do_search_replace (view, replace)
//////////{
  buffer = view.get_buffer [];

  if (~search_dialog (view, replace, what, replacement, flags))
    return;
  end
  if (replace)
    //////////{
      buffer.get_iter_at_offset[ iter, 0];

      while (%t)
	if (~gtk_source_iter_forward_search (iter, what, flags,
	  match_start,
	  match_end,
	  NULL))
	  break;
	end
	gtk_text_buffer_delete (buffer, match_start, match_end);
	gtk_text_buffer_insert (buffer, match_start, replacement, -1);
	iter = match_start;
      end
  else
    buffer.get_iter_at_mark[ iter, buffer.get_insert []];
    if (gtk_source_iter_forward_search (iter, what, flags, match_start, match_end, NULL))
      //////////{
      gtk_text_buffer_select_range (buffer, match_start, match_end);
    else
      insert = iter;
      buffer.get_start_iter[ iter];
      if (gtk_source_iter_forward_search (iter, what, flags, match_start, match_end, insert))
	gtk_text_buffer_select_range (buffer, match_start, match_end);
      end
    end
  end
endfunction

function find_cb (action,	 user_data)
  do_search_replace (user_data, %f);
endfunction

function replace_cb (action,  user_data)
  do_search_replace (user_data, %t);
endfunction

function open_file_cb (action, user_data)
  last_dir = "";
  g_return_if_fail (user_data);

  chooser = gtk_file_chooser_dialog_new ("Open file...", NULL,...
					 GTK.FILE_CHOOSER_ACTION_OPEN,...
					 GTK.STOCK_CANCEL, GTK.RESPONSE_CANCEL,...
					 GTK.STOCK_OPEN, GTK.RESPONSE_OK,...
					 NULL);
					 
  if (last_dir ~= NULL && g_path_is_absolute (last_dir))
    chooser.set_current_folder[last_dir];
  end
  response = gtk_dialog_run (chooser);
  
  if (response == GTK.RESPONSE_OK)
    filename = chooser.get_filename [];
    if (filename ~= NULL)
      g_free (last_dir);
      last_dir = chooser.get_current_folder [];
      open_file (user_data, filename);
      g_free (filename);
    end
  end
  chooser.destroy[];
endfunction 

//#define NON_BLOCKING_PAGINATION
//#ifndef NON_BLOCKING_PAGINATION

function begin_print (operation, context, compositor)
  while (~gtk_source_print_compositor_paginate (compositor, context))
    n_pages = compositor.get_n_pages [];
    operation.set_n_pages[n_pages];
  end
endfunction 

function y = paginate (operation, context, compositor)
  g_print ("Pagination progress: %.2f %%\n", compositor.get_pagination_progress [] * 100.0);
  
  if (gtk_source_print_compositor_paginate (compositor, context))
    g_assert (compositor.get_pagination_progress [] == 1.0);
    g_print ("Pagination progress: %.2f %%\n", compositor.get_pagination_progress [] * 100.0);
    
    n_pages = compositor.get_n_pages [];
    operation.set_n_pages[n_pages];
    y = %t;return;
  end
  y= %f;
endfunction 

function draw_page (operation, context, page_nr, compositor)
//  This part of the code shows how to add a custom overlay to the
  cr = context.get_cairo_context [];
  cairo_save (cr);
  layout = gtk_print_context_create_pango_layout (context);
  pango_layout_set_text (layout, "Draft", -1);

  desc = pango_font_description_from_string ("Sans Bold 120");
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  pango_layout_get_extents (layout, NULL, rect);

  cairo_move_to (cr,
		 (context.get_width[] - ( rect.width / PANGO_SCALE)) / 2,
		 (context.get_height[] - ( rect.height / PANGO_SCALE)) / 2);

  pango_cairo_layout_path (cr, layout);

  //  Font Outline  
  cairo_set_source_rgba (cr, 0.85, 0.85, 0.85, 0.80);
  cairo_set_line_width (cr, 0.5);
  cairo_stroke_preserve (cr);

  //  Font Fill  
  cairo_set_source_rgba (cr, 0.8, 0.8, 0.8, 0.60);
  cairo_fill (cr);

  g_object_unref (layout);
  cairo_restore (cr);
  //  To print page_nr you only need to call the following function  
  gtk_source_print_compositor_draw_page (compositor, context, page_nr);
endfunction 

function end_print (operation, context, compositor)
  g_object_unref (compositor);
endfunction 

// #define LINE_NUMBERS_FONT_NAME	"Sans 8"
// #define HEADER_FONT_NAME	"Sans 11"
// #define FOOTER_FONT_NAME	"Sans 11"
// #define BODY_FONT_NAME		"Monospace 9"

function print_file_cb (action, user_data)
  g_return_if_fail (user_data);
  view = user_data;
  buffer = GTK.SOURCE_BUFFER (view.get_buffer []);
  filename = buffer.get_data[ "filename"];
  basename = g_filename_display_basename (filename);

  compositor = gtk_source_print_compositor_new_from_view (view);

  compositor = gtk_source_print_compositor_new (buffer);

  compositor.set_tab_width[ view.get_tab_width []];

  compositor.set_wrap_mode[ view.get_wrap_mode []];

  compositor.set_print_line_numbers[1];

  compositor.set_body_font_name[BODY_FONT_NAME];

  //  To test line numbers font ~= text font  
  compositor.set_line_numbers_font_name[LINE_NUMBERS_FONT_NAME];

  compositor.set_header_format[ %t, "Printed on %A","test-widget","%F"];

  compositor.set_footer_format[ %t, "%T", basename, "Page %N/%Q"];

  compositor.set_print_header[%t];
  compositor.set_print_footer[%t];

  compositor.set_header_font_name[  HEADER_FONT_NAME];
  compositor.set_footer_font_name[  FOOTER_FONT_NAME];
  operation = gtk_print_operation_new ();

  operation.set_job_name[basename];

  operation.set_show_progress[%t];

  operation.connect[ "begin-print",  begin_print, compositor];
  operation.connect[ "paginate",   paginate, compositor];

  operation.connect[ "draw-page", draw_page, compositor];
  operation.connect[ "end-print", end_print, compositor];

  gtk_print_operation_run (operation, GTK.PRINT_OPERATION_ACTION_PRINT_DIALOG, NULL, NULL);

  g_object_unref (operation);
  g_free (basename);
endfunction 

//  View UI callbacks ------------------------------------------------------------------  

function y = filechooser_save_run (parent, title, start_file, func, data)
  result = %f;
  //  XXX if (~parent)  parent = get_active_window ();  
  dialog = gtk_file_chooser_dialog_new (title,
					parent,
					GTK.FILE_CHOOSER_ACTION_SAVE,
					GTK.STOCK_CANCEL, GTK.RESPONSE_CANCEL,
					GTK.STOCK_SAVE, GTK.RESPONSE_ACCEPT,
					NULL);

  //  if (parent)  dialog.set_transient_for[parent];  

  if (start_file)
    dialog.set_current_name[start_file];
  end
  
  while (1)
    //////////{
      rep = gtk_dialog_run (dialog);
      if ( rep ==  GTK.RESPONSE_ACCEPT )
	//////////{
	  filename = dialog.get_filename [];
	  if func(filename,data) then
	    result = %t;
	    break;
	  end
      elseif ( rep == GTK.RESPONSE_CANCEL )
	break;
      end
  end
  dialog.destroy[];
endfunction

//  save buffer in filename
  
function result  = save_buffer (buffer, filename )
  result = %f;
  have_backup = %f;
  g_return_val_if_fail (filename ~= NULL, %f);
  bak_filename = g_strconcat (filename, "~", NULL);
  if ( g_rename (filename, bak_filename) ~= 0)
    //////////{
      if (errno ~= ENOENT)
	//////////{
	err = g_strdup_printf ("Cannot back up ''%s'' to ''%s'': %s",
	filename, bak_filename, g_strerror (errno));
	error_dialog (NULL,"%s",err);
	g_free (err);
	result = %f;
	return;
      end
  else
    have_backup = %t;
  end
  
  file = fopen (filename, "wb"); //  see porting issue bellow for binary  
  if (~file)
    //////////{
      error_dialog (NULL,"Cannot back up ''%s'' to ''%s'': %s",filename, bak_filename, g_strerror (errno));
  else
    //////////{
    buffer.get_iter_at_offset[ start, 0];
    buffer.get_end_iter[ end_iter];

    chars = buffer.get_slice[ start, end_iter, %f];

      //  porting issue for win32
      // If the stream is from a file opened in text mode, any linefeed embedded
      // in the output string is translated to carriage-return linefeed on output
        
      if (fputs (chars, file) == EOF ||
	  fclose (file) == EOF)
	//////////{
	  error_dialog (NULL,"Error writing to ''%s'': %s",filename, g_strerror (errno));
      else
	//////////{
	  //  Success
	    
	  result = %t;
	  buffer.set_modified[%f];
      end
      g_free (chars);
  end
  
  if (~result && have_backup)
    //////////{
      if ( g_rename (bak_filename, filename) ~= 0)
	//////////{
	  error_dialog (NULL,"Error restoring backup file ''%s'' to ''%s'': %s\nBackup left as ''%s''",
			filename, bak_filename, g_strerror (errno), bak_filename);
      end
  end
  g_free (bak_filename);

endfunction


function y = save_as_ok_func (filename, data)
  buffer= data;
  old_filename = buffer.get_data[ "filename"];
  if (~ old_filename  || strcmp (filename, old_filename) ~= 0)
    if (stat (filename, statbuf) == 0)
	//////////{
	err = g_strdup_printf ("Overwrite existing file ''%s''?", filename);
	dialog = gtk_dialog_new_with_buttons (err,
							   NULL,
							   GTK.DIALOG_MODAL | GTK.DIALOG_DESTROY_WITH_PARENT,
							   GTK.STOCK_OK,
							   GTK.RESPONSE_ACCEPT,
							   GTK.STOCK_CANCEL,
							   GTK.RESPONSE_REJECT,
							   NULL);
	  dialog.set_default_response[GTK.RESPONSE_OK];
	  result = gtk_dialog_run (dialog);
	  g_free (err);
	  dialog.destroy[];
	  if (result ~= GTK.RESPONSE_ACCEPT ) then y = %f;return;end 
      end
  end

  if (save_buffer (buffer, filename ))
    //  this will free old value  
    buffer.set_data[filename = filename];
    //  language may have changed  
    language = get_language (buffer, filename);
    if ( language ~= NULL)
      buffer.set_language[language];
      y = %t;return;
    end
  else
    y= %f; return;
  end
endfunction

function save_as_cb (action, user_data)
  g_return_if_fail (user_data);
  buffer = user_data;
  filechooser_save_run(NULL, "Save File", NULL, save_as_ok_func, buffer);
endfunction

function save_cb (action, user_data)
  buffer= user_data;
  filename = buffer.get_data[ "filename"];
  g_return_if_fail (user_data);
  buffer = user_data;
  if ( filename == NULL)
    save_as_cb(action,user_data);
  else
    save_buffer (buffer, filename );
  end
endfunction

function y= check_buffer_saved (action, user_data)
  buffer= user_data;
  if (buffer.get_modified [])
    //////////{
    pretty_name = buffer_pretty_name (buffer);
    msg = g_strdup_printf ("Save changes to ''%s''?", pretty_name);
    g_free (pretty_name);
    dialog = gtk_dialog_new_with_buttons (msg,
    NULL,
    GTK.DIALOG_MODAL | GTK.DIALOG_DESTROY_WITH_PARENT,
    GTK.STOCK_YES,
    GTK.RESPONSE_ACCEPT,
    GTK.STOCK_NO,
    GTK.RESPONSE_REJECT,
    GTK.STOCK_CANCEL,
    GTK.RESPONSE_REJECT,
    NULL);
    dialog.set_default_response[GTK.RESPONSE_OK];
    result = gtk_dialog_run (dialog);
    g_free (msg);
    dialog.destroy[];
    if (result == GTK.RESPONSE_REJECT ) then y= %f;return;end
    save_cb(action, user_data);
  end
  y=%t
endfunction 

function close_cb (action, user_data)
//////////{
  loop;
  window;
  buffer= user_data;
  hus = buffer.get_data[ "smatrix"];
  if ( hus ~= NULL)
    //////////{
    NspSMatrix *S;
    //  this is a smatrix buffer  
    S = save_buffer_in_smatrix(buffer);
    buffer.set_data[ "smatrix_value" = S];
    //  we do not unref buffer  
  else
    //////////{
    if ( action ~= NULL)
      check_buffer_saved (action, buffer);
      g_object_unref (buffer);
    end
  end
  window =  buffer.get_data[ "buffer_window"];
  loop = window.get_data[ "main_loop"];
  if ( loop ~= NULL) then g_main_loop_quit (loop); end 
  window.destroy[];
endfunction 

function update_cursor_position (buffer, user_data)

  // g_return_if_fail (user_data);

  view = user_data;
  tabwidth = view.get_tab_width [];
  pos_label = view.get_data[ "pos_label"];

  iter = buffer.get_iter_at_mark[ buffer.get_insert []];

  chars = iter.get_offset[];
  row = iter.get_line[] + 1;

  start = iter;
  start.set_line_offset[0];
  col = 0;
  
  while ~start.equal[iter]
    if start.get_char[] == ascii('\t') then 
      col = col + (tabwidth - (mod(col, tabwidth)));
    else
      col = col+1;
    end
    start.forward_char[];
  end

  // #ifdef HAVE_GTKSOURCEVIEW_CONTEXT_CLASS
  classes = buffer.get_context_classes_at_iter[ iter];
  str = catenate(classes,sep=", ");

  msg = sprintf ("char: %d, line: %d, column: %d, classes: %s", chars, row, col, str);
  pos_label.set_text[msg];
endfunction 

function move_cursor_cb (buffer, cursoriter,mark, user_data)
  //if (mark ~= buffer.get_insert []) then return; end
  // update_cursor_position (buffer, user_data);
endfunction

function y = window_deleted_cb (widget, ev, user_data)
  y=%f;
  return;
  view = user_data;
  // g_return_val_if_fail (user_data, %t);
  buffer = view.get_buffer [];
  windows = g_list_remove (windows, widget);
  //  deinstall buffer motion signal handlers  
  g_signal_handlers_disconnect_matched (buffer,
  G_SIGNAL_MATCH_DATA,...
  0, ... //  signal_id  
  0, ... //  detail  
  NULL, ...//  closure  
  NULL, ... //  func  
  user_data);
  //  be sure to quit the loop if edit was entered in a wait mode (asynchronous)  
  loop = widget.get_data[ "main_loop"];
  if ( loop ~= NULL) then g_main_loop_quit (loop); end 
  //  we return %f since we want the window destroyed  
  y = %f;
endfunction 
  
// #ifdef HAVE_GTKSOURCEVIEW_GUTTER
function line_mark_activated (gutter,iter, ev, view)
  mark_type = MARK_TYPE_2;
  buffer = GTK.SOURCE_BUFFER (view.get_buffer []);
  //  get the marks already in the line  
  mark_list = buffer.get_source_marks_at_line[ iter.get_line [], mark_type];
  if (mark_list ~= NULL)
    //  just take the first and delete it  
    gtk_text_buffer_delete_mark (buffer, GTK.TEXT_MARK (mark_list.data));
  else
    //  no mark found: create one  
    gtk_source_buffer_create_source_mark (buffer, NULL, mark_type, iter);
  end
  g_slist_free (mark_list);
endfunction

  
function drag_data_received_cb (widget, context, x, y, selection_data, info, timestamp, data)
  drop= selection_data.get_data [];
  Sciprintf("drop: %s\n",drop);
endfunction

//  Window creation functions   

function text = mark_tooltip_func (mark, user_data)
  buf = mark.get_buffer [];
  iter = buf.get_iter_at_mark[ iter, mark]
  line = iter.get_line[] + 1;
  column = iter.get_line_offset[]
  //    if (strcmp (mark.get_category [], MARK_TYPE_1) == 0)  
  text = printf ("<b>Line</b>: %d\n<i>Column</i>: %d", line, column);
endfunction

function add_source_mark_pixbufs (view)
  if %f then 
    color = gdk_color_new ("pink");
    // 
    view.set_mark_category_background[ MARK_TYPE_2, color];
    view.set_mark_category_icon_from_stock[MARK_TYPE_2, GTK.STOCK_NO];
    view.set_mark_category_priority[MARK_TYPE_2, 2];
    view.set_mark_category_tooltip_markup_func[ MARK_TYPE_2, ...
		    mark_tooltip_func]
  end
endfunction

function button_ok_clicked(widget, user_data)
  close_cb ("",user_data);
endfunction 

function button_cancel_clicked(widget, user_data)
  buffer= user_data;
  window =  buffer.get_data [ "buffer_window"];
  loop = window.get_data[ "main_loop"];
  if ( loop ~= NULL) then g_main_loop_quit (loop);
    window.destroy[];
  end
endfunction

function window = create_view_window (buffer, from, comment, close)
//  window  
  window = gtk_window_new (type = GTK.WINDOW_TOPLEVEL);
  window.set_border_width[0];
  window.set_title[ "GtkSourceView Demo"];

  // XXXX global ? 
  global(windows= list());
  windows($+1)= window;
  
  //  view  
  view = gtk_source_view_new_with_buffer (buffer);

  // if style_scheme then  buffer.set_style_scheme[style_scheme];end
  
  buffer.connect[ "mark-set", move_cursor_cb, view];
  buffer.connect[ "changed", update_cursor_position, view];
  // #ifdef HAVE_GTKSOURCEVIEW_GUTTER
  view.connect[   "line-mark-activated", line_mark_activated, view];
  // #endif
  // #if 0
  view.connect[ "drag_data_received", drag_data_received_cb,  view];
  // #endif

  window.connect[ "delete-event",  window_deleted_cb, view];

  //  action group and UI manager
  
  ui_manager = gtk_ui_manager_new ();
  if %f then 
    action_group = gtk_action_group_new ("ViewActions");
    action_group.add_actions [ view_action_entries, view_action_entries, view];
    
    action_group.add_toggle_actions [ toggle_entries,  toggle_entries, view];
    action_group.add_radio_actions [ tabs_radio_entries,   tabs_radio_entries,  -1, tabs_toggled_cb, view];
    action_group.add_radio_actions [ indent_radio_entries,  indent_radio_entries,  -1, indent_toggled_cb, view];
    action_group.add_radio_actions [ smart_home_end_entries,  smart_home_end_entries, -1, smart_home_end_toggled_cb, view];
    ui_managet.insert_action_group[ action_group, 0];
    //  save a reference to the ui manager in the window for later use  
  end

  window.set_data[ ui_manager= ui_manager];
  //  save a reference to the window in the buffer for later use
  // but we could have more than one window for a unique buffer
    
  buffer.set_data[ buffer_window = window];
  buffer.set_data[ buffer_view = view];
  
  accel_group = ui_manager.get_accel_group [];
  window.add_accel_group[ accel_group];
  
  ui_manager.add_ui_from_string[ catenate(view_ui_description,sep="\n"),-1];
  if %f then 
    printf("building view ui failed: %s", error.message);
    return;
  end

  //  misc widgets  
  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=0);
  sw = gtk_scrolled_window_new ()
  sw.set_shadow_type[GTK.SHADOW_IN];
  pos_label = gtk_label_new(str=str=str=str=str="Position");
  view.set_data[ pos_label = pos_label];
  if length(comment) <> 0 then 
    descr_label = gtk_label_new(str=str=str=str=str=comment);
    view.set_data[ descr_label = descr_label];
  end
  menu = ui_manager.get_widget[ "/MainMenu"];
  //  layout widgets  
  window.add[vbox];
  vbox.pack_start[menu, expand= %f, fill= %f, padding= 0];
  if length(comment) <> 0 then
    vbox.pack_start[descr_label, expand= %f, fill= %f, padding= 0];
  end
  vbox.pack_start[sw, expand= %t, fill= %t, padding= 0];
  sw.add[view];
  vbox.pack_start[pos_label, expand= %f, fill= %f, padding= 0];

  if  close then 
    hbox = gtk_hbox_new (0, %f);
    button = gtk_button_new_from_stock(GTK.STOCK_OK);
    button.connect["clicked",button_ok_clicked,buffer];
    hbox.box_pack_end [button, expand= %f, fill= %f, padding= 0];
    button = gtk_button_new_from_stock(GTK.STOCK_CANCEL);
    button.connect["clicked",button_cancel_clicked,buffer];
    hbox.pack_end[ button, expand= %f, fill= %f, padding= 0]
    vbox.pack_start[hbox, expand= %f, fill= %f, padding= 0];
  end

  //  setup view  
  if %f then
    font_desc = pango_font_description_from_string ("monospace");
    if font_desc <> "" then 
      gtk_widget_modify_font (view, font_desc);
    end
  end
  //  default value used  
  view.set_tab_width[2];
  //  change view attributes to match those of from  
  if type(from,'short')<>'m' then 
    action = action_group.get_action[ "ShowNumbers"];
    action.set_active[from.get_show_line_numbers []];
    action = action_group.get_action[ "ShowMarks"];
    action.set_active[ from.get_show_line_marks []];
    action = action_group.get_action[ "ShowMargin"];
    action.set_active[ from.get_show_right_margin []];
    action = action_group.get_action[ "HlLine"];
    action.set_active[ from.get_highlight_current_line []];
    action = action_group.get_action[ "WrapLines"];
    action.set_active[ from.get_wrap_mode [] ~= GTK.WRAP_NONE];
    action = action_group.get_action[ "AutoIndent"];
    action.set_active[ from.get_auto_indent []];
    action = action_group.get_action[ "InsertSpaces"];
     action.set_active[ from.get_insert_spaces_instead_of_tabs []);

    tmp = g_strdup_printf ("TabWidth%d", from.get_tab_width []);
    action = action_group.get_action[tmp];
    if (action)
      action.set_active[%t];
    end
    g_free (tmp);

    i = from.get_indent_width [];
    if i < 0 then 
      tmp = "IndentWidthUnset" 
    else
      tmp = sprintf("IndentWidth%d", i);
    end
    action = action_group.get_action[tmp];
    if (action) then 
      action.set_active[%t];
    end
  end
  
  // #ifdef HAVE_GTKSOURCEVIEW_GUTTER
  add_source_mark_pixbufs (view);
  // #endif

  //  update the name  
  view_set_title (view,%f);

  vbox.show_all[];
endfunction

function window= create_main_window (buffer, flag, comment,close)
  if flag then
    buffer_ui_description = buffer_file_ui_description;
  else
    buffer_ui_description = buffer_smatrix_ui_description;
  end
  window = create_view_window (buffer, [],comment,close);
  
  ui_manager = window.get_data["ui_manager"];
  
  //  buffer action group  
  if %f then 
    action_group = gtk_action_group_new ("BufferActions");
    action_group.add_actions[ buffer_action_entries, buffer_action_entries, buffer];
    ui_manager.insert_action_group[ action_group, 1];
  end
  //  merge buffer ui  
  ui_manager.add_ui_from_string[catenate(buffer_ui_description,sep='\n'), -1];
  
  //  preselect menu checkitems  
  groups = ui_manager.get_action_groups [];
  //  retrieve the view action group at position 0 in the list  

  if length(groups) >= 1 then 
      
    action_group = groups(1);
    action = action_group.get_action [ "HlBracket"];
    action.set_active[%t];

    action = action_group.get_action [ "ShowNumbers"];
    action.set_active[%t];

    action = action_group.get_action [ "ShowMarks"];
    action.set_active[%t];

    action = action_group.get_action [ "ShowMargin"];
    action.set_active[%f];
    
    action = action_group.get_action [ "AutoIndent"];
    action.set_active[%t];
    
    action = action_group.get_action [ "InsertSpaces"];
    action.set_active[%f];
    
    action = action_group.get_action [ "TabWidth2"];
    action.set_active[%t];
    
    action = action_group.get_action [ "IndentWidthUnset"];
    action.set_active[%t];
  end
endfunction 

// nsp_edit:
// @fname: file name of file to be edited or %NULL
// @read_only: a boolean
// @wait:  a boolean
//
// creates a new edit function

function nsp_edit(fname, read_only, wait)
//  create buffer  
  tag_table = gtk_text_tag_table_new()
  buffer = gtk_source_buffer_new (tag_table);
  open_file (buffer, fname);
  //  create first window  
  window = create_main_window (buffer,%t,"",%f);
  window.set_default_size[600, 400];
  window.show[];
  if  wait then 
    loop = g_main_loop_new (NULL,%f);
    window.set_data[ main_loop = loop];
    g_main_loop_run (loop);
  end
endfunction 

function mat = nsp_edit_smatrix(title, comment, S)
  wait = %t;
  //  create buffer  
  buffer = gtk_source_buffer_new ();
  open_smatrix (buffer,title,S);
  //  create first window  
  window = create_main_window (buffer,%f,comment,%t);
  window.set_default_size[600, 400];
  window.set_title[title];
  window.show[];
  if wait then 
    loop = g_main_loop_new (NULL,%f);
  end
  window.set_data[main_loop = loop];
  if wait then 
    g_main_loop_run (loop);
  end
  mat  = buffer.get_data[ "smatrix_value"];
endfunction

function nsp_sourceview_cleanup()
  g_list_foreach (windows, gtk_widget_destroy, NULL);
  g_list_free (windows);
  g_object_unref (buffer);
  g_free (style_scheme_id);
endfunction 

// list the schemes 

function test_schemes()
  sm = get_default [];
  schemes = sm.get_scheme_ids [];
  g_print ("Available style schemes:\n");
  while %t 
    style_scheme = sm.get_scheme[schemes];
    authors = style_scheme.get_authors [];
    if (authors ) then 
      authors_str = g_strjoinv (", ", authors);
      g_print (" - [%s] %s: %s\n",
      style_scheme.get_id [],
      style_scheme.get_name [],
      style_scheme.get_description []),
      if (authors_str) 
	g_print ("   by %s\n",  authors_str);
      end
      schemes= schemes +1;
    end
    g_print("\n");
  end
endfunction

// function test_get_language (void)
//   lm;
//   const gchar * const *ids;
//   lm = .get_default [];
//   ids = lm.get_language_ids [];
//   g_assert (ids ~= NULL);
//   g_print ("Available languages:\n");
//   while (*ids ~= NULL)
//     //////////{
//       lang1, *lang2;
//       g_print ("[%s]\n",*ids);
//       lang1 = lm.get_language[*ids];
//       g_assert (lang1 ~= NULL);
//       g_assert (GTK.IS_SOURCE_LANGUAGE (lang1));
//       g_assert_cmpstr (*ids, == , gtk_source_language_get_id (lang1));
//       //  langs are owned by the manager  
//       lang2 = lm.get_language[*ids];
//       g_assert (lang1 == lang2);
//       ++ids;
//     end
// endfunction 

function nsp= nsp_gtksource_language (void)
  
  nsp_lang= getenv("NSP")+"/config/language-specs";
  lm = gtk_source_language_manager_new ();
  lm.set_search_path[nsp_lang];
  nsp = lm.get_language["nsp"];
endfunction 

function view_set_title (view, read_only)
  buffer = view.get_buffer[];
  pretty_name = buffer_pretty_name (buffer);
  if read_only then 
    title = sprintf("nsp view - %s", pretty_name);
  else
    title = sprintf("nsp edit - %s", pretty_name);
  end
  window =  buffer.get_data[ "buffer_window"];
  window.set_title[title];
endfunction 
  
//  return a name to store in the view

function name = buffer_pretty_name (buffer)
  persistent(untitled_serial = 1);
  ok = execstr('filename = buffer.get_data[""filename""]',errcatch=%t);
  if ok then 
    name = file('tail',filename);
    return;
  else
    lasterror();
    ok = execstr('hus = buffer.get_data[ ""untitled_serial""]',errcatch=%t);
    if ~ok then 
      lasterror();
      us = untitled_serial;
      buffer.set_data[untitled_serial = untitled_serial];
      untitled_serial = untitled_serial+1;
    else
      us = hus;
    end
    if us == 1 then 
      name = "Untitled" 
    else
      name = sprintf ("Untitled #%d", us);
    end
  end
endfunction

