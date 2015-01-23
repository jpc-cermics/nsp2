/* A toolbar for nsp graphic windows 
 */

extern int nsp_call_predefined_callbacks(BCG *Xgc, const char *name, int winid);

#if 0
/* WIP */

static void
activate_action (GtkAction *action, void *user_data)
{
  static char buf[256];
  BCG *dd= user_data;
  const gchar *name = gtk_action_get_name (action);
  const gchar *typename = G_OBJECT_TYPE_NAME (action);
  g_message ("Action %s (type=%s) activated", name, typename);
  if ( nsp_call_predefined_callbacks(user_data, name, dd->CurWindow) == 1) 
    return;
  sprintf(buf,"scicos_tb(%s,%d)",name,dd->CurWindow);
  enqueue_nsp_command(buf);
}

static void
toggle_action (GtkAction *action)
{
  const gchar *name = gtk_action_get_name (action);
  const gchar *typename = G_OBJECT_TYPE_NAME (action);
  g_message ("Action %s (type=%s) activated (active=%d)", name, typename,
	     gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)));
}

/* convenience functions for declaring actions */
static GtkActionEntry entries[] = {
  { "Cut", GTK_STOCK_CUT, "C_ut", "<control>X",
    "Cut the selected text to the clipboard", G_CALLBACK (activate_action) },
  { "Copy", GTK_STOCK_COPY, "_Copy", "<control>C",
    "Copy the selected text to the clipboard", G_CALLBACK (activate_action) },
  { "Paste", GTK_STOCK_PASTE, "_Paste", "<control>V",
    "Paste the text from the clipboard", G_CALLBACK (activate_action) },
  { "Zoom out", GTK_STOCK_ZOOM_OUT, "Zoom out", NULL,
    "Zoom out", G_CALLBACK (activate_action) },
  { "Zoom in", GTK_STOCK_ZOOM_IN, "Zoom in", NULL ,
    "Zoom in", G_CALLBACK (activate_action) },
  { "Zoom 100", GTK_STOCK_ZOOM_100, "Zoom 100", NULL,
    "Zoom 100", G_CALLBACK (activate_action) },
  { "Fit diagram to figure", GTK_STOCK_ZOOM_FIT, "Zoom fit", NULL,
    "Zoom fit", G_CALLBACK (activate_action) },
  { "quit", GTK_STOCK_QUIT,  NULL, "<control>Q",
    "Quit the application", G_CALLBACK (gtk_main_quit) },
  { "Run", GTK_STOCK_MEDIA_PLAY,  NULL, NULL,
    "Run", G_CALLBACK (activate_action) },
  { "$scicos_stop", GTK_STOCK_MEDIA_STOP,  NULL, NULL,
    "Stop simulation", G_CALLBACK (activate_action) },
};

static guint n_entries = G_N_ELEMENTS (entries);

static GtkToggleActionEntry toggle_entries[] = {
  { "bold", GTK_STOCK_BOLD, "_Bold", "<control>B",
    "Change to bold face", 
    G_CALLBACK (toggle_action), FALSE },
};

static guint n_toggle_entries = G_N_ELEMENTS (toggle_entries);

/* XML description of the menus for the test app.  The parser understands
 * a subset of the Bonobo UI XML format, and uses GMarkup for parsing */

static const gchar *ui_info =
  "  <toolbar name=\"Toolbar\">\n"
  "    <toolitem name=\"zoom fit\" action=\"Fit diagram to figure\" />\n"
  "    <toolitem name=\"zoom out\" action=\"Zoom out\" />\n"
  "    <toolitem name=\"zoom in\" action=\"Zoom in\" />\n"
  "    <toolitem name=\"zoom 100\" action=\"Zoom 100\" />\n"
  "    <separator name=\"sep11\" />\n"
  "    <toolitem name=\"cut\" action=\"Cut\" />\n"
  "    <toolitem name=\"copy\" action=\"Copy\" />\n"
  "    <toolitem name=\"paste\" action=\"Paste\" />\n"
  "    <separator name=\"sep10\" />\n"
  "    <toolitem name=\"run\" action=\"Run\" />\n"
  "    <toolitem name=\"stop\" action=\"$scicos_stop\" />\n"
  "    <toolitem name=\"quit\" action=\"quit\" />\n"
  "  </toolbar>\n";

static void nsp_gwin_add_ui_manager(BCG *dd,GtkWidget *box )
{
  GtkToolbar *toolbar = NULL;
  GtkUIManager *merge;
  GtkWidget *tb, *window = dd->private->window;
  GError *error = NULL;
  GtkActionGroup *action_group = gtk_action_group_new ("TestActions");
  gtk_action_group_add_actions (action_group, 
				entries, n_entries, 
				dd);
  gtk_action_group_add_toggle_actions (action_group, 
				       toggle_entries, n_toggle_entries, 
				       dd);
  merge = gtk_ui_manager_new ();
  gtk_ui_manager_insert_action_group (merge, action_group, 0);

  gtk_window_add_accel_group (GTK_WINDOW (window), 
			      gtk_ui_manager_get_accel_group (merge));
  
  if (!gtk_ui_manager_add_ui_from_string (merge, ui_info, -1, &error))
    {
      g_message ("building menus failed: %s", error->message);
      g_error_free (error);
    }
  /* explicitely insert the toolbar */
  tb = gtk_ui_manager_get_widget (merge, "/Toolbar");
  toolbar = GTK_TOOLBAR (tb);
  gtk_toolbar_set_show_arrow (toolbar, TRUE);
  /* gtk_toolbar_set_icon_size (toolbar, GTK_ICON_SIZE_SMALL_TOOLBAR);*/
  gtk_box_pack_start (GTK_BOX (box), tb, FALSE, TRUE, 0);  
  gtk_widget_show(tb);
}
#endif 


