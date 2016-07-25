/*
 * Copyright (C) 2006, 2007 Apple Inc.
 * Copyright (C) 2007 Alp Toker <alp@atoker.com>
 * Copyright (C) 2008-2016 Jean-Philippe Chancelier <jpc@cermics.enpc.fr>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE COMPUTER, INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE COMPUTER, INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <gtk/gtk.h>
#include <webkit/webkit.h>

#include <nsp/nsp.h>
#include <nsp/gtksci.h>
#include <nsp/system.h>
#include <nsp/hash.h>
#include <nsp/file.h>
#include <nsp/smatrix.h>

#include "../system/regexp.h"
#include "eggfindbar.h"

#define N_(x) x

/* on windows TRUE and FALSE are undef by
 * "nsp/object.h"
 */

#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

#include <nsp/interf.h>
#include <nsp/nsptcl.h>
#include <nsp/nspthreads.h>

static GtkWidget* main_window=NULL;
int nsp_open_webkit_window (const gchar *help_path,const gchar *locale,const gchar *help_file);

typedef struct _nsp_webkit_data nsp_webkit_data;
struct _nsp_webkit_data {GAsyncQueue *queue;int ans; char *mandir,*locale,*help_file;};

static void  window_find_search_changed_cb  (GObject         *object,
					     GParamSpec      *arg1,
					     void        *window);
static void  window_find_case_changed_cb    (GObject         *object,
					     GParamSpec      *arg1,
					     void        *window);
static void  window_find_previous_cb        (GtkEntry        *entry,
					     void        *window);
static void  window_find_next_cb            (GtkEntry        *entry,
					     void        *window);
static void  window_findbar_close_cb        (GtkWidget       *widget,
					     void        *window);

static void activate_uri_entry_cb (GtkWidget* entry, gpointer data)
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  const gchar* uri = gtk_entry_get_text (GTK_ENTRY (entry));
  g_assert (uri);
  webkit_web_view_open (web_view, uri);
}

static void
update_title (GtkWindow* window, gint load_progress)
{
  gchar* main_title = g_object_get_data(G_OBJECT (main_window),"help_title");
  GString* string;
  if ( main_title == NULL) return;
  string = g_string_new (main_title);
  g_string_append (string, " - nsp help (webkit)");
  if (load_progress < 100)
    g_string_append_printf (string, " (%d%%)", load_progress);
  gchar* title = g_string_free (string, FALSE);
  gtk_window_set_title (window, title);
  g_free (title);
}

static void
link_hover_cb (WebKitWebView* page, const gchar* title, const gchar* link, gpointer data)
{
  GtkStatusbar* main_statusbar = g_object_get_data(G_OBJECT (main_window),"help_main_statusbar");
  guint status_context_id = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT (main_window),"help_status_context_id"));
  /* underflow is allowed */
  gtk_statusbar_pop (main_statusbar, status_context_id);
  if (link)
    gtk_statusbar_push (main_statusbar, status_context_id, link);
}

static void
title_change_cb (WebKitWebView* web_view, WebKitWebFrame* web_frame, const gchar* title, gpointer data)
{
  g_object_set_data_full (G_OBJECT (main_window),"help_title", g_strdup (title),
			  (GDestroyNotify) g_free);
  update_title (GTK_WINDOW (main_window),100);
}

static void
progress_change_cb (WebKitWebView* page, gint progress, gpointer data)
{
  update_title (GTK_WINDOW (main_window),progress);
}

static void
load_commit_cb (WebKitWebView* page, WebKitWebFrame* frame, gpointer data)
{
  GtkWidget* uri_entry = g_object_get_data (G_OBJECT(main_window),
					    "help_uri_entry");
  const gchar* uri = webkit_web_frame_get_uri(frame);
  if (uri)
    gtk_entry_set_text (GTK_ENTRY (uri_entry), uri);
}

static void
destroy_cb (GtkWidget* widget, gpointer data)
{
  gtk_widget_destroy(main_window);
  main_window=NULL;
}

static void
go_back_cb (GtkWidget* widget, gpointer data)
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_go_back (web_view);
}

static void
go_forward_cb (GtkWidget* widget, gpointer data)
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_go_forward (web_view);
}

#ifdef HAVE_WEBKIT_ZOOM
static void
go_zoom_in_cb (GtkWidget* widget, gpointer data)
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_zoom_in (web_view);
}

static void
go_zoom_out_cb (GtkWidget* widget, gpointer data)
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_zoom_out (web_view);
}

static void
go_zoom_100_cb (GtkWidget* widget, gpointer data)
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  double zl=webkit_web_view_get_zoom_level(web_view);
  if ( zl != 1.0)
    webkit_web_view_set_zoom_level(web_view,1.0);
}
#endif

static GtkWidget*create_browser (WebKitWebView **web_view_p)
{
  WebKitWebView *web_view;
  WebKitWebSettings * settings;
  GtkWidget* scrolled_window = gtk_scrolled_window_new (NULL, NULL);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  *web_view_p= web_view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
  g_object_set_data(G_OBJECT (main_window),"help_web_view",web_view);
  gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET (web_view));

  g_signal_connect (G_OBJECT (web_view), "title-changed", G_CALLBACK (title_change_cb), web_view);
  g_signal_connect (G_OBJECT (web_view), "load-progress-changed", G_CALLBACK (progress_change_cb), web_view);
  g_signal_connect (G_OBJECT (web_view), "load-committed", G_CALLBACK (load_commit_cb), web_view);
  g_signal_connect (G_OBJECT (web_view), "hovering-over-link", G_CALLBACK (link_hover_cb), web_view);

  /*  change default settings
   *
   */
  settings = webkit_web_settings_new();

#ifdef WIN32
  g_object_set(G_OBJECT(settings),
    /* "serif-font-family", x,
       "sans-serif-font-family", x,
    */
    "monospace-font-family", "Courier New",
    "default-font-family", "Times New Roman",
    "default-font-size", 10 ,
    "default-monospace-font-size", 10 ,
    NULL);

#else
  g_object_set(G_OBJECT(settings),
	       /*   "serif-font-family", x,
		    "sans-serif-font-family", x,
		    "monospace-font-family", x,
		    "default-font-family", x,
	       */
	       "default-font-size", 10 ,
	       "default-monospace-font-size", 10 ,
#if GTK_CHECK_VERSION(3,0,0)
#else
	       "enable-java-appleT",FALSE,
#endif
	       "enable-plugins",FALSE,
	       NULL);
#endif
  webkit_web_view_set_settings(WEBKIT_WEB_VIEW(web_view), settings);
  webkit_web_view_set_full_content_zoom(WEBKIT_WEB_VIEW(web_view), TRUE);
  return scrolled_window;
}

static GtkWidget*
create_statusbar ()
{
  guint status_context_id;
  GtkStatusbar*  main_statusbar = GTK_STATUSBAR (gtk_statusbar_new ());
  g_object_set_data(G_OBJECT(main_window),"help_main_statusbar",main_statusbar);
  status_context_id = gtk_statusbar_get_context_id (main_statusbar, "Link Hover");
  g_object_set_data(G_OBJECT (main_window),"help_status_context_id",GUINT_TO_POINTER (status_context_id));

  return (GtkWidget*)main_statusbar;
}

static GtkWidget*create_toolbar ()
{
  GtkWidget* uri_entry;
  GtkWidget* toolbar = gtk_toolbar_new ();
  GtkToolItem* item;
#if GTK_CHECK_VERSION(3,0,0)
  gtk_orientable_set_orientation (GTK_ORIENTABLE(toolbar), GTK_ORIENTATION_HORIZONTAL);
#else
  gtk_toolbar_set_orientation (GTK_TOOLBAR (toolbar), GTK_ORIENTATION_HORIZONTAL);
#endif
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_BOTH_HORIZ);

  /* the back button */
#if GTK_CHECK_VERSION(3,0,0)
  item = gtk_tool_button_new (NULL, "Back");
  gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON(item), "go-previous");
#else
  item = gtk_tool_button_new_from_stock (GTK_STOCK_GO_BACK);
#endif
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_back_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

  /* The forward button */
#if GTK_CHECK_VERSION(3,0,0)
  item = gtk_tool_button_new (NULL, "Forward");
  gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON(item),"go-next");
#else
  item = gtk_tool_button_new_from_stock (GTK_STOCK_GO_FORWARD);
#endif
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_forward_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

#ifdef HAVE_WEBKIT_ZOOM
  /* The zooms buttons */
#if GTK_CHECK_VERSION(3,0,0)
  item = gtk_tool_button_new (NULL, "zoom-in");
  gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON(item),"zoom-in");
#else
  item = gtk_tool_button_new_from_stock (GTK_STOCK_ZOOM_IN);
#endif
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_zoom_in_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);
  
#if GTK_CHECK_VERSION(3,0,0)
  item = gtk_tool_button_new (NULL, "zoom-out");
  gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON(item),"zoom-out");
#else
  item = gtk_tool_button_new_from_stock (GTK_STOCK_ZOOM_OUT);
#endif
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_zoom_out_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);
  
#if GTK_CHECK_VERSION(3,0,0)
  item = gtk_tool_button_new (NULL, "zoom-100");
  gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON(item),"zoom-original");
#else
  item = gtk_tool_button_new_from_stock (GTK_STOCK_ZOOM_100);
#endif
  g_signal_connect (G_OBJECT (item), "clicked", G_CALLBACK (go_zoom_100_cb), NULL);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);
#endif

  /* The URL entry */
  item = gtk_tool_item_new ();
  gtk_tool_item_set_expand (item, TRUE);
  uri_entry = gtk_entry_new ();
  g_object_set_data(G_OBJECT(main_window), "help_uri_entry",uri_entry);
  gtk_container_add (GTK_CONTAINER (item), uri_entry);
  g_signal_connect (G_OBJECT (uri_entry), "activate", G_CALLBACK (activate_uri_entry_cb),  (gpointer)uri_entry);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

  /* The go button */
#if GTK_CHECK_VERSION(3,0,0)
  item = gtk_tool_button_new (NULL, "ok");
  gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON(item),"go-jump");
#else
  item = gtk_tool_button_new_from_stock (GTK_STOCK_OK);
#endif
  g_signal_connect_swapped (G_OBJECT (item), "clicked", G_CALLBACK (activate_uri_entry_cb), (gpointer)uri_entry);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), item, -1);

  return toolbar;
}

static GtkWidget*
create_window ()
{
  GtkWidget* window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size (GTK_WINDOW (window), 800, 600);
  gtk_widget_set_name (window, "Nsp Help");
  g_signal_connect (G_OBJECT (window), "destroy", G_CALLBACK (destroy_cb), NULL);
  return window;
}

static void window_activate_print
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_execute_script (web_view, "print();");
}

static void window_activate_close 
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  gtk_widget_destroy(main_window);
  main_window=NULL;
}

static void window_activate_copy
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  /* window is main_window */
#if GTK_CHECK_VERSION(3,0,0)
  GtkWidget *widget =  gtk_window_get_focus (GTK_WINDOW (main_window));
#else
  GtkWidget *widget =  gtk_window_get_focus (GTK_WINDOW (window));
#endif  
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  if (GTK_IS_EDITABLE (widget))
    {
      gtk_editable_copy_clipboard (GTK_EDITABLE (widget));
    }
  else
    {
      webkit_web_view_copy_clipboard (web_view);
    }
}

static void window_activate_find
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  GtkWidget* find_bar = g_object_get_data(G_OBJECT (main_window),"help_find_bar");
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  if ( find_bar == NULL) return;
  gtk_widget_show (find_bar);
  gtk_widget_grab_focus (find_bar);
  webkit_web_view_set_highlight_text_matches (web_view, TRUE);
}

/* int window_activate_preferences(){}; */
static void  window_activate_back
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_go_back (web_view);
}

static void  window_activate_forward
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_go_forward (web_view);
}

#ifdef HAVE_WEBKIT_ZOOM
static void window_activate_zoom_in
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_zoom_in (web_view);
}

static void window_activate_zoom_out
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  webkit_web_view_zoom_out (web_view);
}

static void window_activate_zoom_default
#if GTK_CHECK_VERSION(3,0,0)
 (GSimpleAction  *action, GVariant *parameter, gpointer user_data)
#else
  (GtkAction *action,  void  *window)
#endif
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  double zl=webkit_web_view_get_zoom_level(web_view);
  if ( zl != 1.0)
    webkit_web_view_set_zoom_level(web_view,1.0);
}
#endif

static const GtkActionEntry actions[] = {
  { "FileMenu", NULL, N_("_File") },
  { "EditMenu", NULL, N_("_Edit") },
  { "ViewMenu", NULL, N_("_View") },
  { "GoMenu",   NULL, N_("_Go") },
  { "HelpMenu", NULL, N_("_Help") },
  /* File menu */
  { "Print", GTK_STOCK_PRINT, N_("_Print"), "<control>P", NULL, G_CALLBACK (window_activate_print) },
  { "Close", GTK_STOCK_CLOSE, NULL, NULL, NULL, G_CALLBACK (window_activate_close) },
  /* Edit menu */
  { "Copy", GTK_STOCK_COPY, NULL, "<control>C", NULL, G_CALLBACK (window_activate_copy) },
  { "Find", GTK_STOCK_FIND, NULL, "<control>F", NULL, G_CALLBACK (window_activate_find) },
  { "Find Next", GTK_STOCK_GO_FORWARD, N_("Find Next"), "<control>G", NULL, G_CALLBACK (window_find_next_cb) },
  { "Find Previous", GTK_STOCK_GO_BACK, N_("Find Previous"), "<shift><control>G", NULL, G_CALLBACK (window_find_previous_cb) },
  /* Go menu */
  { "Back", GTK_STOCK_GO_BACK, NULL, "<alt>Left",
    N_("Go to the previous page"),
    G_CALLBACK (window_activate_back) },
  { "Forward", GTK_STOCK_GO_FORWARD, NULL, "<alt>Right",
    N_("Go to the next page"),
    G_CALLBACK (window_activate_forward) },
  /* View menu */
#ifdef HAVE_WEBKIT_ZOOM
  { "ZoomIn", GTK_STOCK_ZOOM_IN, N_("_Larger Text"), "<ctrl>plus",
    N_("Increase the text size"),
    G_CALLBACK (window_activate_zoom_in) },
  { "ZoomOut", GTK_STOCK_ZOOM_OUT, N_("S_maller Text"), "<ctrl>minus",
    N_("Decrease the text size"),
    G_CALLBACK (window_activate_zoom_out) },
  { "ZoomDefault", GTK_STOCK_ZOOM_100, N_("_Normal Size"), "<ctrl>0",
    N_("Use the normal text size"),
    G_CALLBACK (window_activate_zoom_default) },
#endif

};

static const gchar *view_ui_description =
  "<ui>"
  "  <menubar name=\"MenuBar\">"
  "    <menu action=\"FileMenu\">"
  "      <menuitem action=\"Print\"/>"
  "      <separator/>"
  "      <menuitem action=\"Close\"/>"
  "    </menu>"
  "    <menu action=\"EditMenu\">"
  "      <menuitem action=\"Copy\"/>"
  "      <separator/>"
  "      <menuitem action=\"Find\"/>"
  "      <menuitem action=\"Find Next\"/>"
  "      <menuitem action=\"Find Previous\"/>"
  /*
  "      <separator/>"
  "      <menuitem action=\"Preferences\"/>"
  */
  "    </menu>"
#ifdef HAVE_WEBKIT_ZOOM
  "    <menu action=\"ViewMenu\">"
  "      <menuitem action=\"ZoomIn\"/>"
  "      <menuitem action=\"ZoomOut\"/>"
  "      <menuitem action=\"ZoomDefault\"/>"
  "    </menu>"
#endif
  "    <menu action=\"GoMenu\">"
  "      <menuitem action=\"Back\"/>"
  "      <menuitem action=\"Forward\"/>"
  "    </menu>"
  "  </menubar>"
  "</ui>";

int nsp_open_webkit_window (const gchar *help_path,const gchar *locale,const gchar *help_file)
{
#if GTK_CHECK_VERSION (3,0,0)
#else
#ifdef WIN32
  GdkGeometry geometry_hints;
#endif
#endif
  GtkWidget     *find_bar;
  GtkUIManager  *manager;
  GtkAccelGroup *accel_group;
  GtkWidget     *vbox, *menubar;
  GError        *error;

  if ( main_window == NULL)
    {
      WebKitWebView *web_view;
      GtkActionGroup *action_group;
      main_window = create_window ();
      manager = gtk_ui_manager_new ();
      accel_group = gtk_ui_manager_get_accel_group (manager);
      gtk_window_add_accel_group (GTK_WINDOW (main_window), accel_group);
      action_group = gtk_action_group_new ("MainWindow");
      /* gtk_action_group_set_translation_domain (action_group, GETTEXT_PACKAGE);
       */
      gtk_action_group_add_actions (action_group,actions,G_N_ELEMENTS (actions),main_window);
      gtk_ui_manager_insert_action_group (manager,action_group,    0);
      g_object_unref (action_group);

      error = NULL;
      if (!gtk_ui_manager_add_ui_from_string (manager, view_ui_description, -1, &error))
      	{
      	  g_message ("building view ui failed: %s", error->message);
      	  g_error_free (error);
      	}
      menubar = gtk_ui_manager_get_widget (manager, "/MenuBar");
#if GTK_CHECK_VERSION (3,0,0)
      vbox =gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
      vbox =gtk_vbox_new (FALSE, 0);
#endif
      gtk_box_pack_start (GTK_BOX (vbox), menubar, FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (vbox), create_toolbar (), FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (vbox), create_browser (&web_view), TRUE, TRUE, 0);
      find_bar = egg_find_bar_new ();
      g_object_set_data(G_OBJECT (main_window),"help_find_bar",find_bar);

      gtk_box_pack_start (GTK_BOX (vbox), create_statusbar (), FALSE, FALSE, 0);
      gtk_container_add (GTK_CONTAINER (main_window), vbox);
      gtk_box_pack_start (GTK_BOX (vbox), find_bar, FALSE, FALSE, 0);
      /* gtk_widget_set_no_show_all ( find_bar, TRUE); */
      /* uri = g_strconcat ("file://", help_path, "/", locale, "/", NULL); */
      webkit_web_view_open (web_view, help_file );
      g_signal_connect (find_bar,
			"notify::search-string",
			G_CALLBACK(window_find_search_changed_cb),
			main_window);
      g_signal_connect (find_bar,
			"notify::case-sensitive",
			G_CALLBACK (window_find_case_changed_cb),
			main_window);
      g_signal_connect (find_bar,
			"previous",
			G_CALLBACK (window_find_previous_cb),
			main_window);
      g_signal_connect (find_bar,
			"next",
			G_CALLBACK (window_find_next_cb),
			main_window);
      g_signal_connect (find_bar,
			"close",
			G_CALLBACK (window_findbar_close_cb),
			main_window);
      /* gtk_widget_grab_focus (GTK_WIDGET (web_view)); */

      /* set proxy uri if defined by environment variable http_proxy */
#ifdef SOUP_TYPE_PROXY_RESOLVER_DEFAULT
      soup_session_add_feature_by_type(webkit_get_default_session(),
				       SOUP_TYPE_PROXY_RESOLVER_DEFAULT);
#else
      {
	const char *httpProxy = g_getenv("http_proxy");
	if (httpProxy)
	  {
	    SoupURI *proxyUri = soup_uri_new(httpProxy);
	    g_object_set(webkit_get_default_session(),
			 SOUP_SESSION_PROXY_URI, proxyUri, NULL);
	    soup_uri_free(proxyUri);
	  }
      }
#endif
      gtk_widget_show_all (main_window);
#if GTK_CHECK_VERSION (3,0,0)
#else
#ifdef WIN32
      /* do not resiqe window on windows to prevent a crash */
      geometry_hints.min_width = 800;
      geometry_hints.max_width = 800;
      geometry_hints.min_height = 600;
      geometry_hints.max_height = 600;
      gtk_window_set_geometry_hints (GTK_WINDOW(main_window),
				     NULL,
				     &geometry_hints,
				     GDK_HINT_MAX_SIZE|GDK_HINT_MIN_SIZE);
#endif
#endif
    }
  else
    {
      WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
      webkit_web_view_open (web_view, help_file );
    }
  gtk_window_present(GTK_WINDOW(main_window));
  return 0;
}


/* handlers to interact with the egg_find_bar
 *
 */

static void
window_find_search_changed_cb (GObject    *object,
                               GParamSpec *pspec,
                               void    *window)
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  GtkWidget* find_bar = g_object_get_data(G_OBJECT (main_window),"help_find_bar");
  webkit_web_view_unmark_text_matches (web_view);
  webkit_web_view_mark_text_matches (
				     web_view,
				     egg_find_bar_get_search_string (EGG_FIND_BAR (find_bar)),
				     egg_find_bar_get_case_sensitive (EGG_FIND_BAR (find_bar)), 0);
  webkit_web_view_set_highlight_text_matches (web_view, TRUE);

        webkit_web_view_search_text (
                web_view, egg_find_bar_get_search_string (EGG_FIND_BAR (find_bar)),
                egg_find_bar_get_case_sensitive (EGG_FIND_BAR (find_bar)),
                TRUE, TRUE);
}

static void
window_find_case_changed_cb (GObject    *object,
                             GParamSpec *pspec,
			     void *window)
{
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  GtkWidget* find_bar = g_object_get_data(G_OBJECT (main_window),"help_find_bar");
  const gchar   *string;
  gboolean       case_sensitive;
  string = egg_find_bar_get_search_string (EGG_FIND_BAR (find_bar));
  case_sensitive = egg_find_bar_get_case_sensitive (EGG_FIND_BAR (find_bar));

  webkit_web_view_unmark_text_matches (web_view);
  webkit_web_view_mark_text_matches (web_view, string, case_sensitive, 0);
  webkit_web_view_set_highlight_text_matches (web_view, TRUE);
}

static void
window_find_next_cb (GtkEntry *entry,void *window)
{
  GtkWidget* find_bar = g_object_get_data(G_OBJECT (main_window),"help_find_bar");
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  const gchar   *string;
  gboolean       case_sensitive;
  gtk_widget_show (find_bar);
  string = egg_find_bar_get_search_string (EGG_FIND_BAR (find_bar));
  case_sensitive = egg_find_bar_get_case_sensitive (EGG_FIND_BAR (find_bar));
  webkit_web_view_search_text (web_view, string, case_sensitive, TRUE, TRUE);
}

static void
window_find_previous_cb (GtkEntry *entry, void *window)
{
  GtkWidget* find_bar = g_object_get_data(G_OBJECT (main_window),"help_find_bar");
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  const gchar   *string;
  gboolean       case_sensitive;
  gtk_widget_show (find_bar);
  string = egg_find_bar_get_search_string (EGG_FIND_BAR (find_bar));
  case_sensitive = egg_find_bar_get_case_sensitive (EGG_FIND_BAR (find_bar));
  webkit_web_view_search_text (web_view, string, case_sensitive, FALSE, TRUE);
}

static void
window_findbar_close_cb (GtkWidget *widget, void  *window)
{
  GtkWidget* find_bar = g_object_get_data(G_OBJECT (main_window),"help_find_bar");
  WebKitWebView* web_view= g_object_get_data(G_OBJECT (main_window),"help_web_view");
  gtk_widget_hide (find_bar);
  webkit_web_view_set_highlight_text_matches (web_view, FALSE);
}
