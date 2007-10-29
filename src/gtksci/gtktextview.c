/* testtext.c
 * Copyright (C) 2000 Red Hat, Inc
 * Author: Havoc Pennington
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#undef GTK_DISABLE_DEPRECATED

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

typedef struct _Buffer Buffer;
typedef struct _View View;

static gint untitled_serial = 1;

GSList *active_window_stack = NULL;

struct _Buffer
{
  gint refcount;
  GtkTextBuffer *buffer;
  char *filename;
  gint untitled_serial;
  GtkTextTag *not_editable_tag;
  GtkTextTag *found_text_tag;
  GtkTextTag *rise_tag;
  GtkTextTag *large_tag;
  GtkTextTag *indent_tag;
  GtkTextTag *margin_tag;
  GtkTextTag *custom_tabs_tag;
  GSList *color_tags;
  guint color_cycle_timeout;
  gdouble start_hue;
};

struct _View
{
  GtkWidget *window;
  GtkWidget *text_view;
  GtkAccelGroup *accel_group;
  GtkItemFactory *item_factory;
  Buffer *buffer;
};

static void push_active_window (GtkWindow *window);
static void pop_active_window (void);
static GtkWindow *get_active_window (void);

static Buffer * create_buffer      (void);
static gboolean check_buffer_saved (Buffer *buffer);
static gboolean save_buffer        (Buffer *buffer);
static gboolean save_as_buffer     (Buffer *buffer);
static char *   buffer_pretty_name (Buffer *buffer);
static void     buffer_filename_set (Buffer *buffer);
static void     buffer_search_forward (Buffer *buffer,
                                       const char *str,
                                       View *view);
static void     buffer_search_backward (Buffer *buffer,
                                       const char *str,
                                       View *view);
static void     buffer_set_colors      (Buffer  *buffer,
                                        gboolean enabled);
static void     buffer_cycle_colors    (Buffer  *buffer);

static View *view_from_widget (GtkWidget *widget);

static View *create_view      (Buffer *buffer);
static void  check_close_view (View   *view);
static void  close_view       (View   *view);
static void  view_set_title   (View   *view);

GSList *buffers = NULL;
GSList *views = NULL;

static void
push_active_window (GtkWindow *window)
{
  g_object_ref (window);
  active_window_stack = g_slist_prepend (active_window_stack, window);
}

static void
pop_active_window (void)
{
  g_object_unref (active_window_stack->data);
  active_window_stack = g_slist_delete_link (active_window_stack, active_window_stack);
}

static GtkWindow *
get_active_window (void)
{
  if (active_window_stack)
    return active_window_stack->data;
  else
    return NULL;
}

/*
 * Filesel utility function
 */

typedef gboolean (*FileselOKFunc) (const char *filename, gpointer data);

static void
filesel_ok_cb (GtkWidget *button, GtkWidget *filesel)
{
  FileselOKFunc ok_func = (FileselOKFunc)g_object_get_data (G_OBJECT (filesel), "ok-func");
  gpointer data = g_object_get_data (G_OBJECT (filesel), "ok-data");
  gint *result = g_object_get_data (G_OBJECT (filesel), "ok-result");
  
  gtk_widget_hide (filesel);
  
  if ((*ok_func) (gtk_file_selection_get_filename (GTK_FILE_SELECTION (filesel)), data))
    {
      gtk_widget_destroy (filesel);
      *result = TRUE;
    }
  else
    gtk_widget_show (filesel);
}

gboolean
filesel_run (GtkWindow    *parent, 
	     const char   *title,
	     const char   *start_file,
	     FileselOKFunc func,
	     gpointer      data)
{
  GtkWidget *filesel = gtk_file_selection_new (title);
  gboolean result = FALSE;

  if (!parent)
    parent = get_active_window ();
  
  if (parent)
    gtk_window_set_transient_for (GTK_WINDOW (filesel), parent);

  if (start_file)
    gtk_file_selection_set_filename (GTK_FILE_SELECTION (filesel), start_file);

  
  g_object_set_data (G_OBJECT (filesel), "ok-func", func);
  g_object_set_data (G_OBJECT (filesel), "ok-data", data);
  g_object_set_data (G_OBJECT (filesel), "ok-result", &result);

  g_signal_connect (GTK_FILE_SELECTION (filesel)->ok_button,
		    "clicked",
		    G_CALLBACK (filesel_ok_cb), filesel);
  g_signal_connect_swapped (GTK_FILE_SELECTION (filesel)->cancel_button,
			    "clicked",
			    G_CALLBACK (gtk_widget_destroy), filesel);

  g_signal_connect (filesel, "destroy",
		    G_CALLBACK (gtk_main_quit), NULL);

  gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);

  gtk_widget_show (filesel);
  gtk_main ();

  return result;
}

/*
 * MsgBox utility functions
 */

static void
msgbox_yes_cb (GtkWidget *widget, gboolean *result)
{
  *result = 0;
  gtk_object_destroy (GTK_OBJECT (gtk_widget_get_toplevel (widget)));
}

static void
msgbox_no_cb (GtkWidget *widget, gboolean *result)
{
  *result = 1;
  gtk_object_destroy (GTK_OBJECT (gtk_widget_get_toplevel (widget)));
}

static gboolean
msgbox_key_press_cb (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
  fprintf(stderr,"key pressed \n");
  if (event->keyval == GDK_Escape)
    {
      g_signal_stop_emission_by_name (widget, "key_press_event");
      gtk_object_destroy (GTK_OBJECT (widget));
      return TRUE;
    }

  return FALSE;
}

/* Don't copy this example, it's all crack-smoking - you can just use
 * GtkMessageDialog now
 */
gint
msgbox_run (GtkWindow  *parent,
	    const char *message,
	    const char *yes_button,
	    const char *no_button,
	    const char *cancel_button,
	    gint default_index)
{
  gboolean result = -1;
  GtkWidget *dialog;
  GtkWidget *button;
  GtkWidget *label;
  GtkWidget *vbox;
  GtkWidget *button_box;
  GtkWidget *separator;

  g_return_val_if_fail (message != NULL, FALSE);
  g_return_val_if_fail (default_index >= 0 && default_index <= 1, FALSE);

  if (!parent)
    parent = get_active_window ();
  
  /* Create a dialog
   */
  dialog = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  if (parent)
    gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);

  /* Quit our recursive main loop when the dialog is destroyed.
   */
  g_signal_connect (dialog, "destroy",
		    G_CALLBACK (gtk_main_quit), NULL);

  /* Catch Escape key presses and have them destroy the dialog
   */
  g_signal_connect (dialog, "key_press_event",
		    G_CALLBACK (msgbox_key_press_cb), NULL);

  /* Fill in the contents of the widget
   */
  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (dialog), vbox);
  
  label = gtk_label_new (message);
  gtk_misc_set_padding (GTK_MISC (label), 12, 12);
  gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
  gtk_box_pack_start (GTK_BOX (vbox), label, TRUE, TRUE, 0);

  separator = gtk_hseparator_new ();
  gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, FALSE, 0);

  button_box = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (vbox), button_box, FALSE, FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (button_box), 8);
  

  /* When Yes is clicked, call the msgbox_yes_cb
   * This sets the result variable and destroys the dialog
   */
  if (yes_button)
    {
      button = gtk_button_new_with_label (yes_button);
      GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
      gtk_container_add (GTK_CONTAINER (button_box), button);

      if (default_index == 0)
	gtk_widget_grab_default (button);
      
      g_signal_connect (button, "clicked",
			G_CALLBACK (msgbox_yes_cb), &result);
    }

  /* When No is clicked, call the msgbox_no_cb
   * This sets the result variable and destroys the dialog
   */
  if (no_button)
    {
      button = gtk_button_new_with_label (no_button);
      GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
      gtk_container_add (GTK_CONTAINER (button_box), button);

      if (default_index == 0)
	gtk_widget_grab_default (button);
      
      g_signal_connect (button, "clicked",
			G_CALLBACK (msgbox_no_cb), &result);
    }

  /* When Cancel is clicked, destroy the dialog
   */
  if (cancel_button)
    {
      button = gtk_button_new_with_label (cancel_button);
      GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
      gtk_container_add (GTK_CONTAINER (button_box), button);
      
      if (default_index == 1)
	gtk_widget_grab_default (button);
      
      g_signal_connect_swapped (button, "clicked",
				G_CALLBACK (gtk_object_destroy), dialog);
    }

  gtk_widget_show_all (dialog);

  /* Run a recursive main loop until a button is clicked
   * or the user destroys the dialog through the window mananger */
  gtk_main ();

  return result;
}

#ifdef DO_BLINK
/*
 * Example buffer filling code
 */
static gint
blink_timeout (gpointer data)
{
  GtkTextTag *tag;
  static gboolean flip = FALSE;
  
  tag = GTK_TEXT_TAG (data);

  g_object_set (tag,
                 "foreground", flip ? "blue" : "purple",
                 NULL);

  flip = !flip;

  return TRUE;
}
#endif


gboolean
fill_file_buffer (GtkTextBuffer *buffer, const char *filename)
{
  FILE* f;
  gchar buf[2048];
  gint remaining = 0;
  GtkTextIter iter, end;

  f = fopen (filename, "r");
  
  if (f == NULL)
    {
      gchar *err = g_strdup_printf ("Cannot open file '%s': %s",
				    filename, g_strerror (errno));
      msgbox_run (NULL, err, "OK", NULL, NULL, 0);
      g_free (err);
      return FALSE;
    }
  
  gtk_text_buffer_get_iter_at_offset (buffer, &iter, 0);
  while (!feof (f))
    {
      gint count;
      const char *leftover;
      int to_read = 2047  - remaining;

      count = fread (buf + remaining, 1, to_read, f);
      buf[count + remaining] = '\0';

      g_utf8_validate (buf, count + remaining, &leftover);
      
      g_assert (g_utf8_validate (buf, leftover - buf, NULL));
      gtk_text_buffer_insert (buffer, &iter, buf, leftover - buf);

      remaining = (buf + remaining + count) - leftover;
      g_memmove (buf, leftover, remaining);

      if (remaining > 6 || count < to_read)
	  break;
    }

  if (remaining)
    {
      gchar *err = g_strdup_printf ("Invalid UTF-8 data encountered reading file '%s'", filename);
      msgbox_run (NULL, err, "OK", NULL, NULL, 0);
      g_free (err);
    }
  
  /* We had a newline in the buffer to begin with. (The buffer always contains
   * a newline, so we delete to the end of the buffer to clean up.
   */
  gtk_text_buffer_get_end_iter (buffer, &end);
  gtk_text_buffer_delete (buffer, &iter, &end);
  
  gtk_text_buffer_set_modified (buffer, FALSE);

  return TRUE;
}


static gint
delete_event_cb (GtkWidget *window, GdkEventAny *event, gpointer data)
{
  View *view = view_from_widget (window);

  push_active_window (GTK_WINDOW (window));
  check_close_view (view);
  pop_active_window ();

  return TRUE;
}

/*
 * Menu callbacks
 */

static View *
get_empty_view (View *view)
{
  if (!view->buffer->filename &&
      !gtk_text_buffer_get_modified (view->buffer->buffer))
    return view;
  else
    return create_view (create_buffer ());
}

static View *
view_from_widget (GtkWidget *widget)
{
  if (GTK_IS_MENU_ITEM (widget))
    {
      GtkItemFactory *item_factory = gtk_item_factory_from_widget (widget);
      return g_object_get_data (G_OBJECT (item_factory), "view");      
    }
  else
    {
      GtkWidget *app = gtk_widget_get_toplevel (widget);
      return g_object_get_data (G_OBJECT (app), "view");
    }
}


gboolean
open_ok_func (const char *filename, gpointer data)
{
  View *view = data;
  View *new_view = get_empty_view (view);

  if (!fill_file_buffer (new_view->buffer->buffer, filename))
    {
      if (new_view != view)
	close_view (new_view);
      return FALSE;
    }
  else
    {
      g_free (new_view->buffer->filename);
      new_view->buffer->filename = g_strdup (filename);
      buffer_filename_set (new_view->buffer);
      
      return TRUE;
    }
}

static void
do_open (gpointer             callback_data,
	 guint                callback_action,
	 GtkWidget           *widget)
{
  View *view = view_from_widget (widget);

  push_active_window (GTK_WINDOW (view->window));
  filesel_run (NULL, "Open File", NULL, open_ok_func, view);
  pop_active_window ();
}

static void
do_save_as (gpointer             callback_data,
	    guint                callback_action,
	    GtkWidget           *widget)
{
  View *view = view_from_widget (widget);  

  push_active_window (GTK_WINDOW (view->window));
  save_as_buffer (view->buffer);
  pop_active_window ();
}

static void
do_save (gpointer             callback_data,
	 guint                callback_action,
	 GtkWidget           *widget)
{
  View *view = view_from_widget (widget);

  push_active_window (GTK_WINDOW (view->window));
  if (!view->buffer->filename)
    do_save_as (callback_data, callback_action, widget);
  else
    save_buffer (view->buffer);
  pop_active_window ();
}

static void
do_close   (gpointer             callback_data,
	    guint                callback_action,
	    GtkWidget           *widget)
{
  View *view = view_from_widget (widget);

  push_active_window (GTK_WINDOW (view->window));
  check_close_view (view);
  pop_active_window ();
}

static void
do_exit    (gpointer             callback_data,
	    guint                callback_action,
	    GtkWidget           *widget)
{
  View *view = view_from_widget (widget);

  GSList *tmp_list = buffers;

  push_active_window (GTK_WINDOW (view->window));
  while (tmp_list)
    {
      if (!check_buffer_saved (tmp_list->data))
	return;

      tmp_list = tmp_list->next;
    }

  /* gtk_main_quit (); */
  pop_active_window ();
}


enum
{
  RESPONSE_FORWARD,
  RESPONSE_BACKWARD
};

static void
dialog_response_callback (GtkWidget *dialog, gint response_id, gpointer data)
{
  GtkTextBuffer *buffer;
  View *view = data;
  GtkTextIter start, end;
  gchar *search_string;

  if (response_id != RESPONSE_FORWARD &&
      response_id != RESPONSE_BACKWARD)
    {
      gtk_widget_destroy (dialog);
      return;
    }
  
  buffer = g_object_get_data (G_OBJECT (dialog), "buffer");

  gtk_text_buffer_get_bounds (buffer, &start, &end);
  
  search_string = gtk_text_iter_get_text (&start, &end);

  g_print ("Searching for `%s'\n", search_string);

  if (response_id == RESPONSE_FORWARD)
    buffer_search_forward (view->buffer, search_string, view);
  else if (response_id == RESPONSE_BACKWARD)
    buffer_search_backward (view->buffer, search_string, view);
    
  g_free (search_string);
  
  gtk_widget_destroy (dialog);
}

static void
do_search (gpointer callback_data,
           guint callback_action,
           GtkWidget *widget)
{
  View *view = view_from_widget (widget);
  GtkWidget *dialog;
  GtkWidget *search_text;
  GtkTextBuffer *buffer;

  dialog = gtk_dialog_new_with_buttons ("Search",
                                        GTK_WINDOW (view->window),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        "Forward", RESPONSE_FORWARD,
                                        "Backward", RESPONSE_BACKWARD,
                                        GTK_STOCK_CANCEL,
                                        GTK_RESPONSE_NONE, NULL);


  buffer = gtk_text_buffer_new (NULL);

  search_text = gtk_text_view_new_with_buffer (buffer);

  g_object_unref (buffer);
  
  gtk_box_pack_end (GTK_BOX (GTK_DIALOG (dialog)->vbox),
                    search_text,
                    TRUE, TRUE, 0);

  g_object_set_data (G_OBJECT (dialog), "buffer", buffer);
  
  g_signal_connect (dialog,
                    "response",
                    G_CALLBACK (dialog_response_callback),
                    view);

  gtk_widget_show (search_text);

  gtk_widget_grab_focus (search_text);
  
  gtk_widget_show_all (dialog);
}

static void
do_select_all (gpointer callback_data,
               guint callback_action,
               GtkWidget *widget)
{
  View *view = view_from_widget (widget);
  GtkTextBuffer *buffer;
  GtkTextIter start, end;

  buffer = view->buffer->buffer;

  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_select_range (buffer, &start, &end);
}



static GtkItemFactoryEntry menu_items[] =
{
  { "/_File",            NULL,         NULL,        0, "<Branch>" },
  { "/File/_Open",       "<control>O", do_open,     0, NULL },
  { "/File/_Save",       "<control>S", do_save,     0, NULL },
  { "/File/Save _As...", NULL,         do_save_as,  0, NULL },
  { "/File/sep1",        NULL,         NULL,        0, "<Separator>" },
  { "/File/_Close",     "<control>W" , do_close,    0, NULL },
  { "/File/E_xit",      "<control>Q" , do_exit,     0, NULL },

  { "/_Edit", NULL, 0, 0, "<Branch>" },
  { "/Edit/Find...", NULL, do_search, 0, NULL },
  { "/Edit/Select All", NULL , do_select_all, 0, NULL }, 
};

static gboolean
save_buffer (Buffer *buffer)
{
  GtkTextIter start, end;
  gchar *chars;
  gboolean result = FALSE;
  gboolean have_backup = FALSE;
  gchar *bak_filename;
  FILE *file;

  g_return_val_if_fail (buffer->filename != NULL, FALSE);

  bak_filename = g_strconcat (buffer->filename, "~", NULL);
  
  if (rename (buffer->filename, bak_filename) != 0)
    {
      if (errno != ENOENT)
	{
	  gchar *err = g_strdup_printf ("Cannot back up '%s' to '%s': %s",
					buffer->filename, bak_filename, g_strerror (errno));
	  msgbox_run (NULL, err, "OK", NULL, NULL, 0);
	  g_free (err);
          return FALSE;
	}
    }
  else
    have_backup = TRUE;
  
  file = fopen (buffer->filename, "w");
  if (!file)
    {
      gchar *err = g_strdup_printf ("Cannot back up '%s' to '%s': %s",
				    buffer->filename, bak_filename, g_strerror (errno));
      msgbox_run (NULL, err, "OK", NULL, NULL, 0);
    }
  else
    {
      gtk_text_buffer_get_iter_at_offset (buffer->buffer, &start, 0);
      gtk_text_buffer_get_end_iter (buffer->buffer, &end);
  
      chars = gtk_text_buffer_get_slice (buffer->buffer, &start, &end, FALSE);

      if (fputs (chars, file) == EOF ||
	  fclose (file) == EOF)
	{
	  gchar *err = g_strdup_printf ("Error writing to '%s': %s",
					buffer->filename, g_strerror (errno));
	  msgbox_run (NULL, err, "OK", NULL, NULL, 0);
	  g_free (err);
	}
      else
	{
	  /* Success
	   */
	  result = TRUE;
	  gtk_text_buffer_set_modified (buffer->buffer, FALSE);	  
	}
	
      g_free (chars);
    }

  if (!result && have_backup)
    {
      if (rename (bak_filename, buffer->filename) != 0)
	{
	  gchar *err = g_strdup_printf ("Error restoring backup file '%s' to '%s': %s\nBackup left as '%s'",
					buffer->filename, bak_filename, g_strerror (errno), bak_filename);
	  msgbox_run (NULL, err, "OK", NULL, NULL, 0);
	  g_free (err);
	}
    }

  g_free (bak_filename);
  
  return result;
}

static gboolean
save_as_ok_func (const char *filename, gpointer data)
{
  Buffer *buffer = data;
  char *old_filename = buffer->filename;

  if (!buffer->filename || strcmp (filename, buffer->filename) != 0)
    {
      struct stat statbuf;

      if (stat (filename, &statbuf) == 0)
	{
	  gchar *err = g_strdup_printf ("Ovewrite existing file '%s'?", filename);
	  gint result = msgbox_run (NULL, err, "Yes", "No", NULL, 1);
	  g_free (err);

	  if (result != 0)
	    return FALSE;
	}
    }
  
  buffer->filename = g_strdup (filename);

  if (save_buffer (buffer))
    {
      g_free (old_filename);
      buffer_filename_set (buffer);
      return TRUE;
    }
  else
    {
      g_free (buffer->filename);
      buffer->filename = old_filename;
      return FALSE;
    }
}

static gboolean
save_as_buffer (Buffer *buffer)
{
  return filesel_run (NULL, "Save File", NULL, save_as_ok_func, buffer);
}

static gboolean
check_buffer_saved (Buffer *buffer)
{
  if (gtk_text_buffer_get_modified (buffer->buffer))
    {
      char *pretty_name = buffer_pretty_name (buffer);
      char *msg = g_strdup_printf ("Save changes to '%s'?", pretty_name);
      gint result;
      
      g_free (pretty_name);
      
      result = msgbox_run (NULL, msg, "Yes", "No", "Cancel", 0);
      g_free (msg);
  
      if (result == 0)
	return save_as_buffer (buffer);
      else if (result == 1)
	return TRUE;
      else
	return FALSE;
    }
  else
    return TRUE;
}

#define N_COLORS 16

static Buffer *
create_buffer (void)
{
  Buffer *buffer;
  PangoTabArray *tabs;
  gint i;
  
  buffer = g_new (Buffer, 1);

  buffer->buffer = gtk_text_buffer_new (NULL);
  
  buffer->refcount = 1;
  buffer->filename = NULL;
  buffer->untitled_serial = -1;

  buffer->color_tags = NULL;
  buffer->color_cycle_timeout = 0;
  buffer->start_hue = 0.0;
  
  i = 0;
  while (i < N_COLORS)
    {
      GtkTextTag *tag;

      tag = gtk_text_buffer_create_tag (buffer->buffer, NULL, NULL);
      
      buffer->color_tags = g_slist_prepend (buffer->color_tags, tag);
      
      ++i;
    }
  
  buffer->not_editable_tag =
    gtk_text_buffer_create_tag (buffer->buffer, NULL,
                                "editable", FALSE,
                                "foreground", "purple", NULL);

  buffer->found_text_tag = gtk_text_buffer_create_tag (buffer->buffer, NULL,
                                                       "foreground", "red", NULL);

  buffer->rise_tag = gtk_text_buffer_create_tag (buffer->buffer, NULL,
						 "rise", 10 * PANGO_SCALE, NULL);

  buffer->large_tag = gtk_text_buffer_create_tag (buffer->buffer, NULL,
						 "scale", PANGO_SCALE_X_LARGE, NULL);

  buffer->indent_tag = gtk_text_buffer_create_tag (buffer->buffer, NULL,
						   "indent", 20, NULL);

  buffer->margin_tag = gtk_text_buffer_create_tag (buffer->buffer, NULL,
						   "left_margin", 20, "right_margin", 20, NULL);

  tabs = pango_tab_array_new_with_positions (4,
                                             TRUE,
                                             PANGO_TAB_LEFT, 10,
                                             PANGO_TAB_LEFT, 30,
                                             PANGO_TAB_LEFT, 60,
                                             PANGO_TAB_LEFT, 120);
  
  buffer->custom_tabs_tag = gtk_text_buffer_create_tag (buffer->buffer, NULL,
                                                        "tabs", tabs,
                                                        "foreground", "green", NULL);

  pango_tab_array_free (tabs);
  
  buffers = g_slist_prepend (buffers, buffer);
  
  return buffer;
}

static char *
buffer_pretty_name (Buffer *buffer)
{
  if (buffer->filename)
    {
      char *p;
      char *result = g_path_get_basename (buffer->filename);
      p = strchr (result, '/');
      if (p)
	*p = '\0';

      return result;
    }
  else
    {
      if (buffer->untitled_serial == -1)
	buffer->untitled_serial = untitled_serial++;

      if (buffer->untitled_serial == 1)
	return g_strdup ("Untitled");
      else
	return g_strdup_printf ("Untitled #%d", buffer->untitled_serial);
    }
}

static void
buffer_filename_set (Buffer *buffer)
{
  GSList *tmp_list = views;

  while (tmp_list)
    {
      View *view = tmp_list->data;

      if (view->buffer == buffer)
	view_set_title (view);

      tmp_list = tmp_list->next;
    }
}

static void
buffer_search (Buffer     *buffer,
               const char *str,
               View       *view,
               gboolean forward)
{
  GtkTextIter iter;
  GtkTextIter start, end;
  GtkWidget *dialog;
  int i;
  
  /* remove tag from whole buffer */
  gtk_text_buffer_get_bounds (buffer->buffer, &start, &end);
  gtk_text_buffer_remove_tag (buffer->buffer,  buffer->found_text_tag,
                              &start, &end );
  
  gtk_text_buffer_get_iter_at_mark (buffer->buffer, &iter,
                                    gtk_text_buffer_get_mark (buffer->buffer,
                                                              "insert"));

  i = 0;
  if (*str != '\0')
    {
      GtkTextIter match_start, match_end;

      if (forward)
        {
          while (gtk_text_iter_forward_search (&iter, str,
                                               GTK_TEXT_SEARCH_VISIBLE_ONLY |
                                               GTK_TEXT_SEARCH_TEXT_ONLY,
                                               &match_start, &match_end,
                                               NULL))
            {
              ++i;
              gtk_text_buffer_apply_tag (buffer->buffer, buffer->found_text_tag,
                                         &match_start, &match_end);
              
              iter = match_end;
            }
        }
      else
        {
          while (gtk_text_iter_backward_search (&iter, str,
                                                GTK_TEXT_SEARCH_VISIBLE_ONLY |
                                                GTK_TEXT_SEARCH_TEXT_ONLY,
                                                &match_start, &match_end,
                                                NULL))
            {
              ++i;
              gtk_text_buffer_apply_tag (buffer->buffer, buffer->found_text_tag,
                                         &match_start, &match_end);
              
              iter = match_start;
            }
        }
    }

  dialog = gtk_message_dialog_new (GTK_WINDOW (view->window),
				   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   "%d strings found and marked in red",
                                   i);

  g_signal_connect_swapped (dialog,
                            "response",
                            G_CALLBACK (gtk_widget_destroy), dialog);
  
  gtk_widget_show (dialog);
}

static void
buffer_search_forward (Buffer *buffer, const char *str,
                       View *view)
{
  buffer_search (buffer, str, view, TRUE);
}

static void
buffer_search_backward (Buffer *buffer, const char *str,
                        View *view)
{
  buffer_search (buffer, str, view, FALSE);
}

static void
buffer_ref (Buffer *buffer)
{
  buffer->refcount++;
}

static void
buffer_unref (Buffer *buffer)
{
  buffer->refcount--;
  if (buffer->refcount == 0)
    {
      buffer_set_colors (buffer, FALSE);
      buffers = g_slist_remove (buffers, buffer);
      g_object_unref (buffer->buffer);
      g_free (buffer->filename);
      g_free (buffer);
    }
}

static void
hsv_to_rgb (gdouble *h,
	    gdouble *s,
	    gdouble *v)
{
  gdouble hue, saturation, value;
  gdouble f, p, q, t;

  if (*s == 0.0)
    {
      *h = *v;
      *s = *v;
      *v = *v; /* heh */
    }
  else
    {
      hue = *h * 6.0;
      saturation = *s;
      value = *v;
      
      if (hue >= 6.0)
	hue = 0.0;
      
      f = hue - (int) hue;
      p = value * (1.0 - saturation);
      q = value * (1.0 - saturation * f);
      t = value * (1.0 - saturation * (1.0 - f));
      
      switch ((int) hue)
	{
	case 0:
	  *h = value;
	  *s = t;
	  *v = p;
	  break;
	  
	case 1:
	  *h = q;
	  *s = value;
	  *v = p;
	  break;
	  
	case 2:
	  *h = p;
	  *s = value;
	  *v = t;
	  break;
	  
	case 3:
	  *h = p;
	  *s = q;
	  *v = value;
	  break;
	  
	case 4:
	  *h = t;
	  *s = p;
	  *v = value;
	  break;
	  
	case 5:
	  *h = value;
	  *s = p;
	  *v = q;
	  break;
	  
	default:
	  g_assert_not_reached ();
	}
    }
}

static void
hue_to_color (gdouble   hue,
              GdkColor *color)
{
  gdouble h, s, v;

  h = hue;
  s = 1.0;
  v = 1.0;

  g_return_if_fail (hue <= 1.0);
  
  hsv_to_rgb (&h, &s, &v);

  color->red = h * 65535;
  color->green = s * 65535;
  color->blue = v * 65535;
}


static gint
color_cycle_timeout (gpointer data)
{
  Buffer *buffer = data;

  buffer_cycle_colors (buffer);

  return TRUE;
}

static void
buffer_set_colors (Buffer  *buffer,
                   gboolean enabled)
{
  GSList *tmp;
  gdouble hue = 0.0;

  if (enabled && buffer->color_cycle_timeout == 0)
    buffer->color_cycle_timeout = g_timeout_add (200, color_cycle_timeout, buffer);
  else if (!enabled && buffer->color_cycle_timeout != 0)
    {
      g_source_remove (buffer->color_cycle_timeout);
      buffer->color_cycle_timeout = 0;
    }
    
  tmp = buffer->color_tags;
  while (tmp != NULL)
    {
      if (enabled)
        {
          GdkColor color;
          
          hue_to_color (hue, &color);

          g_object_set (tmp->data,
                        "foreground_gdk", &color,
                        NULL);
        }
      else
        g_object_set (tmp->data,
                      "foreground_set", FALSE,
                      NULL);

      hue += 1.0 / N_COLORS;
      
      tmp = g_slist_next (tmp);
    }
}

static void
buffer_cycle_colors (Buffer *buffer)
{
  GSList *tmp;
  gdouble hue = buffer->start_hue;
  
  tmp = buffer->color_tags;
  while (tmp != NULL)
    {
      GdkColor color;
      
      hue_to_color (hue, &color);
      
      g_object_set (tmp->data,
                    "foreground_gdk", &color,
                    NULL);

      hue += 1.0 / N_COLORS;
      if (hue > 1.0)
        hue = 0.0;
      
      tmp = g_slist_next (tmp);
    }

  buffer->start_hue += 1.0 / N_COLORS;
  if (buffer->start_hue > 1.0)
    buffer->start_hue = 0.0;
}

static void
close_view (View *view)
{
  views = g_slist_remove (views, view);
  buffer_unref (view->buffer);
  gtk_widget_destroy (view->window);
  g_object_unref (view->item_factory);
  g_free (view);
}

static void
check_close_view (View *view)
{
  if (view->buffer->refcount > 1 ||
      check_buffer_saved (view->buffer))
    close_view (view);
}

static void
view_set_title (View *view)
{
  char *pretty_name = buffer_pretty_name (view->buffer);
  char *title = g_strconcat ("nsp edit - ", pretty_name, NULL);

  gtk_window_set_title (GTK_WINDOW (view->window), title);

  g_free (pretty_name);
  g_free (title);
}

static void
cursor_set_callback (GtkTextBuffer     *buffer,
                     const GtkTextIter *location,
                     GtkTextMark       *mark,
                     gpointer           user_data)
{
  GtkTextView *text_view;

  /* Redraw tab windows if the cursor moves
   * on the mapped widget (windows may not exist before realization...
   */
  
  text_view = GTK_TEXT_VIEW (user_data);
  
  if (GTK_WIDGET_MAPPED (text_view) &&
      mark == gtk_text_buffer_get_insert (buffer))
    {
      GdkWindow *tab_window;

      tab_window = gtk_text_view_get_window (text_view,
                                             GTK_TEXT_WINDOW_TOP);
      if ( tab_window != NULL) 
	gdk_window_invalidate_rect (tab_window, NULL, FALSE);
      
      tab_window = gtk_text_view_get_window (text_view,
                                             GTK_TEXT_WINDOW_BOTTOM);
      if ( tab_window != NULL) 
	gdk_window_invalidate_rect (tab_window, NULL, FALSE);
    }
}


static void
get_lines (GtkTextView  *text_view,
           gint          first_y,
           gint          last_y,
           GArray       *buffer_coords,
           GArray       *numbers,
           gint         *countp)
{
  GtkTextIter iter;
  gint count;
  gint size;  

  g_array_set_size (buffer_coords, 0);
  g_array_set_size (numbers, 0);
  
  /* Get iter at first y */
  gtk_text_view_get_line_at_y (text_view, &iter, first_y, NULL);

  /* For each iter, get its location and add it to the arrays.
   * Stop when we pass last_y
   */
  count = 0;
  size = 0;

  while (!gtk_text_iter_is_end (&iter))
    {
      gint y, height;
      gint line_num;
      
      gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);

      g_array_append_val (buffer_coords, y);
      line_num = gtk_text_iter_get_line (&iter);
      g_array_append_val (numbers, line_num);
      
      ++count;

      if ((y + height) >= last_y)
        break;
      
      gtk_text_iter_forward_line (&iter);
    }

  *countp = count;
}

static gint
line_numbers_expose (GtkWidget      *widget,
                     GdkEventExpose *event,
                     gpointer        user_data)
{
  gint count;
  GArray *numbers;
  GArray *pixels;
  gint first_y;
  gint last_y;
  gint i;
  GdkWindow *left_win;
  GdkWindow *right_win;
  PangoLayout *layout;
  GtkTextView *text_view;
  GtkTextWindowType type;
  GdkDrawable *target;
  
  text_view = GTK_TEXT_VIEW (widget);
  
  /* See if this expose is on the line numbers window */
  left_win = gtk_text_view_get_window (text_view,
                                       GTK_TEXT_WINDOW_LEFT);

  right_win = gtk_text_view_get_window (text_view,
                                        GTK_TEXT_WINDOW_RIGHT);

  if (event->window == left_win)
    {
      type = GTK_TEXT_WINDOW_LEFT;
      target = left_win;
    }
  else if (event->window == right_win)
    {
      type = GTK_TEXT_WINDOW_RIGHT;
      target = right_win;
    }
  else
    return FALSE;
  
  first_y = event->area.y;
  last_y = first_y + event->area.height;

  gtk_text_view_window_to_buffer_coords (text_view,
                                         type,
                                         0,
                                         first_y,
                                         NULL,
                                         &first_y);

  gtk_text_view_window_to_buffer_coords (text_view,
                                         type,
                                         0,
                                         last_y,
                                         NULL,
                                         &last_y);

  numbers = g_array_new (FALSE, FALSE, sizeof (gint));
  pixels = g_array_new (FALSE, FALSE, sizeof (gint));
  
  get_lines (text_view,
             first_y,
             last_y,
             pixels,
             numbers,
             &count);
  
  /* Draw fully internationalized numbers! */
  
  layout = gtk_widget_create_pango_layout (widget, "");
  
  i = 0;
  while (i < count)
    {
      gint pos;
      gchar *str;
      
      gtk_text_view_buffer_to_window_coords (text_view,
                                             type,
                                             0,
                                             g_array_index (pixels, gint, i),
                                             NULL,
                                             &pos);

      str = g_strdup_printf ("%d", g_array_index (numbers, gint, i));

      pango_layout_set_text (layout, str, -1);

      gtk_paint_layout (widget->style,
                        target,
                        GTK_WIDGET_STATE (widget),
                        FALSE,
                        NULL,
                        widget,
                        NULL,
                        2, pos + 2,
                        layout);

      g_free (str);
      
      ++i;
    }

  g_array_free (pixels, TRUE);
  g_array_free (numbers, TRUE);
  
  g_object_unref (layout);

  /* don't stop emission, need to draw children */
  return FALSE;
}





static View *
create_view (Buffer *buffer)
{
  View *view;
  
  GtkWidget *sw;
  GtkWidget *vbox;
  
  view = g_new0 (View, 1);
  views = g_slist_prepend (views, view);

  view->buffer = buffer;
  buffer_ref (buffer);
  
  view->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  g_object_set_data (G_OBJECT (view->window), "view", view);
  
  g_signal_connect (view->window, "delete_event",
		    G_CALLBACK (delete_event_cb), NULL);

  view->accel_group = gtk_accel_group_new ();
  view->item_factory = gtk_item_factory_new (GTK_TYPE_MENU_BAR, "<main>", view->accel_group);
  g_object_set_data (G_OBJECT (view->item_factory), "view", view);
  
  gtk_item_factory_create_items (view->item_factory, G_N_ELEMENTS (menu_items), menu_items, view);

  gtk_window_add_accel_group (GTK_WINDOW (view->window), view->accel_group);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (view->window), vbox);

  gtk_box_pack_start (GTK_BOX (vbox),
		      gtk_item_factory_get_widget (view->item_factory, "<main>"),
		      FALSE, FALSE, 0);
  
  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
                                 GTK_POLICY_AUTOMATIC,
                                 GTK_POLICY_AUTOMATIC);

  view->text_view = gtk_text_view_new_with_buffer (buffer->buffer);
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (view->text_view),
                               GTK_WRAP_WORD);

  /* Make sure border width works, no real reason to do this other than testing */
  gtk_container_set_border_width (GTK_CONTAINER (view->text_view),
                                  5);
  
  /* Draw tab stops in the top and bottom windows. */

  g_signal_connect (view->buffer->buffer,
		    "mark_set",
		    G_CALLBACK (cursor_set_callback),
		    view->text_view);

  /* Draw line numbers in the side windows; we should really be
   * more scientific about what width we set them to.
   */
  
  gtk_text_view_set_border_window_size (GTK_TEXT_VIEW (view->text_view),
                                        GTK_TEXT_WINDOW_LEFT,
                                        30);
  
  g_signal_connect (view->text_view,
                    "expose_event",
                    G_CALLBACK (line_numbers_expose),
                    NULL);

  
  gtk_box_pack_start (GTK_BOX (vbox), sw, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (sw), view->text_view);

  gtk_window_set_default_size (GTK_WINDOW (view->window), 500, 500);

  gtk_widget_grab_focus (view->text_view);

  view_set_title (view);

  gtk_widget_show_all (view->window);
  return view;
}


#ifdef ALONE 
int
main (int argc, char** argv)
{
  Buffer *buffer;
  View *view;
  int i;

  gtk_init (&argc, &argv);
  
  buffer = create_buffer ();
  view = create_view (buffer);
  buffer_unref (buffer);
  
  push_active_window (GTK_WINDOW (view->window));
  pop_active_window ();
  gtk_main ();
  return 0;
}

#else 

int nsp_edit(void)
{
  Buffer *buffer;
  View *view;
  /* gtk_init (&argc, &argv); */
  buffer = create_buffer ();
  view = create_view (buffer);
  buffer_unref (buffer);
  push_active_window (GTK_WINDOW (view->window));
  pop_active_window ();
  /* gtk_main (); */
  return 0;
}
#endif


