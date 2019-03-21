/*
 * Copyright (C) 2001,2002 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * vteapp: from vte-0.40.0 slightly adapted for nsp
 * Copyright (C) 2016 Chancelier Jean-Philippe (ENPC/Cermics).
 *
 */

#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <gtk/gtkx.h>
#include <glib-object.h>

#undef VTE_DISABLE_DEPRECATED
#include <vte/vte.h>

#include <glib/gi18n.h>

#define DINGUS1 "(((gopher|news|telnet|nntp|file|http|ftp|https)://)|(www|ftp)[-A-Za-z0-9]*\\.)[-A-Za-z0-9\\.]+(:[0-9]*)?"
#define DINGUS2 DINGUS1 "/[-A-Za-z0-9_\\$\\.\\+\\!\\*\\(\\),;:@&=\\?/~\\#\\%]*[^]'\\.}>\\) ,\\\"]"

static const char *builtin_dingus[] = {
  DINGUS1,
  DINGUS2,
  NULL
};

#define NSP
static int
button_pressed(GtkWidget *widget, GdkEventButton *event, gpointer data);
#ifdef NSP
gdouble def_scale;
#include "tumbi48.xpm"
#endif

#if VTE_CHECK_VERSION(0,38,0)
/* exists in VERSION(0,38,0) */
#else
/* to be defined for VERSION(0,32,0) */
static void
vte_terminal_set_geometry_hints_for_window(VteTerminal *terminal,
                                           GtkWindow *window);
static gdouble
vte_terminal_get_font_scale(VteTerminal *terminal);
static void
vte_terminal_set_font_scale(VteTerminal *terminal,
                            gdouble scale);
#endif

static void
window_title_changed(GtkWidget *widget, gpointer win)
{
  GtkWindow *window;

  g_assert(VTE_TERMINAL(widget));
  g_assert(GTK_IS_WINDOW(win));
  g_assert(vte_terminal_get_window_title(VTE_TERMINAL(widget))  != NULL);
  window = GTK_WINDOW(win);
  gtk_window_set_title(window, vte_terminal_get_window_title(VTE_TERMINAL(widget)));
}

static void
icon_title_changed(GtkWidget *widget, gpointer win)
{
  /* GtkWindow *window = GTK_WINDOW(win); */
  g_assert(VTE_TERMINAL(widget));
  g_assert(GTK_IS_WINDOW(win));
  /* g_assert(vte_terminal_get_icon_title(VTE_TERMINAL(widget)) != NULL); */
  
  g_message("Icon title changed to \"%s\".\n",
	    vte_terminal_get_icon_title(VTE_TERMINAL(widget)));
}

static void
char_size_changed(GtkWidget *widget, guint width, guint height, gpointer data)
{
  VteTerminal *terminal = VTE_TERMINAL(widget);
  GtkWindow *window = GTK_WINDOW(data);

  if (!gtk_widget_get_realized (widget))
    return;

  vte_terminal_set_geometry_hints_for_window(terminal, window);
}

static void
char_size_realized(GtkWidget *widget, gpointer data)
{
  VteTerminal *terminal = VTE_TERMINAL(widget);
  GtkWindow *window = GTK_WINDOW(data);

  if (!gtk_widget_get_realized (widget))
    return;

  vte_terminal_set_geometry_hints_for_window(terminal, window);
}


static void
destroy_and_quit(VteTerminal *terminal, GtkWidget *window, int tag )
{
  const char *output_file = g_object_get_data (G_OBJECT (terminal), "output_file");
  // printf("Inside destroy_and_quit\n");
  if (output_file) {
    GFile *file;
    GOutputStream *stream;
    GError *error = NULL;

    file = g_file_new_for_commandline_arg (output_file);
    stream = G_OUTPUT_STREAM (g_file_replace (file, NULL, FALSE, G_FILE_CREATE_NONE, NULL, &error));

    if (stream) {
#if VTE_CHECK_VERSION(0,38,0)
      vte_terminal_write_contents_sync (terminal, stream,
					VTE_WRITE_DEFAULT,
					NULL, &error);
#else
      vte_terminal_write_contents (terminal, stream,
				   VTE_TERMINAL_WRITE_DEFAULT,
				   NULL, &error);
#endif
      g_object_unref (stream);
    }

    if (error) {
      g_printerr ("%s\n", error->message);
      g_error_free (error);
    }

    g_object_unref (file);
  }
  if (tag == TRUE && window != NULL && GTK_IS_WIDGET(window)) gtk_widget_destroy (window);
  gtk_main_quit ();
}

/* called when closing the terminal 
 */

static void
delete_event(GtkWidget *window, GdkEvent *event, gpointer terminal)
{
  const int tag =GPOINTER_TO_INT( g_object_get_data (G_OBJECT (terminal), "info"));
  /* printf("Inside delete_event\n"); */
  g_object_set_data (G_OBJECT (terminal), "info",GINT_TO_POINTER (FALSE));
  destroy_and_quit(VTE_TERMINAL (terminal), window, tag);
  /* printf("quit delete_event\n"); */
}

/* called when calling quit or exit in nsp 
 * take care : no int status as second argument 
 */

static void
child_exited(GtkWidget *terminal, gpointer window)
{
  const int tag =GPOINTER_TO_INT( g_object_get_data (G_OBJECT (terminal), "info"));
  /* printf("Inside child_exited\n"); */
  g_object_set_data (G_OBJECT (terminal), "info",GINT_TO_POINTER (FALSE));
  destroy_and_quit(VTE_TERMINAL (terminal), window, tag);
  /* printf("quit child_exited\n"); */
}

#ifndef NSP
static int
button_pressed(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  VteTerminal *terminal;
  char *match;
  int tag;
  GtkBorder padding;
  int char_width, char_height;

  switch (event->button) {
  case 3:
    terminal = VTE_TERMINAL(widget);

    gtk_style_context_get_padding(gtk_widget_get_style_context(widget),
				  gtk_widget_get_state_flags(widget),
				  &padding);
    char_width = vte_terminal_get_char_width (terminal);
    char_height = vte_terminal_get_char_height (terminal);
    match = vte_terminal_match_check(terminal,
				     (event->x - padding.left) / char_width,
				     (event->y - padding.top) / char_height,
				     &tag);
    if (match != NULL) {
      g_print("Matched `%s' (%d).\n", match, tag);
      g_free(match);
      if (GPOINTER_TO_INT(data) != 0) {
	vte_terminal_match_remove(terminal, tag);
      }
    }
    break;
  case 1:
  case 2:
  default:
    break;
  }
  return FALSE;
}
#endif

static void
iconify_window(GtkWidget *widget, gpointer data)
{
  gtk_window_iconify(data);
}

static void
deiconify_window(GtkWidget *widget, gpointer data)
{
  gtk_window_deiconify(data);
}

static void
raise_window(GtkWidget *widget, gpointer data)
{
  GdkWindow *window;

  if (GTK_IS_WIDGET(data)) {
    window = gtk_widget_get_window(GTK_WIDGET(data));
    if (window) {
      gdk_window_raise(window);
    }
  }
}

static void
lower_window(GtkWidget *widget, gpointer data)
{
  GdkWindow *window;

  if (GTK_IS_WIDGET(data)) {
    window = gtk_widget_get_window(GTK_WIDGET(data));
    if (window) {
      gdk_window_lower(window);
    }
  }
}

static void
maximize_window(GtkWidget *widget, gpointer data)
{
  GdkWindow *window;

  if (GTK_IS_WIDGET(data)) {
    window = gtk_widget_get_window(GTK_WIDGET(data));
    if (window) {
      gdk_window_maximize(window);
    }
  }
}

static void
restore_window(GtkWidget *widget, gpointer data)
{
  GdkWindow *window;

  if (GTK_IS_WIDGET(data)) {
    window = gtk_widget_get_window(GTK_WIDGET(data));
    if (window) {
      gdk_window_unmaximize(window);
    }
  }
}

static void
refresh_window(GtkWidget *widget, gpointer data)
{
  GdkWindow *window;
  GtkAllocation allocation;
  GdkRectangle rect;

  if (GTK_IS_WIDGET(data)) {
    window = gtk_widget_get_window(widget);
    if (window) {
      gtk_widget_get_allocation(widget, &allocation);
      rect.x = rect.y = 0;
      rect.width = allocation.width;
      rect.height = allocation.height;
      gdk_window_invalidate_rect(window, &rect, TRUE);
    }
  }
}

static void
resize_window(GtkWidget *widget, guint width, guint height, gpointer data)
{
  VteTerminal *terminal;

  if ((GTK_IS_WINDOW(data)) && (width >= 2) && (height >= 2)) {
    gint owidth, oheight, char_width, char_height, column_count, row_count;
    GtkBorder padding;

    terminal = VTE_TERMINAL(widget);

    gtk_window_get_size(GTK_WINDOW(data), &owidth, &oheight);

    /* Take into account border overhead. */
    char_width = vte_terminal_get_char_width (terminal);
    char_height = vte_terminal_get_char_height (terminal);
    column_count = vte_terminal_get_column_count (terminal);
    row_count = vte_terminal_get_row_count (terminal);
    gtk_style_context_get_padding(gtk_widget_get_style_context(widget),
				  gtk_widget_get_state_flags(widget),
				  &padding);

    owidth -= char_width * column_count + padding.left + padding.right;
    oheight -= char_height * row_count + padding.top + padding.bottom;
    gtk_window_resize(GTK_WINDOW(data),
		      width + owidth, height + oheight);
  }
 }

static void
move_window(GtkWidget *widget, guint x, guint y, gpointer data)
{
  GdkWindow *window;

  if (GTK_IS_WIDGET(data)) {
    window = gtk_widget_get_window(GTK_WIDGET(data));
    if (window) {
      gdk_window_move(window, x, y);
    }
  }
}

static void
adjust_font_size(GtkWidget *widget, gpointer data, gdouble factor)
{
  VteTerminal *terminal;
  gdouble scale;
  glong char_width, char_height;
  gint columns, rows, owidth, oheight;

  /* Read the screen dimensions in cells. */
  terminal = VTE_TERMINAL(widget);
  columns = vte_terminal_get_column_count(terminal);
  rows = vte_terminal_get_row_count(terminal);

  /* Take into account padding and border overhead. */
  gtk_window_get_size(GTK_WINDOW(data), &owidth, &oheight);
  char_width = vte_terminal_get_char_width (terminal);
  char_height = vte_terminal_get_char_height (terminal);
  owidth -= char_width * columns;
  oheight -= char_height * rows;

  scale = vte_terminal_get_font_scale(terminal);
  vte_terminal_set_font_scale(terminal, scale * factor);

  /* This above call will have changed the char size! */
  char_width = vte_terminal_get_char_width (terminal);
  char_height = vte_terminal_get_char_height (terminal);

  gtk_window_resize(GTK_WINDOW(data),
		    columns * char_width + owidth,
		    rows * char_height + oheight);
}

static void
increase_font_size(GtkWidget *widget, gpointer data)
{
  adjust_font_size(widget, data, 1.2);
}

static void
decrease_font_size(GtkWidget *widget, gpointer data)
{
  adjust_font_size(widget, data, 1. / 1.2);
}

static gboolean
read_and_feed(GIOChannel *source, GIOCondition condition, gpointer data)
{
  char buf[2048];
  gsize size;
  GIOStatus status;
  g_assert(VTE_IS_TERMINAL(data));
  status = g_io_channel_read_chars(source, buf, sizeof(buf),
				   &size, NULL);
  if ((status == G_IO_STATUS_NORMAL) && (size > 0)) {
    vte_terminal_feed(VTE_TERMINAL(data), buf, size);
    return TRUE;
  }
  return FALSE;
}

static void
disconnect_watch(gpointer data)
{
  g_source_remove(GPOINTER_TO_INT(data));
}

static void
clipboard_get(GtkClipboard *clipboard, GtkSelectionData *selection_data,
	      guint info, gpointer owner)
{
  /* No-op. */
  return;
}

static void
take_xconsole_ownership(GtkWidget *widget, gpointer data)
{
  char *name, hostname[255];
  GdkAtom atom;
  GtkClipboard *clipboard;
  GtkTargetList *target_list;
  GtkTargetEntry *targets;
  int n_targets;

  target_list = gtk_target_list_new(NULL, 0);
  gtk_target_list_add_text_targets(target_list, 0);
  targets = gtk_target_table_new_from_list (target_list, &n_targets);
  gtk_target_list_unref(target_list);

  memset(hostname, '\0', sizeof(hostname));
  gethostname(hostname, sizeof(hostname) - 1);

  name = g_strdup_printf("MIT_CONSOLE_%s", hostname);
  atom = gdk_atom_intern(name, FALSE);
  clipboard = gtk_clipboard_get_for_display(gtk_widget_get_display(widget),
					    atom);
  g_free(name);

  gtk_clipboard_set_with_owner(clipboard,
			       targets,
			       n_targets,
			       clipboard_get,
			       (GtkClipboardClearFunc)gtk_main_quit,
			       G_OBJECT(widget));
}

static void
add_weak_pointer(GObject *object, GtkWidget **target)
{
  g_object_add_weak_pointer(object, (gpointer*)target);
}

static void
terminal_notify_cb(GObject *object,
		   GParamSpec *pspec,
		   gpointer user_data)
{
  GValue value = { 0, };
  char *value_string;

  if (!pspec ||
      pspec->owner_type != VTE_TYPE_TERMINAL)
    return;


  g_value_init(&value, pspec->value_type);
  g_object_get_property(object, pspec->name, &value);
  value_string = g_strdup_value_contents(&value);
  g_print("NOTIFY property \"%s\" value '%s'\n", pspec->name, value_string);
  g_free(value_string);
  g_value_unset(&value);
}

/* Derived terminal class */

typedef struct _VteappTerminal      VteappTerminal;
typedef struct _VteappTerminalClass VteappTerminalClass;

struct _VteappTerminalClass {
  VteTerminalClass parent_class;
};
struct _VteappTerminal {
  VteTerminal parent_instance;
};

static GType vteapp_terminal_get_type(void);

G_DEFINE_TYPE(VteappTerminal, vteapp_terminal, VTE_TYPE_TERMINAL)

static void
vteapp_terminal_class_init(VteappTerminalClass *klass)
{
}

static void
vteapp_terminal_init(VteappTerminal *terminal)
{
}

static GtkWidget *
vteapp_terminal_new(void)
{
  return g_object_new(vteapp_terminal_get_type(), NULL);
}

/* Command line options */

static int
parse_enum(GType type,
	   const char *string)
{
  GEnumClass *enum_klass;
  const GEnumValue *enum_value;
  int value = 0;

  enum_klass = (GEnumClass*)g_type_class_ref(type);
  enum_value = g_enum_get_value_by_nick(enum_klass, string);
  if (enum_value)
    value = enum_value->value;
  else
    g_warning("Unknown enum '%s'\n", string);
  g_type_class_unref(enum_klass);

  return value;
}

static guint
parse_flags(GType type,
	    const char *string)
{
  GFlagsClass *flags_klass;
  guint value = 0;
  char **flags;
  guint i;

  flags = g_strsplit_set(string, ",|", -1);
  if (flags == NULL)
    return 0;

  flags_klass = (GFlagsClass*)g_type_class_ref(type);
  for (i = 0; flags[i] != NULL; ++i) {
    const GFlagsValue *flags_value;

    flags_value = g_flags_get_value_by_nick(flags_klass, flags[i]);
    if (flags_value)
      value |= flags_value->value;
    else
      g_warning("Unknown flag '%s'\n", flags[i]);
  }
  g_type_class_unref(flags_klass);

  return value;
}

static gboolean
parse_color (const gchar *value,
             GdkRGBA *color)
{
  if (!gdk_rgba_parse(color, value)) {
    g_printerr("Failed to parse value \"%s\" as color", value);
    return FALSE;
  }

  return TRUE;
}

static void
add_dingus (VteTerminal *terminal,
            char **dingus)
{
#ifdef USE_VTEREGEX
  const GdkCursorType cursors[] = { GDK_GUMBY, GDK_HAND1 };
  VteRegex *regex;
  GError *error;
  int id, i;

  for (i = 0; dingus[i]; ++i) {
    error = NULL;
    if (!(regex = vte_regex_new_for_match(dingus[i],-1, 0, &error))) {
      /* g_warning("Failed to compile regex '%s': %s\n",dingus[i], error->message); */
      g_error_free(error);
      continue;
    }

    id = vte_terminal_match_add_regex(terminal, regex, 0);
    vte_regex_unref (regex);
    vte_terminal_match_set_cursor_type(terminal, id,
				       cursors[i % G_N_ELEMENTS(cursors)]);
  }
#endif
}


/* change drag and drop behaviour because we want to
 * be able to drop anywhere in the terminal window
 * even in non editable zones. Because the evaluation
 * of pasted data will be then added at the end.
 */

static void
gtk_text_view_drag_end (GtkWidget        *widget,
                        GdkDragContext   *context,
			gpointer data)
{

}

static gboolean
gtk_text_view_drag_motion (GtkWidget        *widget,
                           GdkDragContext   *context,
                           gint              x,
                           gint              y,
                           guint             time,
			   gpointer data)
{
  /* TRUE return means don't propagate the drag motion to parent
   * widgets that may also be drop sites.
   */
  return FALSE;
}



static void
gtk_text_view_drag_data_received (GtkWidget        *widget,
                                  GdkDragContext   *context,
                                  gint              x,
                                  gint              y,
                                  GtkSelectionData *selection_data,
                                  guint             info,
                                  guint             time,
				  gpointer data)
{
  gboolean success = TRUE;
  GtkTextIter drop_point,start,end;
  g_signal_stop_emission_by_name (widget, "drag_data_received");
  /* deals with the drop */
  switch (info) {
  case GTK_TEXT_BUFFER_TARGET_INFO_BUFFER_CONTENTS:
    break;
  case GTK_TEXT_BUFFER_TARGET_INFO_RICH_TEXT:
    break;
  case GTK_TEXT_BUFFER_TARGET_INFO_TEXT: 
    {
      guchar *str;
      GdkAtom *targets;
      str = gtk_selection_data_get_text (selection_data);
      if (str)
	{
	  int n = strlen((char *) str);
	 /* remove trailing \r\n */
	 if ( str[n-2] == '\r' ) str[n-2]='\0';
	 if ( str[n-1] == '\n' ) str[n-2]='\0';
	 fprintf(stderr,"This is str %s\n",str);
	 g_free (str);
       }
     break;
   }
  }
  gtk_drag_finish (context, success,
		   success && gdk_drag_context_get_actions (context) == GDK_ACTION_MOVE,
		   time);
}
static void _dummy(const gchar *log_domain,
		   GLogLevelFlags log_level,
		   const gchar *message,
		   gpointer user_data )
{
  /* Dummy does nothing */ 
  return ;      
}

int
main(int argc, char **argv)
{
  GtkStyleContext *style_context;
  GdkPixbuf *pixbuf;
  GtkWidget *socket_button;
  GdkScreen *screen;
  GdkVisual *visual;
  GtkWidget *window, *widget,*hbox = NULL,*vbox=NULL, *scrollbar, *scrolled_window = NULL;
  VteTerminal *terminal;
  char *env_add[] = {
#ifdef VTE_DEBUG
    (char *) "FOO=BAR", (char *) "BOO=BIZ",
#endif
    NULL,
    NULL};
  char *transparent = NULL;
  char *encoding = NULL;
  char *cjk_ambiguous_width = NULL;
  gboolean audible = FALSE,
    debug = FALSE, no_builtin_dingus = FALSE, dbuffer = TRUE,
    console = FALSE, keep = FALSE,
    icon_title = FALSE, shell = TRUE,
    reverse = -3,  use_geometry_hints = TRUE,
    use_scrolled_window = FALSE,
    show_object_notifications = FALSE, rewrap = TRUE, dark_theme = FALSE;
  char *geometry = NULL;
  gint lines = -1;
  const char *message = "Launching interactive shell...\r\n";
  const char *font = NULL;
  const char *command = NULL;
  const char *working_directory = NULL;
  const char *output_file = NULL;
  char *pty_flags_string = NULL;
  char *cursor_blink_mode_string = NULL;
  char *cursor_shape_string = NULL;
  char *scrollbar_policy_string = NULL;
  char *border_width_string = NULL;
  char *cursor_color_string = NULL;
  char *highlight_foreground_color_string = NULL;
  char *highlight_background_color_string = NULL;
  char **dingus = NULL;
  GdkRGBA fore, back;
  GdkRGBA theme_fg, theme_bg;
    
  const GOptionEntry options[]={
    {
      "console", 'C', 0,
      G_OPTION_ARG_NONE, &console,
      "Watch /dev/console", NULL
    },
    {
      "no-builtin-dingus", 0, G_OPTION_FLAG_REVERSE,
      G_OPTION_ARG_NONE, &no_builtin_dingus,
      "Highlight URLs inside the terminal", NULL
    },
    {
      "dingu", 'D', 0,
      G_OPTION_ARG_STRING_ARRAY, &dingus,
      "Add regex highlight", NULL
    },
    {
      "no-rewrap", 'R', G_OPTION_FLAG_REVERSE,
      G_OPTION_ARG_NONE, &rewrap,
      "Disable rewrapping on resize", NULL
    },
    {
      "shell", 'S', G_OPTION_FLAG_REVERSE,
      G_OPTION_ARG_NONE, &shell,
      "Disable spawning a shell inside the terminal", NULL
    },
    {
      "transparent", 'T', 0,
      G_OPTION_ARG_STRING, &transparent,
      "Enable the use of a transparent background", "ALPHA"
    },
    {
      "double-buffer", '2', G_OPTION_FLAG_REVERSE,
      G_OPTION_ARG_NONE, &dbuffer,
      "Disable double-buffering", NULL
    },
    {
      "audible-bell", 'a', 0,
      G_OPTION_ARG_NONE, &audible,
      "Use audible terminal bell",
      NULL
    },
    {
      "command", 'c', 0,
      G_OPTION_ARG_STRING, &command,
      "Execute a command in the terminal", NULL
    },
    {
      "debug", 'd', 0,
      G_OPTION_ARG_NONE, &debug,
      "Enable various debugging checks", NULL
    },
    {
      "command", 'e', 0,
      G_OPTION_ARG_STRING, &command,
      "Execute a command in the terminal", NULL
    },
    {
      "font", 'f', 0,
      G_OPTION_ARG_STRING, &font,
      "Specify a font to use", NULL
    },
    {
      "geometry", 'g', 0,
      G_OPTION_ARG_STRING, &geometry,
      "Set the size (in characters) and position", "GEOMETRY"
    },
    {
      "highlight-foreground-color", 0, 0,
      G_OPTION_ARG_STRING, &highlight_foreground_color_string,
      "Enable distinct highlight foreground color for selection", NULL
    },
    {
      "highlight-background-color", 0, 0,
      G_OPTION_ARG_STRING, &highlight_background_color_string,
      "Enable distinct highlight background color for selection", NULL
    },
    {
      "icon-title", 'i', 0,
      G_OPTION_ARG_NONE, &icon_title,
      "Enable the setting of the icon title", NULL
    },
    {
      "keep", 'k', 0,
      G_OPTION_ARG_NONE, &keep,
      "Live on after the window closes", NULL
    },
    {
      "scrollback-lines", 'n', 0,
      G_OPTION_ARG_INT, &lines,
      "Specify the number of scrollback-lines", NULL
    },
    {
      "cursor-blink", 0, 0,
      G_OPTION_ARG_STRING, &cursor_blink_mode_string,
      "Cursor blink mode (system|on|off)", "MODE"
    },
    {
      "cursor-color", 0, 0,
      G_OPTION_ARG_STRING, &cursor_color_string,
      "Enable a colored cursor", NULL
    },
    {
      "cursor-shape", 0, 0,
      G_OPTION_ARG_STRING, &cursor_shape_string,
      "Set cursor shape (block|underline|ibeam)", NULL
    },
    {
      "encoding", 0, 0,
      G_OPTION_ARG_STRING, &encoding,
      "Specify the terminal encoding to use", NULL
    },
    {
      "cjk-width", 0, 0,
      G_OPTION_ARG_STRING, &cjk_ambiguous_width,
      "Specify the cjk ambiguous width to use for UTF-8 encoding", "NARROW|WIDE"
    },
    {
      "working-directory", 'w', 0,
      G_OPTION_ARG_FILENAME, &working_directory,
      "Specify the initial working directory of the terminal",
      NULL
    },
    {
      "reverse", 0, 0,
      G_OPTION_ARG_NONE, &reverse,
      "Reverse foreground/background colors", NULL
    },
    {
      "no-geometry-hints", 'G', G_OPTION_FLAG_REVERSE,
      G_OPTION_ARG_NONE, &use_geometry_hints,
      "Allow the terminal to be resized to any dimension, not constrained to fit to an integer multiple of characters",
      NULL
    },
    {
      "scrolled-window", 'W', 0,
      G_OPTION_ARG_NONE, &use_scrolled_window,
      "Use a GtkScrolledWindow as terminal container",
      NULL
    },
    {
      "scrollbar-policy", 'P', 0,
      G_OPTION_ARG_STRING, &scrollbar_policy_string,
      "Set the policy for the vertical scroolbar in the scrolled window (always|auto|never; default:always)",
      NULL
    },
    {
      "object-notifications", 'N', 0,
      G_OPTION_ARG_NONE, &show_object_notifications,
      "Print VteTerminal object notifications",
      NULL
    },
    {
      "output-file", 0, 0,
      G_OPTION_ARG_STRING, &output_file,
      "Save terminal contents to file at exit", NULL
    },
    {
      "pty-flags", 0, 0,
      G_OPTION_ARG_STRING, &pty_flags_string,
      "PTY flags set from default|no-utmp|no-wtmp|no-lastlog|no-helper|no-fallback", NULL
    },
    {
      "border-width", 0, 0,
      G_OPTION_ARG_STRING, &border_width_string,
      "Border with", "WIDTH"
    },
    { NULL }
  };
  GOptionContext *context;
  GError *error = NULL;
#if VTE_CHECK_VERSION(0,38,0)
  VteCursorBlinkMode cursor_blink_mode = VTE_CURSOR_BLINK_SYSTEM;
  VteCursorShape cursor_shape = VTE_CURSOR_SHAPE_BLOCK;
#else
  VteTerminalCursorBlinkMode cursor_blink_mode = VTE_CURSOR_BLINK_SYSTEM;
  VteTerminalCursorShape cursor_shape = VTE_CURSOR_SHAPE_BLOCK;
#endif
  GtkPolicyType scrollbar_policy = GTK_POLICY_ALWAYS;
  VtePtyFlags pty_flags = VTE_PTY_DEFAULT;
  /*
    _vte_debug_init();
  */

  /*   DEPRECATED:
       Have to do this early. 
  if (getenv("VTE_PROFILE_MEMORY")) {
    if (atol(getenv("VTE_PROFILE_MEMORY")) != 0) {
      g_mem_set_vtable(glib_mem_profiler_table);
    }
  }
  */
  
  if (g_getenv("VTE_CJK_WIDTH")) {
    g_printerr("VTE_CJK_WIDTH is not supported anymore, use --cjk-width instead\n");
  }

  context = g_option_context_new (" - test VTE terminal emulation");
  g_option_context_add_main_entries (context, options, NULL);
  g_option_context_add_group (context, gtk_get_option_group (TRUE));
  g_option_context_parse (context, &argc, &argv, &error);
  g_option_context_free (context);
  if (error != NULL) {
    g_printerr ("Failed to parse command line arguments: %s\n",
		error->message);
    g_error_free (error);
    return 1;
  }
  if (cursor_blink_mode_string) {
#if VTE_CHECK_VERSION(0,38,0)
    cursor_blink_mode = parse_enum(VTE_TYPE_CURSOR_BLINK_MODE, cursor_blink_mode_string);
#else
    cursor_blink_mode = parse_enum(VTE_TYPE_TERMINAL_CURSOR_BLINK_MODE, cursor_blink_mode_string);
#endif
    g_free(cursor_blink_mode_string);
  }

  if (cursor_shape_string) {
#if VTE_CHECK_VERSION(0,38,0)
    cursor_shape = parse_enum(VTE_TYPE_CURSOR_SHAPE, cursor_shape_string);
#else
    cursor_shape = parse_enum(VTE_TYPE_TERMINAL_CURSOR_SHAPE, cursor_shape_string);
#endif    
    g_free(cursor_shape_string);
  }

  if (scrollbar_policy_string) {
    scrollbar_policy = parse_enum(GTK_TYPE_POLICY_TYPE, scrollbar_policy_string);
    g_free(scrollbar_policy_string);
  }
  if (pty_flags_string) {
    pty_flags |= parse_flags(VTE_TYPE_PTY_FLAGS, pty_flags_string);
    g_free(pty_flags_string);
  }


  gdk_window_set_debug_updates(debug);

  /* Create a window to hold the scrolling shell, and hook its
   * delete event to the quit function.. */
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

#ifdef NSP
  /* we can remove this since --class=Nsp --name=nsp will do the same */
  gtk_window_set_title (GTK_WINDOW (window), "Nsp");
  pixbuf = gdk_pixbuf_new_from_xpm_data (tumbi48_xpm);
  gtk_window_set_icon(GTK_WINDOW(window),pixbuf);
  gtk_window_set_wmclass (GTK_WINDOW (window), "nsp", "Nsp");
#endif
  /* DEPRECATED:
  gtk_container_set_resize_mode(GTK_CONTAINER(window),
				GTK_RESIZE_IMMEDIATE);
  */
  if (border_width_string) {
    guint w;

    w = g_ascii_strtoull (border_width_string, NULL, 10);
    gtk_container_set_border_width(GTK_CONTAINER(window), w);
    g_free (border_width_string);
  }

  /* Set ARGB visual */
  screen = gtk_widget_get_screen (window);
  visual = gdk_screen_get_rgba_visual(screen);
  if (visual)
    gtk_widget_set_visual(GTK_WIDGET(window), visual);

  /* a global vbox*/
  vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  gtk_container_add(GTK_CONTAINER(window), vbox);
  /* Create a gtk_socket to store the menu pluged by nsp */
  {
    char buf[56];
    socket_button = gtk_socket_new();
    gtk_widget_set_size_request(socket_button,1,10);
    // gtk_box_pack_start(GTK_BOX(hbox), socket_button,FALSE,TRUE,0);
    gtk_container_add(GTK_CONTAINER(vbox), socket_button);
    // gtk_box_pack_start(GTK_BOX(vbox),socket_button,FALSE,TRUE,0);
    sprintf(buf,"SCIWIN=%#lx",(gulong) gtk_socket_get_id(GTK_SOCKET(socket_button)));
    env_add[0]=buf;
  }

  if (use_scrolled_window)
    {
      scrolled_window = gtk_scrolled_window_new (NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				     GTK_POLICY_NEVER, scrollbar_policy);
      /* gtk_container_add(GTK_CONTAINER(window), scrolled_window); */
      gtk_box_pack_start(GTK_BOX(vbox), scrolled_window,TRUE,TRUE,0);
    }
  else
    {
      /* Create a box to hold everything. */
      hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
      /* gtk_container_add(GTK_CONTAINER(window), hbox); */
      gtk_box_pack_start(GTK_BOX(vbox), hbox,TRUE,TRUE,0);
    }
  
  /* Create the terminal widget and add it to the scrolling shell. */
  widget = vteapp_terminal_new();
  terminal = VTE_TERMINAL (widget);
  
  if (show_object_notifications)
    g_signal_connect(terminal, "notify", G_CALLBACK(terminal_notify_cb), NULL);

  if (use_scrolled_window)
    {
      gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(terminal));
    }
  else
    {
      gtk_box_pack_start(GTK_BOX(hbox), widget, TRUE, TRUE, 0);
      /* Create the scrollbar for the widget. */
      scrollbar = gtk_scrollbar_new(GTK_ORIENTATION_VERTICAL,
				    gtk_scrollable_get_vadjustment(GTK_SCROLLABLE(terminal)));
      gtk_box_pack_start(GTK_BOX(hbox), scrollbar, FALSE, FALSE, 0);
    }
  
  /* Connect to the "char_size_changed" signal to set geometry hints
   * whenever the font used by the terminal is changed. 
   */
  if (use_geometry_hints) {
    char_size_changed(widget, 0, 0, window);
    g_signal_connect(widget, "char-size-changed",
		     G_CALLBACK(char_size_changed), window);
    g_signal_connect(widget, "realize",
		     G_CALLBACK(char_size_realized), window);
  }
  
  /* Connect to the "window_title_changed" signal to set the main
   * window's title. 
   */
  g_signal_connect(widget, "window-title-changed",
		   G_CALLBACK(window_title_changed), window);
  if (icon_title) {
    g_signal_connect(widget, "icon-title-changed",
		     G_CALLBACK(icon_title_changed), window);
  }

  /* Connect to the "button-press" event. */
#ifdef NSP
  g_signal_connect(widget, "button-press-event",
		   G_CALLBACK(button_pressed), window);
#else
  g_signal_connect(widget, "button-press-event",
		   G_CALLBACK(button_pressed), widget);
#endif
  /* Connect to application request signals. */
  g_signal_connect(widget, "iconify-window",
		   G_CALLBACK(iconify_window), window);
  g_signal_connect(widget, "deiconify-window",
		   G_CALLBACK(deiconify_window), window);
  g_signal_connect(widget, "raise-window",
		   G_CALLBACK(raise_window), window);
  g_signal_connect(widget, "lower-window",
		   G_CALLBACK(lower_window), window);
  g_signal_connect(widget, "maximize-window",
		   G_CALLBACK(maximize_window), window);
  g_signal_connect(widget, "restore-window",
		   G_CALLBACK(restore_window), window);
  g_signal_connect(widget, "refresh-window",
		   G_CALLBACK(refresh_window), window);
  g_signal_connect(widget, "resize-window",
		   G_CALLBACK(resize_window), window);
  g_signal_connect(widget, "move-window",
		   G_CALLBACK(move_window), window);

  /* Connect to font tweakage. */
  g_signal_connect(widget, "increase-font-size",
		   G_CALLBACK(increase_font_size), window);
  g_signal_connect(widget, "decrease-font-size",
		   G_CALLBACK(decrease_font_size), window);

  g_signal_connect(widget,"drag_data_received",
		   G_CALLBACK (gtk_text_view_drag_data_received),NULL);

  g_signal_connect(widget,"drag_end",G_CALLBACK (gtk_text_view_drag_end),NULL);
  g_signal_connect(widget,"drag_motion",G_CALLBACK (gtk_text_view_drag_motion),NULL);

  /* Set some defaults. */
  vte_terminal_set_audible_bell(terminal, audible);
  vte_terminal_set_cursor_blink_mode(terminal, cursor_blink_mode);
  vte_terminal_set_scroll_on_output(terminal, FALSE);
  vte_terminal_set_scroll_on_keystroke(terminal, TRUE);
  vte_terminal_set_scrollback_lines(terminal, lines);
  vte_terminal_set_mouse_autohide(terminal, TRUE);


  /* check if we are using a dark theme */
    
  style_context = gtk_widget_get_style_context (widget);
  gtk_style_context_get_color ( style_context, gtk_style_context_get_state ( style_context), &theme_fg);
  gtk_style_context_get_background_color ( style_context, gtk_style_context_get_state ( style_context), &theme_bg);

  /* The GTK-default Raleigh theme in 3.12 ends up assigning
   * black background and black foreground, which is clearly not
   * useful. */
  if (theme_fg.red == 0.0 && theme_fg.green == 0.0 && theme_fg.blue == 0.0 &&
      theme_bg.red == 0.0 && theme_bg.green == 0.0 && theme_bg.blue == 0.0)
    {
      theme_bg.red = 1.0;
      theme_bg.green = 1.0;
      theme_bg.blue = 1.0;
    }

  /* mix between dark_theme (TRUE|FALSE) and reverse (TRUE|FALSE) */
  
  dark_theme = ( theme_bg.red == 1.0 && theme_bg.green == 1.0 && theme_bg.blue == 1.0 ) ?
    FALSE : TRUE ;
  reverse = (dark_theme) ? ((reverse == -3) ? TRUE : !reverse)
    : ((reverse == -3) ? FALSE : reverse);
  
  if (!reverse)
    {
      back.red = back.green = back.blue = 1.0; back.alpha = 1.0;
      fore.red = fore.green = fore.blue = 0.0; fore.alpha = 1.0;
    }
  else
    {
      back.red = back.green = back.blue = 0.0; back.alpha = 1.0;
      fore.red = fore.green = fore.blue = 1.0; fore.alpha = 1.0;
    }
  
  if (transparent != NULL) {
    back.alpha = g_ascii_strtod (transparent, NULL);
    g_free (transparent);
  }

#ifdef NSP
  /* in NSP we try to obtain foregrounf and background from style_context after
   * widget is realized
   */
#if VTE_CHECK_VERSION(0,38,0)
  vte_terminal_set_colors(terminal, &fore, &back, NULL, 0);
#else
  vte_terminal_set_colors_rgba(terminal, &fore, &back, NULL, 0);
#endif
#endif

  if (cursor_color_string) {
    GdkRGBA rgba;
    if (parse_color (cursor_color_string, &rgba))
#if VTE_CHECK_VERSION(0,38,0)
      vte_terminal_set_color_cursor(terminal, &rgba);
#else
    vte_terminal_set_color_cursor_rgba(terminal, &rgba);
#endif
    g_free(cursor_color_string);
  }
  
#if VTE_CHECK_VERSION(0,38,0)
  if (highlight_foreground_color_string) {
    GdkRGBA rgba;
    if (parse_color (highlight_foreground_color_string, &rgba))
      vte_terminal_set_color_highlight_foreground(terminal, &rgba);
    g_free(highlight_foreground_color_string);
  }

  if (highlight_background_color_string) {
    GdkRGBA rgba;
    if (parse_color (highlight_background_color_string, &rgba))
      vte_terminal_set_color_highlight(terminal, &rgba);
    g_free(highlight_background_color_string);
  }
#endif

#if VTE_CHECK_VERSION(0,38,0)
  if (encoding != NULL) {
    if (!vte_terminal_set_encoding(terminal, encoding, &error)) {
      g_printerr("Failed to set encoding: %s\n", error->message);
    }
    g_free(encoding);

  }
#endif

#if VTE_CHECK_VERSION(0,38,0)
  if (cjk_ambiguous_width != NULL) {
    int width = 1;

    if (g_ascii_strcasecmp(cjk_ambiguous_width, "narrow") == 0)
      width = 1;
    else if (g_ascii_strcasecmp(cjk_ambiguous_width, "wide") == 0)
      width = 2;
    else
      g_printerr("Unrecognised value \"%s\" for --cjk-width\n",
		 cjk_ambiguous_width);
    g_free(cjk_ambiguous_width);

    vte_terminal_set_cjk_ambiguous_width(terminal, width);
  }
#endif


  vte_terminal_set_cursor_shape(terminal, cursor_shape);

#if VTE_CHECK_VERSION(0,38,0)
  vte_terminal_set_rewrap_on_resize(terminal, rewrap);
#endif
  /* Set the default font. */
  if (font) {
    PangoFontDescription *desc;

    desc = pango_font_description_from_string(font);
    vte_terminal_set_font(terminal, desc);
    pango_font_description_free(desc);
  }

  /* Match "abcdefg". */
  if (!no_builtin_dingus) {
    add_dingus (terminal, (char **) builtin_dingus);
  }
  if (dingus) {
    add_dingus (terminal, dingus);
    g_strfreev (dingus);
  }


  g_object_set_data (G_OBJECT (widget), "output_file", (gpointer) output_file);
  g_object_set_data (G_OBJECT (widget), "info",GINT_TO_POINTER (TRUE));

  /* Go for it! */
  g_signal_connect(widget, "child-exited", G_CALLBACK(child_exited), window);
  g_signal_connect(window, "delete-event", G_CALLBACK(delete_event), widget);

  add_weak_pointer(G_OBJECT(widget), &widget);
  add_weak_pointer(G_OBJECT(window), &window);

  gtk_widget_realize(widget);
  
  if (console) 
    {
      /* Open a "console" connection. */
      int consolefd = -1, yes = 1, watch;
      GIOChannel *channel;
      consolefd = open("/dev/console", O_RDONLY | O_NOCTTY);
      if (consolefd != -1) 
	{
	  /* Assume failure. */
	  console = FALSE;
#ifdef TIOCCONS
	  if (ioctl(consolefd, TIOCCONS, &yes) != -1) {
	    /* Set up a listener. */
	    channel = g_io_channel_unix_new(consolefd);
	    watch = g_io_add_watch(channel,
				   G_IO_IN,
				   read_and_feed,
				   widget);
	    g_signal_connect_swapped(widget,
				     "eof",
				     G_CALLBACK(disconnect_watch),
				     GINT_TO_POINTER(watch));
	    g_signal_connect_swapped(widget,
				     "child-exited",
				     G_CALLBACK(disconnect_watch),
				     GINT_TO_POINTER(watch));
	    g_signal_connect(widget,
			     "realize",
			     G_CALLBACK(take_xconsole_ownership),
			     NULL);
#ifdef VTE_DEBUG
	    vte_terminal_feed(terminal,
			      "Console log for ...\r\n",
			      -1);
#endif
	    /* Record success. */
	    console = TRUE;
	  }
#endif
	}
      else 
	{
	  /* Bail back to normal mode. */
	  g_warning(_("Could not open console.\n"));
	  close(consolefd);
	  console = FALSE;
	}
    }
  
  if (!console) 
    {
      if (shell) 
	{
	  GError *err = NULL;
	  char **command_argv = NULL;
	  int command_argc;
	  GPid pid = -1;
	  char *free_me = NULL;
	  /*
	    _VTE_DEBUG_IF(VTE_DEBUG_MISC)
	    vte_terminal_feed(terminal, message, -1);
	  */
	  if (command == NULL || *command == '\0')
	    command = free_me = vte_get_user_shell ();
	  
	  if (command == NULL || *command == '\0')
	    command = g_getenv ("SHELL");
	  
	  if (command == NULL || *command == '\0')
	    command = "/bin/sh";

	  if (!g_shell_parse_argv(command, &command_argc, &command_argv, &err) ||
#if VTE_CHECK_VERSION(0,38,0)
	      !vte_terminal_spawn_sync(terminal,
				       pty_flags,
				       NULL,
				       command_argv,
				       env_add,
				       G_SPAWN_SEARCH_PATH,
				       NULL, NULL,
				       &pid,
				       NULL /* cancellable */,
				       &err)
#else
	    !vte_terminal_fork_command_full(terminal,
					    pty_flags,
					    NULL,
					    command_argv,
					    env_add,
					    G_SPAWN_SEARCH_PATH,
					    NULL, NULL,
					    &pid,
					    &err)
#endif
	      )
	    {
	      g_warning("Failed to fork: %s\n", err->message);
	      g_error_free(err);
	    }
	  else 
	    {
	      /* g_print("Fork succeeded, PID %d\n", pid); */
	    }
	  g_free (free_me);
	  g_strfreev(command_argv);
	}
      else
	{
#if VTE_CHECK_VERSION(0,38,0)
#ifdef HAVE_FORK
	  GError *err = NULL;
	  VtePty *pty;
	  pid_t pid;
	  int i;
	  pty = vte_pty_new_sync(VTE_PTY_DEFAULT, NULL, &err);
	  if (pty == NULL) {
	    g_printerr ("Failed to create PTY: %s\n", err->message);
	    g_error_free(err);
	    return 1;
	  }
	  pid = fork();
	  switch (pid) {
	  case -1:
	    /* abnormal */
	    g_warning("Error forking: %s",
		      g_strerror(errno));
	    g_object_unref(pty);
	    break;
	  case 0:
	    /* child */
	    vte_pty_child_setup(pty);
	    for (i = 0; ; i++) {
	      switch (i % 3) {
	      case 0:
	      case 1:
		g_print("%d\n", i);
		break;
	      case 2:
		g_printerr("%d\n", i);
		break;
	      }
	      sleep(1);
	    }
	    _exit(0);
	    break;
	  default:
	    vte_terminal_set_pty(terminal, pty);
	    g_object_unref(pty);
	    vte_terminal_watch_child(terminal, pid);
	    g_print("Child PID is %d (mine is %d).\n",
		    (int) pid, (int) getpid());
	    /* normal */
	    break;
	  }
#endif /* HAVE_FORK */
#else 
	  long i;
	  G_GNUC_BEGIN_IGNORE_DEPRECATIONS;
	  i = vte_terminal_forkpty(terminal,
				   env_add, working_directory,
				   TRUE, TRUE, TRUE);
	  G_GNUC_END_IGNORE_DEPRECATIONS;
	  switch (i) {
	  case -1:
	    /* abnormal */
	    g_warning("Error in vte_terminal_forkpty(): %s",
		      strerror(errno));
	    break;
	  case 0:
	    /* child */
	    for (i = 0; ; i++) {
	      switch (i % 3) {
	      case 0:
	      case 1:
		g_print("%ld\n", i);
		break;
	      case 2:
		g_printerr("%ld\n", i);
		break;
	      }
	      sleep(1);
	    }
	    _exit(0);
	    break;
	  default:
	    g_print("Child PID is %ld (mine is %ld).\n",
		    (long) i, (long) getpid());
	    /* normal */
	    break;
	  }
#endif 
	}
    }
  
  if (geometry) {
    if (!gtk_window_parse_geometry (GTK_WINDOW(window), geometry)) {
      g_warning (_("Could not parse the geometry spec passed to --geometry"));
    }
  } else {
#if VTE_CHECK_VERSION(0,38,0)
    /* As of GTK+ 2.91.0, the default size of a window comes from its minimum
     * size not its natural size, so we need to set the right default size
     * explicitly */
    gtk_window_set_default_geometry (GTK_WINDOW (window),
				     vte_terminal_get_column_count (terminal),
				     vte_terminal_get_row_count (terminal));
#else
    gtk_window_set_default_size (GTK_WINDOW (window), 600,400);
#endif 
  }
  
#ifdef NSP
  gtk_widget_grab_focus (widget);
  gtk_widget_show_all(window);
  def_scale = vte_terminal_get_font_scale(VTE_TERMINAL(widget));
#else
  gtk_widget_show_all(window);
#endif

  g_log_set_default_handler(  _dummy, NULL);

  
  gtk_main();
  
  /* 
     g_assert(widget == NULL);
     g_assert(window == NULL);
  */
  
  if (keep) {
    while (TRUE) {
      sleep(60);
    }
  }
  return 0;
}

#if VTE_CHECK_VERSION(0,38,0)
/* already exists in VTE_CHECK_VERSION(0,38,0) */
#else 
/* Just some arbitrary minimum values */
#define MIN_COLUMNS (16)
#define MIN_ROWS    (2)

/**
 * vte_terminal_get_geometry_hints:
 * @terminal: a #VteTerminal
 * @hints: (out caller-allocates): a #GdkGeometry to fill in
 * @min_rows: the minimum number of rows to request
 * @min_columns: the minimum number of columns to request
 *
 * Fills in some @hints from @terminal's geometry. The hints
 * filled are those covered by the %GDK_HINT_RESIZE_INC,
 * %GDK_HINT_MIN_SIZE and %GDK_HINT_BASE_SIZE flags.
 *
 * See gtk_window_set_geometry_hints() for more information.
 *
 * @terminal must be realized (see gtk_widget_get_realized()).
 */

static void
vte_terminal_get_geometry_hints(VteTerminal *terminal,
                                GdkGeometry *hints,
                                int min_rows,
                                int min_columns)
{
  GtkWidget *widget;
  GtkBorder padding;

  g_return_if_fail(VTE_IS_TERMINAL(terminal));
  g_return_if_fail(hints != NULL);
  widget = &terminal->widget;
  g_return_if_fail(gtk_widget_get_realized(widget));

  gtk_style_context_get_padding(gtk_widget_get_style_context(widget),
				gtk_widget_get_state_flags(widget),
				&padding);

  hints->base_width  = padding.left + padding.right;
  hints->base_height = padding.top  + padding.bottom;
  hints->width_inc   = vte_terminal_get_char_width (terminal);
  hints->height_inc  = vte_terminal_get_char_height (terminal);
  hints->min_width   = hints->base_width  + hints->width_inc  * min_columns;
  hints->min_height  = hints->base_height + hints->height_inc * min_rows;
}

static void
vte_terminal_set_geometry_hints_for_window(VteTerminal *terminal,
                                           GtkWindow *window)
{
  GdkGeometry hints;

  g_return_if_fail(VTE_IS_TERMINAL(terminal));
  g_return_if_fail(gtk_widget_get_realized(&terminal->widget));

  vte_terminal_get_geometry_hints(terminal, &hints, MIN_ROWS, MIN_COLUMNS);
  gtk_window_set_geometry_hints(window,
				&terminal->widget,
				&hints,
				GDK_HINT_RESIZE_INC |
				GDK_HINT_MIN_SIZE |
				GDK_HINT_BASE_SIZE);
}

static void
vte_terminal_set_font_scale(VteTerminal *terminal,
                            gdouble scale)
{
  PangoFontDescription *desired;
  desired = pango_font_description_copy(vte_terminal_get_font(terminal));
  pango_font_description_set_size(desired,
				  CLAMP(scale, 4, 144) * PANGO_SCALE);
  vte_terminal_set_font(terminal, desired);
  pango_font_description_free(desired);
}
static gdouble
vte_terminal_get_font_scale(VteTerminal *terminal)
{
  gdouble newsize;
  PangoFontDescription *desired;
  desired = pango_font_description_copy(vte_terminal_get_font(terminal));
  newsize = pango_font_description_get_size(desired) / PANGO_SCALE;
  pango_font_description_free(desired);
  return newsize;
}
#endif

/* popup menu for nsp vteapp
 *
 */

/* added for nsp */

static gint font_def_size=0;

typedef struct _fsize_data fsize_data;
struct _fsize_data
{
  GtkWidget *widget;
  gpointer data;
};

static GtkWidget *popup_menu = NULL ;

/* menu for right click
 */

static void copy_cb (GtkWidget *widget)
{
  vte_terminal_copy_clipboard(VTE_TERMINAL(widget));
}

static void paste_cb (GtkWidget *widget)
{
  vte_terminal_paste_clipboard(VTE_TERMINAL(widget));
}

static void menu_increase_font_size(GtkWidget *widget, gpointer data)
{
  fsize_data *data1 = data;
  adjust_font_size(data1->widget,data1->data, 1.2);
}

static void menu_decrease_font_size(GtkWidget *widget, gpointer data)
{
  fsize_data *data1 = data;
  adjust_font_size(data1->widget,data1->data, 1. / 1.2);
}

static void menu_normal_font_size(GtkWidget *widget, gpointer data)
{
  fsize_data *data1 = data;
  VteTerminal *terminal = VTE_TERMINAL(data1->widget);
  gdouble scale = vte_terminal_get_font_scale(terminal);
  adjust_font_size(data1->widget,data1->data, def_scale/scale );
}

static void menu_reset(GtkWidget *widget, gpointer data)
{
  fsize_data *data1 = data;
  VteTerminal *terminal = VTE_TERMINAL(data1->widget);
  vte_terminal_reset(terminal,TRUE,TRUE);
  vte_terminal_feed_child(terminal, "\n",-1);
}

GtkWidget *nsp_create_menu (GtkWidget *wterminal,  gpointer data)
{
  GtkWidget *menu=NULL;
  GtkWidget *menuitem=NULL;
  VteTerminal *terminal =  VTE_TERMINAL(wterminal);
  static fsize_data data1={NULL,NULL};

  g_return_val_if_fail(GTK_IS_WINDOW(data),NULL);

  if ( popup_menu != NULL) gtk_widget_destroy (popup_menu);

  popup_menu = menu = gtk_menu_new ();

  /* copy */
  menuitem = gtk_menu_item_new_with_mnemonic("Copy");

  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  gtk_widget_show (menuitem);
  g_signal_connect_swapped (menuitem, "activate",
			    G_CALLBACK (copy_cb),wterminal);
  gtk_widget_set_sensitive (menuitem,vte_terminal_get_has_selection (terminal) ? TRUE: FALSE);

  /* paste */
  menuitem = gtk_menu_item_new_with_mnemonic("_Paste");
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  gtk_widget_show (menuitem);
  g_signal_connect_swapped (menuitem, "activate",
			    G_CALLBACK (paste_cb),wterminal);

  /* zoom data */
  data1.data = data;
  data1.widget = wterminal;

  /* zoom in */
  menuitem = gtk_menu_item_new_with_mnemonic("Zoom _In");

  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  gtk_widget_show (menuitem);
  g_signal_connect(menuitem, "activate", G_CALLBACK(menu_increase_font_size),&data1);
  
  /* zoom out */
  menuitem = gtk_menu_item_new_with_mnemonic("Zoom _Out");
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  gtk_widget_show (menuitem);
  g_signal_connect(menuitem, "activate", G_CALLBACK(menu_decrease_font_size),&data1);

  /* zoom def */
  menuitem = gtk_menu_item_new_with_mnemonic("Normal _Size");
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  gtk_widget_show (menuitem);
  g_signal_connect(menuitem, "activate", G_CALLBACK(menu_normal_font_size),&data1);

  /* reset */
  menuitem = gtk_menu_item_new_with_mnemonic("Reset");
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  gtk_widget_show (menuitem);
  g_signal_connect(menuitem, "activate", G_CALLBACK(menu_reset),&data1);

  return menu;
}

static int
button_pressed(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  GtkWidget *menu;
  VteTerminal *terminal;
  char *match;
  int tag;
  gint xpad, ypad;

  switch (event->button) {
  case 3:
    terminal = VTE_TERMINAL(widget);
    menu =   nsp_create_menu (widget,data);
    if ( menu == NULL) return FALSE;
    gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
		    NULL, NULL,0,gtk_get_current_event_time());
    return TRUE;
    break;
  case 1:
  case 2:
  default:
    break;
  }
  return FALSE;
}

