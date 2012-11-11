/* The GIMP -- an image manipulation program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * The GIMP Help Browser
 * Copyright (C) 1999-2002 Sven Neumann <sven@gimp.org>
 *                         Michael Natterer <mitch@gimp.org>
 *
 * Some code & ideas stolen from the GNOME help browser.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/* 
 * adapted from the gimp for nsp 
 * the only exported function is 
 * int nsp_help_browser(char *mandir,char *locale,char *help_file) 
 */

#include <string.h> 
#include <stdio.h> 
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <libgtkhtml/gtkhtml.h>

extern gchar *html_selection_get_text (HtmlView *view);

#include <nsp/nsp.h> 
#include <nsp/sciio.h>
#include <nsp/gtksci.h>
#include <nsp/system.h>
#include <nsp/interf.h>
#include <nsp/nsptcl.h>
#include <nsp/hash.h>
#include <nsp/file.h>
#include <nsp/smatrix.h>
#include <nsp/gtksci.h>

#include "queue.h"
#include "uri.h"
#include "../system/regexp.h"

/*  defines  */

#define _(x) x 

#ifdef __EMX__
#define chdir _chdir2
#endif

enum
{
  BUTTON_HOME,
  BUTTON_INDEX,
  BUTTON_BACK,
  BUTTON_FORWARD
};

typedef struct
{
  const gchar *title;
  const gchar *ref;
  gint         count;
} HistoryItem;

static const gchar *eek_png_tag = "<h1>Eeek!</h1>";

static gchar       *gimp_help_root = NULL;
static GList       *history = NULL;
static Queue       *queue;
static gchar       *current_ref;
static GtkWidget   *html;
static GtkWidget   *back_button;
static GtkWidget   *forward_button;
static GtkWidget   *combo;

static GtkWidget *window = NULL; /* jpc */

static GtkTargetEntry help_dnd_target_table[] =
{
  { "_NETSCAPE_URL", 0, 0 },
};

/*  forward declaration  */

static void     load_page   (const gchar  *ref,
                             gboolean      add_to_queue);
static void     request_url (HtmlDocument *doc,
                             const gchar  *url,
                             HtmlStream   *stream,
                             gpointer      data);
static gboolean io_handler  (GIOChannel   *io, 
                             GIOCondition  condition, 
                             gpointer      data);

/*
 * 
 */

static void nsp_void_print_handler(const gchar *format, ...)
{
}
  

/* Taken from glib/gconvert.c:
 * Test of haystack has the needle prefix, comparing case
 * insensitive. haystack may be UTF-8, but needle must
 * contain only ascii.
 */
static gboolean
has_case_prefix (const gchar *haystack, const gchar *needle)
{
  const gchar *h = haystack;
  const gchar *n = needle;

  while (*n && *h && g_ascii_tolower (*n) == g_ascii_tolower (*h))
    {
      n++;
      h++;
    }
  
  return (*n == '\0');
}

static void
close_callback (GtkWidget *widget,
		gpointer   user_data)
{
  gtk_widget_destroy(window); 
  window=NULL;
}

static void
update_toolbar (void)
{
  if (back_button)
    gtk_widget_set_sensitive (back_button, queue_has_prev (queue));
  if (forward_button)
    gtk_widget_set_sensitive (forward_button, queue_has_next (queue));
}

static void
button_callback (GtkWidget *widget,
                 gpointer   data)
{
  const gchar *ref;

  switch (GPOINTER_TO_INT (data))
    {
    case BUTTON_HOME:
      load_page ("manual.html", TRUE);
      break;

    case BUTTON_INDEX:
      load_page ("manualindex.html", TRUE);
      break;

    case BUTTON_BACK:
      if (!(ref = queue_prev (queue)))
        return;
      load_page (ref, FALSE);
      queue_move_prev (queue);
      break;

    case BUTTON_FORWARD:
      if (!(ref = queue_next (queue)))
        return;
      load_page (ref, FALSE);
      queue_move_next (queue);
      break;

    default:
      return;
    }

  update_toolbar ();
}

static void 
entry_changed_callback (GtkWidget *widget,
			gpointer   data)
{
  GList       *list;
  HistoryItem *item;
  gchar       *compare_text;
  gboolean     found = FALSE;
  gchar       *active_text;

  /* newly allocated text */
  active_text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(widget));
  if ( active_text == NULL) return;
  /* Sciprintf("In combo callback with %s\n",active_text); */
  for (list = history; list && !found; list = list->next)
    {
      item = (HistoryItem *) list->data;
      if (item->count)
        {
          compare_text = g_strdup_printf ("%s <%i>",
                                          item->title, item->count + 1);
        }
      else
        {
          compare_text = (gchar *) item->title;
        }
      if (strcmp (compare_text, active_text) == 0)
	{
	  /* Sciprintf("Found in history compare_text=%s, active_text=%s\n",compare_text,active_text); */
	  load_page (item->ref, TRUE);
	  found = TRUE;
	}
      if (item->count)
        {
          g_free (compare_text);
        }
    }
  if ( active_text != NULL)  g_free(active_text);
}

static void
history_add (const gchar *ref,
	     const gchar *title)
{
  GList       *list;
  GList       *found = NULL;
  HistoryItem *item;
  gint         title_found_count = 0;

  for (list = history; list && !found; list = list->next)
    {
      item = (HistoryItem *) list->data;

      if (strcmp (item->title, title) == 0)
	{
	  if (strcmp (item->ref, ref) != 0)
	    {
	      title_found_count++;
	      continue;
	    }

	  found = list;
        }
    }

  if (! found)
    {
      gchar *combo_title;
      item = g_new (HistoryItem, 1);
      item->ref   = g_strdup (ref);
      item->title = g_strdup (title);
      item->count = title_found_count;
      history = g_list_prepend (history, item);
      if (item->count)
	combo_title = g_strdup_printf ("%s <%i>",
				       item->title,
				       item->count + 1);
      else
	combo_title = g_strdup (item->title);
      g_signal_handlers_block_by_func (GTK_COMBO_BOX(combo), entry_changed_callback,NULL);
      gtk_combo_box_prepend_text(GTK_COMBO_BOX (combo),combo_title);
      g_signal_handlers_unblock_by_func (GTK_COMBO_BOX(combo), entry_changed_callback,NULL);
      g_free(combo_title);
    }
}

static void
title_changed (HtmlDocument *doc,
               const gchar  *new_title,
               gpointer      data)
{
  gchar *title;
  if (!new_title) new_title = (_("<Untitled>"));
  title = g_strstrip (g_strdup (new_title));
  history_add (current_ref, title);
  /* 
  g_signal_handlers_block_by_func (GTK_COMBO_BOX(combo),entry_changed_callback,NULL);
  gtk_combo_box_set_title(GTK_COMBO_BOX(combo), title);
  g_signal_handlers_unblock_by_func (GTK_COMBO_BOX (combo),entry_changed_callback,NULL);
  */
  g_free (title);
}


static void
load_page (const gchar *ref,	  
	   gboolean     add_to_queue)
{
  HtmlDocument *doc = HTML_VIEW (html)->document;
  gchar        *abs;
  gchar        *new_ref;
  gchar        *anchor;

  g_return_if_fail (ref != NULL);

  abs = uri_to_abs (ref, current_ref);

  g_return_if_fail (abs != NULL);

  anchor = strchr (ref, '#');
  if (anchor && anchor[0] && anchor[1])
    {
      new_ref = g_strconcat (abs, anchor, NULL);
      anchor += 1;
    }
  else
    {
      new_ref = g_strdup (abs);
      anchor = NULL;
    }

  if (strcmp (current_ref, abs))
    {
      if (!has_case_prefix (abs, "file:/"))
        {
          /* load_remote_page (ref); XXXX */
          return;
        }

      html_document_clear (doc);
      html_document_open_stream (doc, "text/html");
      gtk_adjustment_set_value (gtk_layout_get_vadjustment (GTK_LAYOUT (html)),
                                0);
      
      request_url (doc, abs, doc->current_stream, NULL);
    }

  if (anchor)
    html_view_jump_to_anchor (HTML_VIEW (html), anchor);

  g_free (current_ref);
  current_ref = new_ref;

  if (add_to_queue) 
    queue_add (queue, new_ref);
  
  update_toolbar ();
}

static void
link_clicked (HtmlDocument *doc,
              const gchar  *url,
              gpointer      data)
{
  load_page (url, TRUE);
}

static void
request_url (HtmlDocument *doc,
             const gchar  *url,
             HtmlStream   *stream,
             gpointer      data)
{
  gchar *abs;
  gchar *filename;

  g_return_if_fail (url != NULL);
  g_return_if_fail (stream != NULL);

  abs = uri_to_abs (url, current_ref);
  if (!abs)
    return;

  filename = g_filename_from_uri (abs, NULL, NULL);

  if (filename)
    {
      gint fd;

      fd = open (filename, O_RDONLY);

      if (fd == -1)
        {
          gchar *name;
          gchar *msg;

          name = g_filename_to_utf8 (filename, -1, NULL, NULL, NULL);

          msg = g_strdup_printf
            ("<html>"
             "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />"
             "<head><title>%s</title></head>"
             "<body bgcolor=\"white\">"
             "<div align=\"center\">"
             "<div>%s</div>"
             "<h3>%s</h3>"
             "<tt>%s</tt>"
             "</div>"
             "<br /><br />"
             "<div align=\"justify\">%s</div>"
             "</body>"
             "</html>",
             _("Document Not Found"),
             eek_png_tag,
             _("Could not locate help documentation"),
             name,
             _("The requested document could not be found in your Nsp help "
               "path as shown above. This means that the topic has not yet "
               "been written or your installation is not complete. Ensure "
               "that your installation is complete before reporting this "
               "error as a bug."));
          
          /* html_document_write_stream (doc, msg, strlen (msg)); */
          html_stream_write (stream, msg,strlen(msg));
          html_stream_close (stream);

	  /* g_print ("pb with document: \"%s\" ",name); */

          g_free (msg);
          g_free (name);
        }
      else
        {
          GIOChannel *io = g_io_channel_unix_new (fd);

          g_io_channel_set_close_on_unref (io, TRUE);
          g_io_channel_set_encoding (io, NULL, NULL);

          g_io_add_watch (io, G_IO_IN | G_IO_ERR | G_IO_HUP | G_IO_NVAL, 
                          io_handler, stream);
        }

      g_free (filename);
    }

  g_free (abs);
}

static gboolean
io_handler (GIOChannel   *io,
            GIOCondition  condition, 
            gpointer      data)
{
  HtmlStream *stream;
  gchar       buffer[8192];
  gsize       bytes;

  stream = (HtmlStream *) data;

  if (condition & G_IO_IN) 
    {
      if (g_io_channel_read_chars (io, buffer, sizeof (buffer),
                                   &bytes, NULL) != G_IO_STATUS_ERROR
          && bytes > 0)
        {
          html_stream_write (stream, buffer, bytes);
        }
      else
	{
          html_stream_close (stream);
          g_io_channel_unref (io);

	  return FALSE;
	}

      if (condition & G_IO_HUP) 
        {
          while (g_io_channel_read_chars (io, buffer, sizeof (buffer),
                                          &bytes, NULL) != G_IO_STATUS_ERROR
                 && bytes > 0)
            {
              html_stream_write (stream, buffer, bytes);
            }
        }
    }

  if (condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL)) 
    {
      html_stream_close (stream);
      g_io_channel_unref (io);

      return FALSE;
    }

  return TRUE;
}

static void
drag_begin (GtkWidget      *widget,
            GdkDragContext *context,
            gpointer        data)
{
  gtk_drag_set_icon_stock (context, GTK_STOCK_JUMP_TO, -8, -8);
}

static void
drag_data_get (GtkWidget        *widget, 
               GdkDragContext   *context,
               GtkSelectionData *selection_data,
               guint             info,
               guint             time,
               gpointer          data)
{
  if (! current_ref)
    return;

  gtk_selection_data_set (selection_data,
                          selection_data->target,
                          8, 
                          (const guchar *) current_ref, 
                          strlen (current_ref));
}




/* Add a menu for right click 
 * which copy to clipboard. 
 */ 

static GtkWidget *popup_menu = NULL ; 
static gchar *copy_cb_str = NULL;

static void
copy_cb (GtkWidget *widget)
{
  if (copy_cb_str) 
    {
      GtkClipboard *clipboard;
      GdkDisplay *display;
      /* GdkAtom board= GDK_SELECTION_PRIMARY; */
      GdkAtom board= GDK_SELECTION_CLIPBOARD; 
      display = gtk_widget_get_display(widget);
      clipboard= gtk_clipboard_get_for_display(display, board);
      gtk_clipboard_set_text(clipboard, copy_cb_str, -1);
      /* fprintf(stderr,"%s",str); */
      /* nsp_input_feed( copy_cb_str); */
      g_free (copy_cb_str);
    }
}

static GtkWidget *create_copy_menu (GtkWidget *html)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  
  if ( popup_menu != NULL) gtk_widget_destroy (popup_menu);

  popup_menu = menu = gtk_menu_new ();
  
  menuitem = gtk_image_menu_item_new_from_stock (GTK_STOCK_COPY, NULL);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  gtk_widget_show (menuitem);
  g_signal_connect_swapped (menuitem, "activate",
			    G_CALLBACK (copy_cb),html);

  copy_cb_str = html_selection_get_text (HTML_VIEW (html));
  gtk_widget_set_sensitive (menuitem, (copy_cb_str == NULL) ?  FALSE : TRUE );

  return menu;


}

static int
button_pressed(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
  GtkWidget *menu;
  
  switch (event->button) {
  case 3:
    menu =   create_copy_menu (widget);
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


static void
open_browser_dialog (const gchar *help_path,
		     const gchar *locale,
		     const gchar *help_file)
{
  /* GtkWidget *window; */
  GtkWidget *vbox;
  GtkWidget *hbox;
  GtkWidget *bbox;
  GtkWidget *scroll;
  GtkWidget *button;
  GtkWidget *drag_source;
  GtkWidget *image;
  gchar     *eek_png_path;
  GdkColor color;
  
  start_sci_gtk(); /* in case gtk was not initialized */

  if ( window != NULL) 
    {
      load_page (help_file, TRUE); 
      return;
    }

  gdk_color_parse("white", &color);

  if ( window != NULL) 
    {
      return ;
    }

  eek_png_path = g_build_filename (gimp_help_root, "images", "eek.png", NULL);

  if (g_file_test (eek_png_path, G_FILE_TEST_EXISTS))
    eek_png_tag = g_strdup_printf ("<img src=\"%s\">", eek_png_path);

  g_free (eek_png_path);

  /*  the dialog window  */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  g_signal_connect (window, "destroy",
                    G_CALLBACK (close_callback),
                    NULL);
  gtk_window_set_wmclass (GTK_WINDOW (window), "helpbrowser", "Nsp");
  gtk_window_set_title (GTK_WINDOW (window), _("Nsp Help Browser"));

  /* gimp_help_connect (window, gimp_standard_help_func, "dialogs/help.html"); */

  vbox = gtk_vbox_new (FALSE, 2);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_show (vbox);

  /*  buttons  */
  bbox = gtk_hbutton_box_new ();
  gtk_button_box_set_layout (GTK_BUTTON_BOX (bbox), GTK_BUTTONBOX_START);
  gtk_box_pack_start (GTK_BOX (vbox), bbox, FALSE, FALSE, 0);
  gtk_widget_show (bbox);

  button = gtk_button_new_from_stock (GTK_STOCK_HOME);
  gtk_container_add (GTK_CONTAINER (bbox), button);
  gtk_widget_show (button);
  g_signal_connect (button, "clicked",
                    G_CALLBACK (button_callback),
                    GINT_TO_POINTER (BUTTON_HOME));

  button = gtk_button_new_from_stock (GTK_STOCK_INDEX);
  gtk_container_add (GTK_CONTAINER (bbox), button);
  gtk_widget_show (button);
  g_signal_connect (button, "clicked",
                    G_CALLBACK (button_callback),
                    GINT_TO_POINTER (BUTTON_INDEX));

  back_button = button = gtk_button_new_from_stock (GTK_STOCK_GO_BACK);
  gtk_container_add (GTK_CONTAINER (bbox), button);
  gtk_widget_set_sensitive (GTK_WIDGET (button), FALSE);
  g_signal_connect (button, "clicked",
                    G_CALLBACK (button_callback),
                    GINT_TO_POINTER (BUTTON_BACK));
  gtk_widget_show (button);

  forward_button = button = gtk_button_new_from_stock (GTK_STOCK_GO_FORWARD);
  gtk_container_add (GTK_CONTAINER (bbox), button);
  gtk_widget_set_sensitive (GTK_WIDGET (button), FALSE);
  g_signal_connect (button, "clicked",
                    G_CALLBACK (button_callback),
                    GINT_TO_POINTER (BUTTON_FORWARD));
  gtk_widget_show (button);

  hbox = gtk_hbox_new (FALSE, 2);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
  gtk_widget_show (hbox);

  /*  the drag source  */
  drag_source = gtk_event_box_new ();
  gtk_box_pack_start (GTK_BOX (hbox), drag_source, FALSE, FALSE, 4);
  gtk_widget_show (drag_source);

  gtk_drag_source_set (GTK_WIDGET (drag_source),
                       GDK_BUTTON1_MASK,
                       help_dnd_target_table,
                       G_N_ELEMENTS (help_dnd_target_table), 
                       GDK_ACTION_MOVE | GDK_ACTION_COPY);
  g_signal_connect (drag_source, "drag_begin",
                    G_CALLBACK (drag_begin),
                    NULL);
  g_signal_connect (drag_source, "drag_data_get",
                    G_CALLBACK (drag_data_get),
                    NULL);
  
  image = gtk_image_new_from_stock (GTK_STOCK_JUMP_TO, GTK_ICON_SIZE_BUTTON);
  gtk_container_add (GTK_CONTAINER (drag_source), image);
  gtk_widget_show (image);

  /*  the title combo  */
  combo = gtk_combo_box_entry_new_text ();
  gtk_widget_set_size_request (GTK_WIDGET (combo), 360, -1);
  gtk_box_pack_start (GTK_BOX (hbox), combo, TRUE, TRUE, 0);
  gtk_widget_show (combo);

  g_signal_connect (GTK_COMBO_BOX(combo), "changed",
                    G_CALLBACK (entry_changed_callback), 
                    NULL);
  
  /*  HTML view  */
  html  = html_view_new ();
  queue = queue_new ();

  gtk_widget_set_size_request (GTK_WIDGET (html), 600, 600);
  gtk_widget_modify_bg(GTK_WIDGET (html), GTK_STATE_NORMAL,  &color );
  
  scroll = 
    gtk_scrolled_window_new (gtk_layout_get_hadjustment (GTK_LAYOUT (html)),
                             gtk_layout_get_vadjustment (GTK_LAYOUT (html)));
  
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll),
                                  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (vbox), scroll, TRUE, TRUE, 0);
  gtk_widget_show (scroll);

  gtk_container_add (GTK_CONTAINER (scroll), html);
  gtk_widget_show (html);
  
  html_view_set_document (HTML_VIEW (html), html_document_new ());

  g_signal_connect (HTML_VIEW (html)->document, "title_changed",
                    G_CALLBACK (title_changed),
                    NULL);
  g_signal_connect (HTML_VIEW (html)->document, "link_clicked",
                    G_CALLBACK (link_clicked),
                    NULL);
  g_signal_connect (HTML_VIEW (html)->document, "request_url",
                    G_CALLBACK (request_url),
                    NULL);

  g_signal_connect(G_OBJECT(html), "button-press-event",
		   G_CALLBACK(button_pressed),html);

  gtk_widget_show (window);

  current_ref = g_strconcat ("file://", help_path, "/", locale, "/", NULL);

  load_page (help_file, TRUE);
}




/*------------------------------------------------------
 * mandir = man path or null (SCI+'man')
 * locale = "eng" or "fr" 
 * help_file = null or absolute (XXX) file name 
 * returns 0 on success and 1 if index.html not found 
 *------------------------------------------------------*/

static int nsp_help_topic(const char *topic,char *buf);

int nsp_help_browser(char *mandir,char *locale,char *help_file) 
{
  char buf[FSIZE+1];
  GPrintFunc old;
  const char *sci = nsp_getenv("SCI"); 
  char *l = locale ; /* (locale == NULL) ? "eng": locale ;  */
  if ( mandir == NULL && sci != NULL) 
    mandir = g_strconcat (sci, G_DIR_SEPARATOR_S, "man",G_DIR_SEPARATOR_S,  "html",  NULL);
  /* ignore g_print in libgtkhtml library */
  old=g_set_print_handler ((GPrintFunc) nsp_void_print_handler);
  /* expand topic -> filename in buf */
  if ( help_file == NULL )
    {
#ifdef WIN32 
      /* X: -> //X/ and \ -> / */
      if ( strncmp(mandir+1,":",1)==0) 
	{
	  char *str = mandir+2;
	  int i;
	  strcpy(buf,"//");buf[2]=mandir[0];
	  for ( i = 0 ; i < strlen(str)+1 ; i++)
	    {
	      if ( str[i] == '\\') buf[i+2]='/';
	      else buf[i+2]= str[i];
	    }
	}
      else 
	{
	  strcat(buf,mandir);
	}
#else 
      strcpy(buf,mandir);
#endif 
      strcat(buf,"/generated/manual.html");
      open_browser_dialog (mandir,l,buf);
    }
  else if ( strncmp(help_file,"file:",5)==0 || strncmp(help_file,"http:",5)==0) 
    {
      strcpy(buf,help_file);
      open_browser_dialog (mandir,l,buf);
    }
  else  
    {
      if ( nsp_help_topic(help_file,buf)== FAIL ) return FAIL; 
      if ( buf[0]== '\0') return OK; 
      open_browser_dialog (mandir,l,buf);
    }
  return 0; 
}

static NspHash *nsp_help_table = NULLHASH;

/* XXX*/
/* extern nsp_string nsp_dirname(const char *fileName);*/

static int nsp_help_fill_help_table(const char *index_file)
{
  nsp_string dirname;
  nsp_tcldstring name,filename;
  int all=TRUE;
  char buf[FSIZE+1];
  NspSMatrix *S = NULL;
  int xdr= FALSE,swap = TRUE,i;
  NspFile *F;
  char *mode = "r";
  if ( index_file != NULL) 
    nsp_path_expand(index_file,buf,FSIZE);
  else 
    nsp_path_expand("NSP/man/html/generated/manual.4dx",buf,FSIZE);

  if ((dirname = nsp_dirname (buf))== NULL)return FAIL;

  if ((F=nsp_file_open(buf,mode,xdr,swap)) == NULLSCIFILE)
    {
      Scierror("Error %f not found\n",buf);
      return FAIL;
    }
  if ( nsp_fscanf_smatrix(F,&S) == FAIL) 
    {
      nsp_file_close(F);
      return FAIL;
    }
  if (nsp_file_close(F) == FAIL  ) return FAIL;
  /* initialize hash table for help */
  if (  nsp_help_table == NULLHASH) 
    {
      if ( ( nsp_help_table = nsp_hash_create("%help",S->mn) ) == NULLHASH) 
	return  FAIL;
    }
  for ( i = 0 ; i < S->mn ; i++)
    {
      char *str,*str1;
      NspObject *Obj;
      int nmatch;
      char patterns[]={ "^[^{]*{([^{]*)\\|LNK{([^{]*)}{[^$]*}"};
      Tcl_RegExp regExpr;
      nsp_tcldstring_init(&name);
      if (( regExpr = nsp_tclregexp_compile(patterns)) == NULL) goto bug;
      if ((nsp_tcl_regsub(S->S[i],regExpr,"\\1",&name,&nmatch,all))==FAIL)  goto bug;

      nsp_tcldstring_init(&filename);
      if ((nsp_tcl_regsub(S->S[i],regExpr,"\\2",&filename,&nmatch,all))==FAIL)  goto bug;
      str = str1= nsp_tcldstring_value(&name);
      /* remove \ from names */
      while ( *str1 != '\0') 
	{
	  if (*str1 == '\\' ) str1++;
	  else *str++=*str1++;
	}
      *str='\0';
      /* fprintf(stderr,"val=[%s]\n",nsp_tcldstring_value(&name));*/
      /* need a join here */
      /* Sciprintf("dirname for help [%s]\n",dirname); */
#ifdef WIN32 
      if ( strncmp(dirname+1,":",1)==0) 
	{
	  strcpy(buf,"//X");buf[2]=dirname[0];
	}
      else 
	{
	  buf[0]=dirname[0];buf[1]=dirname[1];
	}
      strcat(buf,dirname+2);
#else 
      strcpy(buf,dirname);
#endif 
      strcat(buf,"/");
      strcat(buf,nsp_tcldstring_value(&filename));
      if (( Obj = nsp_new_string_obj(nsp_tcldstring_value(&name),buf,-1))
	  == NULLOBJ)
	goto bug;
      nsp_tcldstring_free(&name);	 
      nsp_tcldstring_free(&filename);	
      
      if (nsp_hash_enter(nsp_help_table,Obj) == FAIL) goto bug;
    }
  return OK;
 bug:
  nsp_tcldstring_free(&name);	 
  nsp_tcldstring_free(&filename);	
  return FAIL;
}


static int nsp_help_topic(const char *topic, char *buf)
{
  NspObject *Obj;
  if ( nsp_help_table == NULLHASH)
    {
      /* initialize */
      if ( nsp_help_fill_help_table(NULL) == FAIL 
	   || nsp_help_table == NULLHASH)
	{
	  Scierror("Error: cannot build help table \n");
	  return FAIL;
	}
    }
  if ( strcmp(topic + strlen(topic) -3,"4dx")==0)
    {
      /* add contents of index file to help table */
      if ( nsp_help_fill_help_table(topic) == FAIL ) 
	{
	  Scierror("Error: cannot add index file to help table \n");
	  return FAIL;
	}
      return OK;
    }

  if ( nsp_hash_find(nsp_help_table,topic,&Obj)== FAIL) 
    {
      Sciprintf("No man for %s\n",topic);
      strcpy(buf,"");
      return OK;
    }
  else
    {
      strcpy(buf,((NspSMatrix *) Obj)->S[0]);
      /* Sciprintf("topic help file: [%s]\n",buf);  */
    }
  return OK;
}


/*
static void nsp_input_feed_example(char *example)
{
  char *pos = example, *tmpdir;
  FILE *fd;
  gchar *fname,*instr; 

  if (( tmpdir=nsp_getenv("NSP_TMPDIR")) == NULL) 
    {
      Sciprintf("NSP_TMPDIR not set \r\n");
      return;
    }
  
  fname = g_strconcat (tmpdir, G_DIR_SEPARATOR_S, "example.sce",NULL);
  if ( fname == NULL) return ;

  if ((fd = fopen(fname,"w"))==NULL) return ;

  while ( 1 ) 
    {
      while ( *pos != '&' && *pos != '\0' ) 
	{
	  putc(*pos++,fd);
	}
      if ( *pos == '&') 
	{
	  if ( strncmp(pos,"&#10;",5) ==0) 
	    {
	      putc('\n',fd);

	      pos = pos +5;
	    }
	  else if ( strncmp(pos,"&quot;",6) ==0) 
	    {
	      putc('"',fd);
	      pos = pos +6;
	    }
	  else if ( strncmp(pos,"&amp;",5) ==0) 
	    {
	      putc('&',fd);
	      pos = pos +5;
	    }
	  else 
	    {
	      putc(*pos++,fd);
	    }
	}
      else if ( *pos == '\0') 
	{
	  break;
	}
    }

  fclose(fd);
  
  instr = g_strconcat("exec('",fname,"',7);",NULL);
  nsp_input_feed(instr);
  g_free(instr);
  g_free(fname);

}
  
*/



