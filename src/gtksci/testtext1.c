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


struct _Buffer
{
  gint refcount;
  GtkTextBuffer *buffer;
  GtkTextTag *not_editable_tag;
  GtkTextTag *custom_tabs_tag;
  GtkTextMark *mark;

};

struct _View
{
  GtkWidget *window;
  GtkWidget *text_view;
  GtkAccelGroup *accel_group;
  Buffer *buffer;
};

static Buffer * create_buffer      (void);
static View *view_from_widget (GtkWidget *widget);
static View *create_view      (Buffer *buffer);
static void  check_close_view (View   *view);
static void  close_view       (View   *view);
static void nsp_insert_prompt(const char *prompt);



/*
 *
 */

#include "nsp-logo.xpm" 

void fill_buffer_with_logo (View *view)
{
  GtkTextIter iter,start,end;
  GdkPixbuf *pixbuf;
  pixbuf = gdk_pixbuf_new_from_xpm_data (nsp_logo_xpm);
  gtk_text_buffer_get_iter_at_offset (view->buffer->buffer, &iter, 0);
  gtk_text_buffer_insert_pixbuf (view->buffer->buffer, &iter, pixbuf);
  gtk_text_buffer_insert (view->buffer->buffer, &iter, "\n",-1);
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);
  g_object_unref (pixbuf);
}

static gint
delete_event_cb (GtkWidget *window, GdkEventAny *event, gpointer data)
{
  View *view = view_from_widget (window);
  check_close_view (view);
  return TRUE;
}

/*
 * Menu callbacks
 */

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

enum
{
  RESPONSE_FORWARD,
  RESPONSE_BACKWARD
};



#define N_COLORS 16

static Buffer *
create_buffer (void)
{
  Buffer *buffer;
  PangoTabArray *tabs;
  
  buffer = g_new (Buffer, 1);

  buffer->buffer = gtk_text_buffer_new (NULL);
  
  buffer->refcount = 1;

  buffer->not_editable_tag =
    gtk_text_buffer_create_tag (buffer->buffer, NULL,
                                "editable", FALSE,
                                "foreground", "purple", NULL);

  tabs = pango_tab_array_new_with_positions (4,
                                             TRUE,
                                             PANGO_TAB_LEFT, 10,
                                             PANGO_TAB_LEFT, 30,
                                             PANGO_TAB_LEFT, 60,
                                             PANGO_TAB_LEFT, 120);
  
  buffer->custom_tabs_tag = gtk_text_buffer_create_tag (buffer->buffer, NULL,
                                                        "tabs", tabs,
                                                        "foreground", "green", NULL);

  buffer->mark = NULL;

  pango_tab_array_free (tabs);

  return buffer;
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
      g_object_unref (buffer->buffer);
      g_free (buffer);
    }
}



static void
close_view (View *view)
{
  buffer_unref (view->buffer);
  gtk_widget_destroy (view->window);
  g_free (view);
}

static void
check_close_view (View *view)
{
  close_view (view);
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

    }
}



typedef struct _view_history view_history;

struct _view_history {
  GList *history, *history_tail, *history_cur;
  int history_size, dir ; /* dir = 0,1,-1 */
};

#define MAX_HISTORY_SIZE 1000

static void nsp_append_history(char *text,view_history *data)
{
  if ( data == NULL) return ;
  if (data->history_tail == NULL) 
    {
      data->history = g_list_append (NULL, g_strdup (text));
      data->history_tail =data->history_cur= data->history;
      data->dir = 0;
    } 
  else if (text[0] != '\0' && strcmp (text, data->history_tail->data) != 0) 
    {
      /* do not insert repetitions */
      g_list_append (data->history_tail, g_strdup (text));
      data->history_tail =data->history_cur= data->history_tail->next;
      data->dir = 0;
    }
  else 
    {
      data->history_cur=data->history_tail;
      data->dir = 0;
    }
  if (data->history_size == MAX_HISTORY_SIZE) 
    {
      g_free (data->history->data);
      data->history = g_list_delete_link (data->history, data->history);
      data->history_tail =data->history_cur= data->history;
      data->dir = 0;
    } 
  else 
    {
      data->history_size++;
    }
}

char *nsp_xhistory_up(view_history *data)
{
  if ( data == NULL) return NULL;
  if (data->dir != 0 && data->history_cur->prev != NULL) {
    data->history_cur = data->history_cur->prev;
  }
  data->dir = 1;
  return data->history_cur->data;
}

char *nsp_xhistory_down(view_history *data)
{
  if ( data == NULL) return NULL;
  if ( data->dir == 0 ) return NULL;
  if ( data->history_cur->next != NULL) {
    data->history_cur = data->history_cur->next;
  }
  data->dir = -1;
  return data->history_cur->data;
}



/* dealing with keypressed in text view 
 *
 *
 */

static gchar *nsp_expr=NULL;
static int count=0;

static gboolean
key_press_text_view(GtkWidget *widget, GdkEventKey *event, gpointer xdata)
{
  GtkTextIter start, end,iter;
  static view_history *data  = NULL;
  GtkTextMark *cursor_mark;
  View *view= xdata;
  char *str; 
  if ( data == NULL) 
    {
      data =malloc (sizeof(view_history));
      data->history = data->history_tail = NULL;
      data->history_cur = NULL;
      data->history_size = 0;     
      /* XXXXX A finir */
      g_object_set_data_full (G_OBJECT(widget),"myhistory",data,NULL);
    }

  fprintf(stderr,"key pressed \n");
  switch ( event->keyval ) 
    {
    case GDK_Return:
      {
	gchar *search_string;
	GtkTextBuffer *buffer;
	buffer = view->buffer->buffer;
	gtk_text_buffer_get_bounds (buffer, &start, &end);
	if ( view->buffer->mark != NULL )
	  {
	    gtk_text_buffer_get_iter_at_mark (buffer, &iter,view->buffer->mark);
	    search_string = gtk_text_iter_get_text (&iter, &end);
	  }
	else 
	  {
	    search_string = gtk_text_iter_get_text (&start, &end);
	  }
	fprintf(stderr,"<%s>/n",search_string);
	nsp_append_history(search_string,data);
	gtk_text_buffer_insert (buffer, &end,
				"\n",-1);
	gtk_text_buffer_get_bounds (buffer, &start, &end);
	if ( view->buffer->mark == NULL) 
	  view->buffer->mark = gtk_text_buffer_create_mark (buffer, NULL, &end, TRUE);
	else 
	  gtk_text_buffer_move_mark (buffer, view->buffer->mark, &end);
	
	gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view), 
				      view->buffer->mark,
				      0, TRUE, 0.0, 1.0);
	gtk_text_buffer_apply_tag (view->buffer->buffer,
				   view->buffer->not_editable_tag,
				   &start, &end);
	/* ZZZ */
	fprintf(stderr,"return pressed \n");
	if ( strcmp(search_string,"cut")==0)
	  {
	    gtk_text_buffer_delete(view->buffer->buffer,&start,&end);
	    gtk_text_buffer_move_mark (buffer, view->buffer->mark, &end);
	  }
	else if  ( strcmp(search_string,"top")==0)
	  {
	    fprintf(stderr,"scroll to top\n");
	    gtk_text_buffer_get_iter_at_mark (buffer, &iter,view->buffer->mark);
	    gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW (view->text_view),&iter,
					 0.0,TRUE,
					 0.0,0.0);
	  }
	g_signal_stop_emission_by_name (widget, "key_press_event");
	nsp_expr= search_string;
	gtk_main_quit();
      }
      return TRUE;
    case GDK_Up:
      str = nsp_xhistory_up(data);
      fprintf(stdout,"up pressed\n");
      if ( str != NULL) 
	{
	  fprintf(stdout,"insert text\n");
	  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
	  if ( view->buffer->mark != NULL) 
	    {
	      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,view->buffer->mark);
	      gtk_text_buffer_delete(view->buffer->buffer,&iter,&end);
	    }
	  gtk_text_buffer_insert (view->buffer->buffer, &end, str, -1);
	}
      g_signal_stop_emission_by_name (widget, "key_press_event");
      return TRUE;
      break;
    case GDK_Down:
      str = nsp_xhistory_down(data);
      fprintf(stdout,"down pressed\n");
      if ( str != NULL) 
	{
	  fprintf(stdout,"insert text\n");
	  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
	  if ( view->buffer->mark != NULL) 
	    {
	      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,view->buffer->mark);
	      gtk_text_buffer_delete(view->buffer->buffer,&iter,&end);
	    }
	  gtk_text_buffer_insert (view->buffer->buffer, &end, str, -1);
	}
      g_signal_stop_emission_by_name (widget, "key_press_event");
      return TRUE;
      break;
    default:
      /* if we are at a position when insertion is not possible 
       * we jump to end of text view where we are allowed 
       * to enter text 
       */

      cursor_mark = gtk_text_buffer_get_insert (view->buffer->buffer);
      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,cursor_mark);
      if ( ! gtk_text_iter_can_insert (&iter,GTK_TEXT_VIEW(view->text_view)->editable) )
	{
	  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
	  gtk_text_buffer_place_cursor (view->buffer->buffer,&end);
	}

      break;
    }
  return FALSE;
}


static gint timeout_command (void *v)
{
  if ( checkqueue_nsp_command() == TRUE) 
    {
      gtk_main_quit();
    }
  return TRUE;
}

/* enter a gtk_main waiting for char and 
 * dealing with events.
 */


int Xorgetchar(void)
{
  guint timer;
  if ( nsp_check_events_activated()== FALSE) return(getchar());
  if ( nsp_expr != NULL ) 
    {
      int val1 = nsp_expr[count];
      if (count <= strlen(nsp_expr))
	{
	  g_print ("char returned '%c'\n",val1);
	  val1 = nsp_expr[count];
	  count++;
	}
      else
	{
	  g_print ("char returned <return>\n");
	  val1 = '\n';
	  /* g_free(nsp_expr);*/ nsp_expr=NULL;count=0;
	}
      return val1;
    }
  timer=  gtk_timeout_add(100,  (GtkFunction) timeout_command , NULL);
  gtk_main();
  gtk_timeout_remove(timer);
  count=1;
  g_print ("char returned '%c'\n",nsp_expr[0]);
  return nsp_expr[0];
}


/* DefSciReadLine will enter here 
 *
 */

char *readline_base(const char *prompt)
{
  static int use_prompt=1;
  guint timer;
  if ( nsp_check_events_activated()== FALSE) return readline(prompt);
  if ( use_prompt == 1) 
    {
      nsp_insert_prompt(prompt);
    }
  fprintf(stderr,"in my readline\n");
  timer=  gtk_timeout_add(100,  (GtkFunction) timeout_command , NULL);
  gtk_main();
  gtk_timeout_remove(timer);
  if ( checkqueue_nsp_command() == TRUE) 
    {
      char buf[256];
      dequeue_nsp_command(buf,255);
      use_prompt=0;
      return g_strdup(buf);
    }
  else 
    {
      use_prompt=1;
      g_print ("string returned '%s'\n",nsp_expr);
      return g_strdup(nsp_expr);
    }
  return NULL;
}



static View *
create_view (Buffer *buffer)
{
  View *view;
  
  GtkWidget *sw;
  GtkWidget *vbox;
  GtkWidget *menu;
  
  view = g_new0 (View, 1);

  view->buffer = buffer;
  buffer_ref (buffer);
  
  view->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  g_object_set_data (G_OBJECT (view->window), "view", view);
  
  g_signal_connect (view->window, "delete_event",
		    G_CALLBACK (delete_event_cb), NULL);

  /* 
  view->accel_group = gtk_accel_group_new ();
  gtk_window_add_accel_group (GTK_WINDOW (view->window), view->accel_group);
   XXXX accel group to share with menu
  */
  
  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (view->window), vbox);
  menu = create_main_menu(view->window);  
  gtk_box_pack_start(GTK_BOX(vbox),menu,FALSE,FALSE,0);
  
  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);

  view->text_view = gtk_text_view_new_with_buffer (buffer->buffer);
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (view->text_view),
                               GTK_WRAP_WORD);

  gtk_container_set_border_width (GTK_CONTAINER (view->text_view),
                                  5);

  g_signal_connect (view->text_view,
                    "key_press_event",
                    G_CALLBACK (key_press_text_view),
                    view);  

  g_signal_connect (view->buffer->buffer,
		    "mark_set",
		    G_CALLBACK (cursor_set_callback),
		    view->text_view);
  
  gtk_box_pack_start (GTK_BOX (vbox), sw, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (sw), view->text_view);

  gtk_widget_set_size_request (GTK_WIDGET (view->window),600,400);
  /*   gtk_window_set_default_size (GTK_WINDOW (view->window), 600, 400); */

  gtk_widget_grab_focus (view->text_view);
  
  gtk_widget_show_all (view->window);

  fill_buffer_with_logo (view);

  return view;
}

static  View *view;
static char buf[1025];

int  Sciprint2textview(const char *fmt, va_list ap)
{
  GtkTextBuffer *buffer;
  GtkTextIter start, end;
  int n;
  n= vsnprintf(buf,1024 , fmt, ap );
  buffer = view->buffer->buffer;
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_insert (buffer, &end,buf,-1);
  if ( view->buffer->mark == NULL) 
    view->buffer->mark = gtk_text_buffer_create_mark (buffer, NULL, &end, TRUE);
  else 
    gtk_text_buffer_move_mark (buffer, view->buffer->mark, &end);
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);
  /* ZZZ */
  return n;
}

static void nsp_insert_prompt(const char *prompt)
{
  GtkTextBuffer *buffer;
  GtkTextIter start, end;
  buffer = view->buffer->buffer;
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_insert (buffer, &end,prompt,-1);
  if ( view->buffer->mark == NULL) 
    view->buffer->mark = gtk_text_buffer_create_mark (buffer, NULL, &end, TRUE);
  else 
    gtk_text_buffer_move_mark (buffer, view->buffer->mark, &end);
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);
  /* scroll if needed */
  gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view), 
				view->buffer->mark,
				0, TRUE, 0.0, 1.0);
}


void nsp_create_main_text_view()
{
  Buffer *buffer;
  buffer = create_buffer ();
  view = create_view (buffer);
  buffer_unref (buffer);
  SetScilabIO(Sciprint2textview);
}


