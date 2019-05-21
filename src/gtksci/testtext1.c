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
 *
 *
 * Adapted from the testtext.c file in gtk+/tests
 * to be used as a terminal for Nsp jpc (2006-2019).
 * Note: you can use
 *   gconftool-2 -s /desktop/gnome/interface/gtk_key_theme -t string Emacs
 * to set up emacs editing mode in gtk widgets but note that some editing
 * keys are redefined here as in emacs mode.
 *
 */

#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <signal.h>
#include <unistd.h> /* for isatty */
#include <readline/readline.h>
#include <readline/history.h>

#include <nsp/nsp.h>
#include <nsp/nsptcl.h> /* for nsp_get_extension */
#include <nsp/file.h>
#include <nsp/smatrix.h>
#include <nsp/gtksci.h>
#include <nsp/command.h>
#include <nsp/sciio.h>
#include <nsp/eval.h>
#include <nsp/nspthreads.h>
#include <nsp/system.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#ifdef WIN32
#define sigsetjmp(x,y) setjmp(x)
#define siglongjmp(x,y) longjmp(x,y)
#endif

#include "nsp-logo.xpm"

extern void controlC_handler (int sig);
extern void controlC_handler_void (int sig);
extern GtkWidget *create_main_menu( GtkWidget  *window);
extern Get_char nsp_set_getchar_fun(Get_char F);
extern SciReadFunction nsp_set_readline_fun(SciReadFunction F);
extern char *nsp_prompt(void);

typedef struct _view_history view_history;

struct _view_history {
  GList *history, *history_tail, *history_cur;
  int history_size, dir ; /* dir = 0,1,-1 */
};

#define MAX_HISTORY_SIZE 512

typedef struct _Buffer Buffer;

struct _Buffer
{
  gint refcount;
  GtkTextBuffer *buffer;
  GtkTextTag *not_editable_tag;
  GtkTextTag *center_tag;
  GtkTextMark *mark;
  GtkTextMark *completion_mark;
};

typedef struct _View View;

struct _View
{
  GtkWidget *window;
  GtkWidget *text_view;
  GtkAccelGroup *accel_group;
  Buffer *buffer;
  view_history *view_history;
  gchar *nsp_expr; /* used to store string to pass to nsp interpreter*/
  GAsyncQueue *read_queue, *write_queue;
  gpointer user_data;
};

static jmp_buf my_env;
/* static View *view=NULL; */

static Buffer *nsptv_create_buffer(void);
static View   *nsptv_create_view(Buffer *buffer);
static void    nsp_insert_prompt(View *view,const char *prompt);
static void    nsp_eval_pasted_from_clipboard(const gchar *nsp_expr,View *view,
					      int position, GtkTextIter iter);
static int     Xorgetchar_textview(void);
static void    readline_textview(Tokenizer *T,char *prompt, char *buffer,
				 int *buf_size, int *len_line, int *eof);
static int     nsp_print_to_textview(const char *fmt, va_list ap);
static int  nsp_print_to_textview_internal(View *view,const char *fmt, va_list ap);
static View *nsptv_get_view(const char *str);

#ifdef NSP_WITH_MAIN_GTK_THREAD
static void nsp_insert_prompt_thread(View *view,const char *prompt);
static void nsp_textview_wait_for_text(View *view);
static void nsp_textview_awake_for_text(View *view);
static void nsp_textview_wait_for_end_print_text(View *view);
static void nsp_textview_awake_for_end_print_text(View *view);
#else
static void    nsp_textview_gtk_main(void);
static void    nsp_textview_gtk_main_quit(void);
#endif

/**
 * nsptv_insert_logo:
 * @view:
 *
 * insert the nsp logo in textview at first position.
 * must be called from gtk main thread
 *
 **/

static void nsptv_insert_logo (View *view)
{
  GtkTextIter iter,start,end;
  GdkPixbuf *pixbuf;
  pixbuf = gdk_pixbuf_new_from_xpm_data (nsp_logo);
  gtk_text_buffer_get_iter_at_offset (view->buffer->buffer, &iter, 0);
  gtk_text_buffer_insert_pixbuf (view->buffer->buffer, &iter, pixbuf);
  gtk_text_buffer_insert (view->buffer->buffer, &iter, "\n",-1);
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);
  g_object_unref (pixbuf);
}

/**
 * nsptv_delete_callback:
 * @window:
 * @event:
 * @data:
 *
 * delete callback for the text_view widget
 *
 * Returns:
 **/

static gint nsptv_delete_callback (GtkWidget *window, GdkEventAny *event, gpointer data)
{
  /* take care here that we want to quit the gtk_main */
  sci_clear_and_exit(0);
  return TRUE;
}

/**
 * nsptv_create_buffer:
 * @void:
 *
 * GtkTextBuffer creation.
 *
 * Returns: a new #Buffer object.
 **/

static Buffer *nsptv_create_buffer (void)
{
  Buffer *buffer;
  buffer = g_new (Buffer, 1);
  buffer->buffer = gtk_text_buffer_new (NULL);
  buffer->refcount = 1;
  buffer->not_editable_tag =
#ifdef BEBUG_EDITABLE
    gtk_text_buffer_create_tag (buffer->buffer, NULL,
                                "editable", FALSE,
                                "foreground", "purple", NULL);
#else
  gtk_text_buffer_create_tag (buffer->buffer, NULL,
			      "editable", FALSE,NULL);
#endif
  buffer->center_tag =
    gtk_text_buffer_create_tag (buffer->buffer,NULL,
				"justification", GTK_JUSTIFY_CENTER,NULL,
				"editable",FALSE,"foreground", "purple", NULL);
  buffer->mark = NULL;
  buffer->completion_mark = gtk_text_mark_new("completion",FALSE);
  return buffer;
}

static void nsptv_buffer_ref (Buffer *buffer)
{
  buffer->refcount++;
}

static void nsptv_buffer_unref (Buffer *buffer)
{
  buffer->refcount--;
  if (buffer->refcount == 0)
    {
      g_object_unref (buffer->buffer);
      g_free (buffer);
    }
}


static void nsptv_cursor_set_callback (GtkTextBuffer     *buffer,
				       const GtkTextIter *location,
				       GtkTextMark       *mark,
				       gpointer           user_data)
{
  /* Redraw tab windows if the cursor moves
   * on the mapped widget (windows may not exist before realization).
   */
  GtkTextView *text_view = GTK_TEXT_VIEW (user_data);
  if (gtk_widget_get_mapped (GTK_WIDGET (text_view)) &&
      mark == gtk_text_buffer_get_insert (buffer))
    {

    }
}

static void nsptv_append_history(char *text,view_history *data, int readline_add)
{
  /* readline history */
  if ( readline_add == TRUE)  add_history (text);
  /* textview history */
  if ( data == NULL) return ;
  if ( data->history_tail == NULL)
    {
      /* first insertion.
       */
      data->history = g_list_append (NULL, g_strdup (text));
      data->history_tail =data->history_cur= data->history;
      data->dir = 0;
      return;
    }
  if (text[0] != '\0' && strcmp (text, data->history_tail->data) != 0)
    {
      /* do not insert repetitions */
      GList *loc=  g_list_append(data->history_tail, g_strdup (text));
      if ( loc == NULL) {}
      data->history_tail =data->history_cur= data->history_tail->next;
      data->dir = 0;
    }
  else
    {
      /* no insertion */
      data->history_cur=data->history_tail;
      data->dir = 0;
      return;
    }

  if (data->history_size == MAX_HISTORY_SIZE)
    {
      /* delete the first item in history
       * but we should only get here if an insertion
       * was performed.
       */
      g_free (data->history->data);
      data->history = g_list_delete_link (data->history, data->history);
      data->dir = 0;
    }
  else
    {
      data->history_size++;
    }
}

static void nsptv_clear_history(View *view)
{
  view_history *data = view->view_history;
  GList *history = data->history;
  while ( history != NULL)
    {
      g_free (history->data);
      history = g_list_delete_link (history, history);
    }
}

static char *nsptv_history_up(view_history *data)
{
  if ( data == NULL) return NULL;
  if ( data->history_cur == NULL) return NULL;
  if (data->dir != 0 && data->history_cur->prev != NULL) {
    data->history_cur = data->history_cur->prev;
  }
  data->dir = 1;
  return data->history_cur->data;
}

static char *nsptv_history_down(view_history *data)
{
  if ( data == NULL) return NULL;
  if ( data->history_cur == NULL) return NULL;
  if ( data->dir == 0 ) return NULL;
  if ( data->history_cur->next != NULL) {
    data->dir = -1;
    data->history_cur = data->history_cur->next;
  } else {
    data->dir = 0;
    return NULL;
  }
  return data->history_cur->data;
}

/* delete the info text added when completion is inserted
 */

static void nsptv_delete_completion_infos(View *view)
{
  GtkTextIter start, end;
  GtkTextMark *completion_mark;
  /* test if we have a completion mark by testing if the text buffer contains a
   * mark named completion.
   * if yes then delete the characters from the completion mark (in fact the completion mark + one
   * character) to the end of the buffer.
   */
  completion_mark = gtk_text_buffer_get_mark(view->buffer->buffer, "completion");
  if ( completion_mark == NULL) return ;
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
  gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &start,
				    completion_mark);
  /* delete from start+1 to end of buffer
   */
  gtk_text_iter_forward_char (&start);
  gtk_text_buffer_delete(view->buffer->buffer,&start,&end);
  gtk_text_buffer_delete_mark(view->buffer->buffer,
			      view->buffer->completion_mark);
}

/* insert possible completions for pathnames using
 * readline to build the possible list.
 */

static void nsptv_insert_completions(View *view)
{
  char str_expanded[FSIZE+1];
  GtkTextIter start, end,iter;
  int i=1,ln;
  char **matches;
  gchar *search_string=NULL, *str;
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
  /*
   * search_string is the current statement inserted after the nsp prompt.
   */
  if ( view->buffer->mark != NULL )
    {
      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,view->buffer->mark);
      search_string = gtk_text_iter_get_text (&iter, &end);
    }
  else
    {
      search_string = gtk_text_iter_get_text (&start, &end);
    }

  if ( (ln= strlen(search_string)) == 0) return;
  str = search_string;
  for ( i = ln -1 ; i >= 0 ; i--)
    {
      char c = search_string[i];
      if ( c == '"' || c == '\'' || c == ' ' ) str = search_string + i+1;
    }
  nsp_path_expand(str,str_expanded,FSIZE);
  matches = rl_completion_matches (str_expanded, rl_filename_completion_function);
  if ( matches == NULL || matches[0] == NULL ) return ;
  /* we insert the proposed completion */

  gtk_text_buffer_insert (view->buffer->buffer, &end,
			  matches[0] +strlen(str_expanded) ,-1);
  if ( matches[1] != NULL )
    {
      int i=1;
      /* fix the completion mark before inserting possible completions */
      gtk_text_iter_backward_char (&end);
      gtk_text_buffer_add_mark(view->buffer->buffer, view->buffer->completion_mark, &end);
      gtk_text_iter_forward_char (&end);
      gtk_text_buffer_insert (view->buffer->buffer, &end,"\n",-1);
      while ( matches[i] != NULL)
	{
	  gtk_text_buffer_insert (view->buffer->buffer, &end,matches[i],-1);
	  gtk_text_buffer_insert (view->buffer->buffer, &end,"\n",-1);
	  i++;
	}
      /* put the mark before the completion */
      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,
					view->buffer->completion_mark);
      gtk_text_iter_forward_char (&iter);
      gtk_text_buffer_place_cursor (view->buffer->buffer,&iter);
    }
  i=0;while ( matches[i] != NULL) { free(matches[i]);i++;} free(matches);
}


static void nsptv_key_press_return(View *view,int stop_signal)
{
  GtkTextIter start, end,iter;
  gchar *search_string=NULL;
  GtkTextBuffer *buffer = view->buffer->buffer;
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
  if ( search_string[0] != '\0' && search_string[0] != '\n' )
    nsptv_append_history(search_string,  view->view_history, TRUE);
  if (1) /* search_string[strlen(search_string)-1] != '\n') */
    {
      gtk_text_buffer_insert (buffer, &end, "\n",-1);
      gtk_text_buffer_get_bounds (buffer, &start, &end);
      if ( view->buffer->mark == NULL)
	view->buffer->mark = gtk_text_buffer_create_mark (buffer, NULL, &end, TRUE);
      else
	gtk_text_buffer_move_mark (buffer, view->buffer->mark, &end);
    }
  gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view),
				view->buffer->mark,
				0, TRUE, 0.0, 1.0);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);
  gtk_text_buffer_place_cursor (view->buffer->buffer,&end);
  if ( stop_signal == TRUE )
    g_signal_stop_emission_by_name (view->text_view, "key_press_event");
  view->nsp_expr= search_string;
#ifdef NSP_WITH_MAIN_GTK_THREAD
  nsp_textview_awake_for_text(view);
#else
  nsp_textview_gtk_main_quit();
#endif
}

/* callback dealing with keypressed in text view
 */

#if defined(USE_SHARP_SIGN)
#define GDK_KEY(name) GDK_KEY_##name
#else
#define GDK_KEY(name) GDK_KEY_/**/name
#endif

static gboolean nsptv_key_press_callback(GtkWidget *widget, GdkEventKey *event, gpointer xdata)
{
  GtkTextIter start, end,iter;
  GtkTextMark *cursor_mark;
  View *view= xdata;
  char *str;
  view_history *data  = view->view_history;

  /* delete extra info added by completion  */
  nsptv_delete_completion_infos(view);

  /*fprintf(stderr,"key pressed\n");*/
  switch ( event->keyval )
    {
    case GDK_KEY(Page_Up):
    case GDK_KEY(Page_Down):
      return FALSE;
    case GDK_KEY(Tab ):
      nsptv_insert_completions(view);
      return TRUE;
    case GDK_KEY(KP_Enter):
    case GDK_KEY(Return):
      nsptv_key_press_return(view,TRUE);
      return TRUE;
    case GDK_KEY(Up):
      if ( event->state & GDK_SHIFT_MASK ) {
        return FALSE;
      }
      goto up;
      break;
    case GDK_KEY(Down):
      if ( event->state & GDK_SHIFT_MASK ) {
        return FALSE;
      }
      goto down;
      break;
    case 'p':
      if ( event->state & GDK_CONTROL_MASK )
	{
	  goto up;
	}
      goto def;
      break;
    case 'n':
      if ( event->state & GDK_CONTROL_MASK )
	{
	  goto down;
	}
      goto def;
      break;
    case 'c':
      if ( event->state & GDK_CONTROL_MASK )
	{
	  goto ctrl_c;
	}
      goto def;
      break;
    case 'l' :
      if ( event->state & GDK_CONTROL_MASK )
	{
	  goto ctrl_l;
	}
      goto def;
      break;
    case GDK_KEY(Left ):
      if ( event->state & GDK_SHIFT_MASK ) {
        return FALSE;
      }
      cursor_mark = gtk_text_buffer_get_insert (view->buffer->buffer);
      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,cursor_mark);
      gtk_text_iter_backward_char (&iter);
      if (gtk_text_iter_can_insert (&iter,gtk_text_view_get_editable(GTK_TEXT_VIEW(view->text_view)))) {
        gtk_text_buffer_place_cursor (view->buffer->buffer,&iter);
        gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW (view->text_view),
                                     &iter,
                                     0, FALSE, 0.0, 1.0);
      } else {
        if ( view->buffer->mark != NULL) {
          gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,view->buffer->mark);
          gtk_text_buffer_place_cursor (view->buffer->buffer,&iter);
          gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view),
                                        view->buffer->mark,
                                        0, TRUE, 1.0, 1.0);
          }

      }
      return TRUE;
    case 'b' :
      if ( event->state & GDK_CONTROL_MASK )
	{
	  cursor_mark = gtk_text_buffer_get_insert (view->buffer->buffer);
	  gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,cursor_mark);
	  gtk_text_iter_backward_char (&iter);
	  if (gtk_text_iter_can_insert (&iter,gtk_text_view_get_editable(GTK_TEXT_VIEW(view->text_view))))
	    {
	      gtk_text_buffer_place_cursor (view->buffer->buffer,&iter);
	    }
          gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW (view->text_view),
					&iter,
					0, TRUE, 0.0, 1.0);
	  return TRUE;
	}
      goto def;
      break;
    case 'a':
      if ( event->state & GDK_CONTROL_MASK )
	{
	  if ( view->buffer->mark != NULL)
	    {
	      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,view->buffer->mark);
	      gtk_text_buffer_place_cursor (view->buffer->buffer,&iter);
              gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view),
					    view->buffer->mark,
					    0, TRUE, 1.0, 1.0);
	    }
	  return TRUE;
	}
      goto def;
      break;
 case GDK_KEY(KP_Home):
 case GDK_KEY(Home):
      if ( event->state & GDK_SHIFT_MASK ) {
        gtk_text_buffer_get_selection_bounds(view->buffer->buffer,&start, &end);
      }
      if (view->buffer->mark != NULL) {
        gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,view->buffer->mark);
        gtk_text_buffer_place_cursor (view->buffer->buffer,&iter);
        if ( event->state & GDK_SHIFT_MASK ) {
          gtk_text_buffer_select_range(view->buffer->buffer,&iter,&end);
        }
        /*gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view),
	*			view->buffer->mark,
	*			0, TRUE, 1.0, 1.0);
        */
       }
      return TRUE;
 case GDK_KEY(KP_End):
 case GDK_KEY(End):
      if ( event->state & GDK_SHIFT_MASK ) {
        gtk_text_buffer_get_selection_bounds(view->buffer->buffer,&start, &end);
      }
      if (view->buffer->mark != NULL) {
        gtk_text_buffer_get_bounds (view->buffer->buffer, &end, &iter);
        gtk_text_buffer_place_cursor (view->buffer->buffer,&iter);
        if ( event->state & GDK_SHIFT_MASK ) {
          gtk_text_buffer_select_range(view->buffer->buffer,&iter,&start);
        }
        cursor_mark = gtk_text_buffer_get_insert (view->buffer->buffer);
        gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view),
                                      cursor_mark,
                                      0, TRUE, 1.0, 1.0);

       }
      return TRUE;
 case GDK_KEY(Control_L ):
 case GDK_KEY(Control_R ):
      /* fprintf(stderr,"un controle \n"); */
      return FALSE;
    default:
      goto def;
      break;
    }
  return FALSE;
 up:    str = nsptv_history_up(data);
  /* fprintf(stdout,"up pressed\n"); */
  if (str != NULL) {
    /* fprintf(stdout,"insert text\n"); */
    gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
    if (view->buffer->mark != NULL) {
      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,view->buffer->mark);
      gtk_text_buffer_delete(view->buffer->buffer,&iter,&end);
    }
    gtk_text_buffer_insert (view->buffer->buffer, &end, str, -1);
    gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
    gtk_text_buffer_place_cursor (view->buffer->buffer,&end);
    cursor_mark = gtk_text_buffer_get_insert (view->buffer->buffer);
    gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW (view->text_view),
				   cursor_mark,
				   0, TRUE, 1.0, 1.0);
  }
  g_signal_stop_emission_by_name (widget, "key_press_event");
  return TRUE;
 down:
  str = nsptv_history_down(data);
  /* fprintf(stdout,"down pressed\n"); */
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
  if (view->buffer->mark != NULL) {
     gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,view->buffer->mark);
     gtk_text_buffer_delete(view->buffer->buffer,&iter,&end);
  }
  if (str != NULL) {
    /* fprintf(stdout,"insert text\n"); */
    gtk_text_buffer_insert (view->buffer->buffer, &end, str, -1);
    gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
    gtk_text_buffer_place_cursor (view->buffer->buffer,&end);
    cursor_mark = gtk_text_buffer_get_insert (view->buffer->buffer);
    gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW (view->text_view),
				   cursor_mark,
				   0, TRUE, 1.0, 1.0);
  } else {
    if (view->buffer->mark != NULL) {
        gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view),
				view->buffer->mark,
				0, TRUE, 1.0, 1.0);
    }
  }
  g_signal_stop_emission_by_name (widget, "key_press_event");
  return TRUE;
 ctrl_l :
  /* clear the terminal window */
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
  gtk_text_buffer_delete(view->buffer->buffer,&start,&end);
  gtk_text_buffer_move_mark (view->buffer->buffer, view->buffer->mark, &end);
  /* nsp_textview_insert_logo(view); */
  nsptv_key_press_return(view,TRUE);
  return TRUE;
 ctrl_c:
  /* fprintf(stderr,"un controle C\n"); */
  return FALSE;
 def:
  /* if we are at a position when insertion is not possible
   * we jump to end of text view where we are allowed
   * to enter text
   */
  cursor_mark = gtk_text_buffer_get_insert (view->buffer->buffer);
  gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,cursor_mark);
  if ( ! gtk_text_iter_can_insert (&iter,gtk_text_view_get_editable(GTK_TEXT_VIEW(view->text_view))))
    {
      gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
      gtk_text_buffer_place_cursor (view->buffer->buffer,&end);
      cursor_mark = gtk_text_buffer_get_insert (view->buffer->buffer);
      gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW (view->text_view),
				   cursor_mark,
				   0, FALSE, 1.0, 1.0);
    }
  /* we can here return TRUE or FALSE
   * TRUE and character won't be echoed
   * FALSE and character will be echoed
   */
  return FALSE;
}

void nsp_clc(void)
{
  GtkTextIter start,end;
  View *view= nsptv_get_view("clc");
  if ( view == NULL )
    {
      /* send a Ctrl-l to the terminal */
      printf("\033[2J\033[H");
      return;
    }
  else
    {
      gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
      gtk_text_buffer_delete(view->buffer->buffer,&start,&end);
      gtk_text_buffer_move_mark (view->buffer->buffer, view->buffer->mark, &end);
      /* nsp_textview_insert_logo(view); */
      nsptv_key_press_return(view,TRUE);
    }
}

/* paste with middle button, redefined  */

static gint
nsptv_button_press_callback(GtkWidget *widget, GdkEventButton *event,gpointer xdata)
{
  GtkTextIter iter;
  /* GtkTextBuffer *buffer; */
  View *view= xdata;
  GtkTextView *text_view =GTK_TEXT_VIEW(view->text_view);
  gtk_widget_grab_focus (view->text_view);


  if (event->type == GDK_BUTTON_PRESS && event->button == 2 )
    {
      gint x,y;
      GtkClipboard *clipboard = gtk_widget_get_clipboard (GTK_WIDGET (text_view),
							    GDK_SELECTION_PRIMARY);
      gchar *str = gtk_clipboard_wait_for_text(clipboard);
      /* fprintf(stderr,"A clipboard text: %s\n",str); */
      g_signal_stop_emission_by_name (text_view, "button_press_event");
      /* place the cursor mark at clicked position */
      gtk_text_view_window_to_buffer_coords(text_view,GTK_TEXT_WINDOW_WIDGET,
					    event->x,event->y,&x,&y);
      gtk_text_view_get_iter_at_position (text_view,&iter,NULL,x,y);
      gtk_text_iter_forward_char (&iter);

      /* gtk_text_buffer_place_cursor (view->buffer->buffer,&iter); */
      /* buffer = view->buffer->buffer; */
      nsptv_delete_completion_infos(view);
      if ( str )
	{
	  nsp_eval_pasted_from_clipboard(str,view,0,iter);
	  g_free(str);
	}
      return TRUE;
    }
  return FALSE;
}

/**
 * Xorgetchar_textview:
 * @void:
 *
 * wait for a character when top level
 * is a textview. This function is only used
 * through  nsp_set_getchar_fun(Xorgetchar_textview).
 *
 * Returns: an integer
 **/

static int Xorgetchar_textview(void)
{
  static int count=0;
  View *view;
  if ( nsp_check_events_activated()== FALSE) return(getchar());
  view = nsptv_get_view("Xorgetchar_textview");
  if ( view->nsp_expr != NULL )
    {
      int val1;
      if (count <= strlen(view->nsp_expr))
	{
	  val1 = view->nsp_expr[count];
	  g_print ("char returned '%c'\n",val1);
	  count++;
	}
      else
	{
	  g_print ("char returned <return>\n");
	  val1 = '\n';
	  /* g_free(nsp_expr);*/ view->nsp_expr=NULL;count=0;
	}
      return val1;
    }

#ifdef NSP_WITH_MAIN_GTK_THREAD
  nsp_textview_wait_for_text(view);
#else
  nsp_textview_gtk_main();
#endif

  count=1;
  g_print ("char returned '%c'\n",view->nsp_expr[0]);
  return view->nsp_expr[0];
}

/**
 * nsp_eval_pasted_from_clipboard:
 * @nsp_expr: a string to be evaluated
 * @view: the view object.
 *
 * used for evaluation of sequence obtained
 * by drag-drop or copy-paste. If the sequence
 * is a string without return we paste it into the textview
 * else we evaluate the sequence.
 *
 **/

static void nsp_eval_pasted_from_clipboard(const gchar *nsp_expr,View *view, int position, GtkTextIter iter)
{
  GtkTextIter start, end;
  int  i;
  NspSMatrix *S = nsp_smatrix_split_string(nsp_expr,"\n",1);
  if ( S->mn == 1 && strlen(nsp_expr) >=1 && nsp_expr[strlen(nsp_expr)-1] != '\n')
    {
      if (gtk_text_iter_can_insert (&iter,gtk_text_view_get_editable(GTK_TEXT_VIEW(view->text_view))))
	{
	  gtk_text_buffer_insert (view->buffer->buffer, &iter, nsp_expr,-1);
	  /* set the cursor at the end of insertion :
	   * not done since it also has the effect that the selection is unselected
	   */
	  /* gtk_text_buffer_place_cursor (view->buffer->buffer,&iter); */
	}
      else
	{
	  /* insert at the end */
	  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
	  gtk_text_buffer_insert (view->buffer->buffer, &end, nsp_expr,-1);
	  /* set the cursor at the end of insertion :
	   * not done since it also has the effect that the selection is unselected
	   */
	}
      return;
    }
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
  gtk_text_buffer_insert (view->buffer->buffer, &end, "\n",-1);
  for ( i = 0 ; i < S->mn ; i++ )
    {
      nsptv_append_history(S->S[i], view->view_history, TRUE);
    }
  /* rep =*/  nsp_parse_eval_from_smat(S,TRUE,TRUE,FALSE,FALSE);
  nsp_smatrix_destroy(S);
  if ( get_is_reading() == TRUE )
    {
      /* force a key_press_return, to scroll to end
       * and recover a prompt
       */
      nsptv_key_press_return(view,FALSE);
    }
}

/**
 * nsp_eval_str_in_textview:
 * @nsp_expr: a string to be evaluated
 *
 * used when an execute selection is performed
 * from an edit buffer.
 *
 **/

void nsp_eval_str_in_textview(const gchar *nsp_expr, int execute_silently)
{
  int key_press= FALSE;
  GtkTextIter start, end;
  int rep,  i;
  NspSMatrix *S;
  View *view= nsptv_get_view("nsp_eval_str_in_textview");
  if ( view == NULL) return;
  S = nsp_smatrix_split_string(nsp_expr,"\n",1);
  if ( execute_silently == FALSE )
    {
      gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
      gtk_text_buffer_insert (view->buffer->buffer, &end, "\n",-1);
    }
  for ( i = 0 ; i < S->mn ; i++ )
    {
      nsptv_append_history(S->S[i], view->view_history, TRUE);
    }
  if ( execute_silently == TRUE )
    {
      rep = nsp_parse_eval_from_smat(S,FALSE,FALSE,FALSE,TRUE);
      if ( rep < 0 )
	{
	  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
	  gtk_text_buffer_insert (view->buffer->buffer, &end, "\n",-1);
	  nsp_error_message_show();
	  key_press=TRUE;
	}
    }
  else
    {
      rep = nsp_parse_eval_from_smat(S,TRUE,TRUE,FALSE,FALSE);
    }

  nsp_smatrix_destroy(S);
  if ( get_is_reading() == TRUE )
    {
      /* force a key_press_return, to scroll to end
       * and recover a promp.
       */
      if ( execute_silently == FALSE || key_press==TRUE)
	nsptv_key_press_return(view,FALSE);
    }
}

void nsp_eval_str_in_terminal(const gchar *str, int execute_silently)
{
  if ( nsp_get_in_text_view() == TRUE )
    {
      nsp_eval_str_in_textview(str,execute_silently);
    }
  else
    {
      NspSMatrix *S = nsp_smatrix_split_string(str,"\n",1);
      nsp_readline_clear_line();
      Sciprintf("\n");
      if ( execute_silently == TRUE )
	nsp_parse_eval_from_smat(S,FALSE,FALSE,FALSE,FALSE);
      else
	nsp_parse_eval_from_smat(S,TRUE,TRUE,FALSE,FALSE);
      nsp_smatrix_destroy(S);
      /* restore a prompt */
      Sciprintf(nsp_prompt());
    }
}

/**
 * nsp_eval_drag_drop_info_text:
 * @nsp_expr: a string to be evaluated
 * @view: the view object.
 *
 * used for evaluation of sequence obtained
 * by drag-drop (drag/drop of a file).
 *
 **/

static void nsp_eval_drag_drop_info_text(const gchar *nsp_expr,View *view, int position, GtkTextIter iter)
{
  gchar *fname;
  GError *error = NULL;
  GtkTextIter start, pos=iter;
  if ( strlen(nsp_expr) == 0 ) return;
  if ( ! gtk_text_iter_can_insert (&iter,gtk_text_view_get_editable(GTK_TEXT_VIEW(view->text_view))))
    {
      gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &pos);
    }
  /* filename from uri */
  fname =g_filename_from_uri(nsp_expr,NULL,&error);
  if ( error != NULL ) 
    {
      /* assuming code to be executed */
      gtk_text_buffer_insert (view->buffer->buffer, &pos, nsp_expr,-1);
      g_error_free (error);
    }
  else
    {
      /* assuming a filename */
      /* detect suffix*/ 
      const char *ext = nsp_get_extension(fname);
      fprintf(stderr,"extension \"%s\"\n",ext);
      if ( ext != NULL) 
	{
	  if ( strcmp(ext,".cos")== 0 || strcmp(ext,".cosf")== 0 )
	    {
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, "scicos('",-1);
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, fname,-1);
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, "');",-1);
	    }
	  else  if (strcmp(ext,".sce")== 0 || strcmp(ext,".sci")== 0 || strcmp(ext,".m")== 0)
	    {
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, "exec('",-1);
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, fname,-1);
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, "');",-1);
	    }
	  else if (strcmp(ext,".bin")== 0)
	    {
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, "load('",-1);
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, fname,-1);
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, "');",-1);
	    }
	  else
	    {
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, "'",-1);
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, fname,-1);
	      gtk_text_buffer_insert (view->buffer->buffer, &pos, "'",-1);
	    }
	}
      else
	{
	  gtk_text_buffer_insert (view->buffer->buffer, &pos, "'",-1);
	  gtk_text_buffer_insert (view->buffer->buffer, &pos, fname,-1);
	  gtk_text_buffer_insert (view->buffer->buffer, &pos, "'",-1);
	}
      g_free(fname);
    }
  if ( get_is_reading() == TRUE )
    {
      /* force a key_press_return, to scroll to end
       * and recover a prompt
       */
      nsptv_key_press_return(view,FALSE);
    }
}

/**
 * readline_textview_internal:
 * @prompt: a string giving the prompt to be used
 *
 * this is the function used to read lines when interaction
 * is performed in a textview.
 *
 * Returns: the next acquired character
 **/

static char *readline_textview_internal(const char *prompt)
{
  View *view;
  static int use_prompt=1;
  if ( nsp_check_events_activated()== FALSE) return readline(prompt);
  view= nsptv_get_view("readline_textview_internal");

  if ( use_prompt == 1)
    {
#ifdef NSP_WITH_MAIN_GTK_THREAD
      nsp_insert_prompt_thread(view,prompt);
#else
      nsp_insert_prompt(view,prompt);
#endif
    }

#ifdef NSP_WITH_MAIN_GTK_THREAD
  nsp_textview_wait_for_text(view);
#else
  nsp_textview_gtk_main();
#endif

  if ( checkqueue_nsp_command() == TRUE)
    {
      char buf[256];
      dequeue_nsp_command(buf,255);
      use_prompt=1;
      /* fprintf(stderr,"Something in queue %s\n",buf); */
      /* we should check here if echo is done or not
       * depending on which command is echoed
       * it's not a good idea to echo event_handlers
       * but event handlers do not use the nsp_command
       * queue any more.
       */
      if ( buf[0] != '\0' && buf[0] != '\n' )
        nsptv_append_history(buf,  view->view_history, TRUE);
      Sciprintf("%s\n",buf);
      return g_strdup(buf);
    }
  else
    {
      gchar *str = g_strdup(view->nsp_expr);
      use_prompt=1;
      view->nsp_expr = NULL;
      return str;
    }
  return NULL;
}


/* when we copy a zone : we change the zone to set it
 * editable. The whole buffer is set to non editable
 * after a <Return>.
 * ZZZZ: est-ce que cela risque pas de finir par etre tres lent
 * quand la taille va croitre ?
 */

static void copy_clipboard_callback(GtkTextView *text_view, gpointer  user_data)
{
  GtkClipboard *clipboard;
  View *view= user_data;
  /* set the selection to be editable
     GtkTextIter start,end;
     gtk_text_buffer_get_selection_bounds (view->buffer->buffer, &start, &end);
     gtk_text_buffer_remove_tag(view->buffer->buffer,view->buffer->not_editable_tag,
			     &start, &end);
  */
  clipboard = gtk_widget_get_clipboard (GTK_WIDGET(text_view),GDK_SELECTION_CLIPBOARD);
  gtk_text_buffer_copy_clipboard(view->buffer->buffer,clipboard);
}

/* Note that even if paste_clipboard_callback is called after
 * the default one we do not get the pasted date because
 * gtk_text_buffer_paste_clipboard is asynchronous.
 */

static void paste_clipboard_callback(GtkTextView *text_view, gpointer  user_data)
{
  View *view= user_data;
  GtkClipboard *clipboard = gtk_widget_get_clipboard (GTK_WIDGET (text_view),
						      GDK_SELECTION_CLIPBOARD);
  gchar *str = gtk_clipboard_wait_for_text(clipboard);
  /* fprintf(stderr,"A clipboard text: %s\n",str); */
  g_signal_stop_emission_by_name (text_view, "paste_clipboard");
  if ( str )
    {
      GtkTextIter iter;
      GtkTextMark *mark;
      mark =  gtk_text_buffer_get_insert (view->buffer->buffer);
      gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter, mark);
      nsp_eval_pasted_from_clipboard(str,view,0,iter);
      g_free(str);
    }
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
  GtkTextIter start,end;
  View *view= (View *) data;
  GtkTextBuffer *buffer = view->buffer->buffer;
  /*
   * reset the non editable zone
   */
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_get_iter_at_mark (buffer, &end, view->buffer->mark);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);
}

static gboolean
gtk_text_view_drag_motion (GtkWidget        *widget,
                           GdkDragContext   *context,
                           gint              x,
                           gint              y,
                           guint             time,
			   gpointer data)
{
  View *view= (View *) data;
  GtkTextBuffer *buffer = view->buffer->buffer;
  GtkTextIter start, end;
  /* during the drag_motion we set all position to editable
   * to enable a drop anywhere
   */
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_get_iter_at_mark (buffer, &end, view->buffer->mark);
  gtk_text_buffer_remove_tag(buffer,   view->buffer->not_editable_tag,   &start, &end);
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
  View *view= (View *) data;
#if GTK_CHECK_VERSION(3,0,0)
#else
  GtkTextView *text_view =GTK_TEXT_VIEW(view->text_view);
#endif 
  GtkTextBuffer *buffer = view->buffer->buffer;
  /*
  GtkTextViewPrivate *priv =  text_view->priv;
  */
  g_signal_stop_emission_by_name (widget, "drag_data_received");
  /*
   * reset the non editable zone
   */
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_get_iter_at_mark (buffer, &end, view->buffer->mark);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);

  /* deals with the drop */

  gtk_text_buffer_begin_user_action (buffer);
#if GTK_CHECK_VERSION(3,0,0)
  gtk_text_buffer_get_iter_at_mark (buffer,
                                    &drop_point,
				    gtk_text_buffer_get_insert (view->buffer->buffer));
#else 
  gtk_text_buffer_get_iter_at_mark (buffer,
                                    &drop_point,
                                    text_view->dnd_mark);
#endif 
  
  switch (info) {
  case GTK_TEXT_BUFFER_TARGET_INFO_BUFFER_CONTENTS:
    {
      GtkTextBuffer *src_buffer = NULL;
      GtkTextIter start, end;
      /* gboolean copy_tags = TRUE; */
      
      if (gtk_selection_data_get_length(selection_data) != sizeof (src_buffer))
        return;

      memcpy (&src_buffer, gtk_selection_data_get_data(selection_data), sizeof (src_buffer));

      if (src_buffer == NULL)
        return;

      g_return_if_fail (GTK_IS_TEXT_BUFFER (src_buffer));

      if (gtk_text_buffer_get_tag_table (src_buffer) !=
          gtk_text_buffer_get_tag_table (buffer))
        {
          /*  try to find a suitable rich text target instead  */
          GdkAtom *atoms;
          gint     n_atoms;
          GList   *list;
          GdkAtom  target = GDK_NONE;
	  /*           copy_tags = FALSE; */

          atoms = gtk_text_buffer_get_deserialize_formats (buffer, &n_atoms);

          for (list =gdk_drag_context_list_targets (context); list; list = g_list_next (list))
            {
              gint i;

              for (i = 0; i < n_atoms; i++)
                if (GUINT_TO_POINTER (atoms[i]) == list->data)
                  {
                    target = atoms[i];
                    break;
                  }
            }

          g_free (atoms);

          if (target != GDK_NONE)
            {
              gtk_drag_get_data (widget, context, target, time);
	      gtk_text_buffer_end_user_action (buffer);
              return;
            }
        }

      if (gtk_text_buffer_get_selection_bounds (src_buffer, &start, &end))
        {
	  GtkTextIter iter;
	  gchar *str;
	  str = gtk_text_iter_get_visible_text (&start, &end);
#if GTK_CHECK_VERSION(3,0,0)
	  gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,
					    gtk_text_buffer_get_insert (view->buffer->buffer));
#else
	  gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,
					    GTK_TEXT_VIEW(view->text_view)->dnd_mark );
#endif 
	  nsp_eval_pasted_from_clipboard(str,view,1,iter);
	  g_free (str);
        }
    }
    break;
  case GTK_TEXT_BUFFER_TARGET_INFO_RICH_TEXT:
    fprintf(stderr, "Selection contains rich text \n");
    /*
      gboolean retval;
      GError *error = NULL;
      retval = gtk_text_buffer_deserialize (buffer, buffer,
      selection_data->target,
      &drop_point,
      (guint8 *) selection_data->data,
      selection_data->length,
      &error);
      if (!retval)
      {
      g_warning ("error pasting: %s\n", error->message);
      g_clear_error (&error);
      }
    */
    break;
 case GTK_TEXT_BUFFER_TARGET_INFO_TEXT: 
   {
     guchar *str;
     gint n_atoms=0;
     GdkAtom *targets;
     if ( gtk_selection_data_get_targets (selection_data,&targets,&n_atoms))
       {
	 if ( gtk_selection_data_targets_include_text (selection_data))
	   {
	     fprintf(stderr, "Selection contains text \n");
	   }
	 if ( gtk_selection_data_targets_include_uri (selection_data))
	   {
	     fprintf(stderr, "Selection contains uri \n");
	   }
	 if ( gtk_selection_data_targets_include_rich_text (selection_data,buffer))
	   {
	     fprintf(stderr, "Selection contains rich-text \n");
	   }
	 if ( gtk_selection_data_targets_include_image (selection_data,TRUE))
	   {
	     fprintf(stderr, "Selection contains image \n");
	   }
       }
     else
       {
	 /* fprintf(stderr, "Selection contains no targets\n"); */
       }
     str = gtk_selection_data_get_text (selection_data);
     if (str)
       {
	 int n = strlen((char *) str);
	 GtkTextIter iter;
#if GTK_CHECK_VERSION(3,0,0)
	 gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,
					   gtk_text_buffer_get_insert (view->buffer->buffer));
#else
	 gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &iter,
					   GTK_TEXT_VIEW(view->text_view)->dnd_mark );
#endif 
	 /* we get here after a drag/drop from help or a drag/drop from desktop
	  * we should find how to better detect each case
	  */
	 /* remove trailing \r\n */
	 if ( str[n-2] == '\r' ) str[n-2]='\0';
	 if ( str[n-1] == '\n' ) str[n-2]='\0';
	 nsp_eval_drag_drop_info_text((gchar *) str,view,1,iter);
	 g_free (str);
       }
     break;
   }
  }
  gtk_drag_finish (context, success,
		   success && gdk_drag_context_get_actions (context) == GDK_ACTION_MOVE,
		   time);
  gtk_text_buffer_end_user_action (buffer);
}

/**
 * configure_event:
 * @widget:
 * @event:
 * @data:
 *
 *
 *
 * Returns:
 **/

static int tv_rows = 24;
static int tv_cols  = 80;

static gint nsptv_configure_callback(GtkWidget *widget, GdkEventConfigure *event, gpointer xdata)
{
  View *view= xdata;
  PangoLayout *layout;
  /* GdkRectangle rect; */
  int width,height;
  if ( view->text_view == NULL ) return FALSE;
  /*
  gtk_text_view_get_visible_rect( GTK_TEXT_VIEW(view->text_view),
				  &rect);
  gtk_text_view_buffer_to_window_coords(GTK_TEXT_VIEW(view->text_view),
					GTK_TEXT_WINDOW_WIDGET,
					rect.width, rect.height,
					&width, &height);
  Sciprintf("windows %d %d\n", width,height);
  */
  layout = gtk_widget_create_pango_layout (view->text_view,"abcdefgh");
  pango_layout_get_pixel_size (layout, &width, &height);
  g_object_unref(layout);
  tv_cols = 8* event->width/width;
  tv_rows = event->height/height;
  /* Sciprintf(" cols= %d rows = %d\n", tv_cols, tv_rows); */
  return FALSE;
}


void  nsp_text_view_screen_size(int *rows,int *cols)
{
  *rows = tv_rows;
  *cols = tv_cols;
}

static View *nsptv_create_view (Buffer *buffer)
{
  int i;
  View *view;
  GtkWidget *sw;
  GtkWidget *vbox;
  GtkWidget *menu;

  view = g_new0 (View, 1);

  view->view_history= malloc (sizeof(view_history));
  if ( view->view_history != NULL)
    {
      HISTORY_STATE *state = history_get_history_state();
      view_history *data =  view->view_history;
      data->history_cur =  data->history_tail =data->history = NULL;
      data->history_size = 0;
      for ( i = 0 ; i < state->length ; i++)
	{
	  nsptv_append_history(state->entries[i]->line,data, FALSE);
	}
    }

  view->buffer = buffer;
  nsptv_buffer_ref (buffer);

  view->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_wmclass(GTK_WINDOW(view->window),"Nsp","Nsp");
  gtk_window_set_title(GTK_WINDOW(view->window),"Nsp");
    
  g_object_set_data (G_OBJECT (view->window), "view", view);

  g_signal_connect (view->window, "delete_event",
		    G_CALLBACK (nsptv_delete_callback), NULL);

  g_signal_connect( view->window, "configure_event",
		    G_CALLBACK(nsptv_configure_callback), view);

  /*
  view->accel_group = gtk_accel_group_new ();
  gtk_window_add_accel_group (GTK_WINDOW (view->window), view->accel_group);
   XXXX accel group to share with menu
  */

#if GTK_CHECK_VERSION (3,0,0)
      vbox =gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
      vbox =gtk_vbox_new (FALSE, 0);
#endif

  gtk_container_add (GTK_CONTAINER (view->window), vbox);
  menu = create_main_menu(view->window);
  gtk_box_pack_start(GTK_BOX(vbox),menu,FALSE,FALSE,0);

  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);

  view->text_view = gtk_text_view_new_with_buffer (buffer->buffer);
  gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (view->text_view),
                               GTK_WRAP_CHAR); /* GTK_WRAP_WORD */

#if GTK_CHECK_VERSION(3,0,0)
  PangoFontDescription* p = pango_font_description_from_string("Monospace");
  gtk_widget_override_font (GTK_WIDGET (view->text_view),p);
  pango_font_description_free (p);
#else
#endif
  gtk_container_set_border_width (GTK_CONTAINER (view->text_view), 5);

  g_signal_connect (view->text_view,
                    "key_press_event",
                    G_CALLBACK (nsptv_key_press_callback),
                    view);

  g_signal_connect(view->text_view,
		   "button_press_event",
		   G_CALLBACK (nsptv_button_press_callback),
		   view);

  g_signal_connect(view->text_view,"drag_data_received",G_CALLBACK (gtk_text_view_drag_data_received),view);

  g_signal_connect(view->text_view,"drag_end",G_CALLBACK (gtk_text_view_drag_end),view);
  g_signal_connect(view->text_view,"drag_motion",G_CALLBACK (gtk_text_view_drag_motion),view);

  g_signal_connect (view->text_view,
                    "copy_clipboard",
                    G_CALLBACK (copy_clipboard_callback),
                    view);

  g_signal_connect (view->text_view,
			  "paste_clipboard",
			  G_CALLBACK (paste_clipboard_callback),
			  view);


  g_signal_connect (view->buffer->buffer,
		    "mark_set",
		    G_CALLBACK (nsptv_cursor_set_callback),
		    view->text_view);

  gtk_box_pack_start (GTK_BOX (vbox), sw, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (sw), view->text_view);

  gtk_widget_set_size_request (GTK_WIDGET (view->window),600,400);
  /*   gtk_window_set_default_size (GTK_WINDOW (view->window), 600, 400); */

  gtk_widget_grab_focus (view->text_view);

  gtk_widget_show_all (view->window);

  nsptv_insert_logo (view);

  /* used to store string to pass to nsp interpreter*/
  view->nsp_expr=NULL;
  /* initialize the queues */
#ifdef NSP_WITH_MAIN_GTK_THREAD
  view->read_queue= g_async_queue_new ();
  view->write_queue= g_async_queue_new ();
#else
  view->read_queue= NULL;
  view->write_queue= NULL;
#endif
  

  return view;
}

/* print to the textview
 *
 */
#ifdef NSP_WITH_MAIN_GTK_THREAD
static int nsp_tv_print(View *view,const char *fmt, ...)
{
  int n;
  va_list ap;
  va_start(ap,fmt);
  n= nsp_print_to_textview(fmt, ap );
  va_end(ap);
  return n;
}
#endif

static int  nsp_print_to_textview(const char *fmt, va_list ap)
{
  View *view= nsptv_get_view("nsp_print_to_textview");
  return nsp_print_to_textview_internal(view,fmt,ap);
}

static int  nsp_print_to_textview_internal(View *view,const char *fmt, va_list ap)
{
  char buf[1025];
  const int ncolor=5;
  static int xtag = FALSE;
  static GtkTextTag *color_tags[5], *tag = NULL;
  GtkTextIter start, end;
  int n;
  char *lbuf = buf ;
  /*fprintf(stderr," nsp_print_to_textview\n");*/
  if ( xtag == FALSE )
    {
      int i;/* {8,{9,{7,{6,{7,{2,8}}}}}} */
      const char *cnames[]={"blue","green","red","purple","lightblue"};
      for ( i = 0 ; i < ncolor ; i++)
	color_tags[i] = gtk_text_buffer_create_tag (view->buffer->buffer, NULL,
						    "foreground", cnames[i] , NULL);
      xtag = TRUE;
    }
  n= vsnprintf(buf,1024 , fmt, ap );
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
#if 1
  while (1)
    {
      if ( *lbuf == '\0') break;
      if ( *lbuf == '\033')
	{
	  /* ignore colors code : "\033[%dm(%d,%d)\033[0m",col,i+1,j+1); */
	  lbuf = lbuf + 3;
	  if (*lbuf != 'm')
	    {
	      char color = *lbuf;
	      switch (color)
		{
		case '4': tag = color_tags[0];break;
		case '2': tag = color_tags[1];break;
		case '1': tag = color_tags[2];break;
		case '5': tag = color_tags[3];break;
		case '6': tag = color_tags[4];break;
		}
	      lbuf++;
	    }
	  else
	    {
	      tag = NULL;
	    }
	  lbuf++;
	}
      else
	{
	  char *loc = lbuf;
	  int count = 0;
	  while ( *loc != '\0' && *loc != '\033') loc++;
	  count = loc -lbuf ;
	  if ( tag != NULL)
	    gtk_text_buffer_insert_with_tags(view->buffer->buffer, &end,lbuf,count,tag,NULL);
	  else
	    gtk_text_buffer_insert (view->buffer->buffer, &end,lbuf,count);
	  lbuf += count;
	}
    }
#else
  gtk_text_buffer_insert (view->buffer->buffer, &end,buf,-1);
#endif

  if ( view->buffer->mark == NULL)
    view->buffer->mark = gtk_text_buffer_create_mark (view->buffer->buffer, NULL, &end, TRUE);
  else
    gtk_text_buffer_move_mark (view->buffer->buffer, view->buffer->mark, &end);
  gtk_text_buffer_get_bounds (view->buffer->buffer, &start, &end);
  gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view),
				view->buffer->mark,
				0, TRUE, 0.0, 1.0);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);
  return n;
}

/* should be used in the threaded version
 *
 */

#ifdef NSP_WITH_MAIN_GTK_THREAD
static gboolean nsp_insert_idle_text(gpointer user_data)
{
  View *view = (View *) user_data;
  char *text = view->user_data;
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_enter();
#endif
  nsp_tv_print(view,"%s",text);
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_leave();
#endif
  nsp_textview_awake_for_end_print_text(view);
  return FALSE;
}
#endif

/* It is safe to call this function from
 * non gtk thread.
 */

#ifdef NSP_WITH_MAIN_GTK_THREAD
static int  nsp_print_to_textview_thread(const char *fmt, va_list ap)
{
  int n;
  if (  g_thread_self() == nsp_gtk_main_thread())
    {
      return nsp_print_to_textview(fmt,ap);
    }
  else
    {
      char tbuf[1025];
      View *view= nsptv_get_view("nsp_print_to_textview_thread");
      view->user_data= (gpointer) tbuf;
      n= vsnprintf(tbuf,1024 , fmt, ap );
      g_idle_add(nsp_insert_idle_text,(gpointer) view);
      nsp_textview_wait_for_end_print_text(view);
    }
  return n;
}
#endif

/* write the prompt prompt in textview */

static void nsp_insert_prompt(View *view,const char *prompt)
{
  GtkTextBuffer *buffer;
  GtkTextIter start, end;
  buffer = view->buffer->buffer;
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  /* scroll to be sure that the prompt will be visible */
  if (  view->buffer->mark != NULL)
    gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (view->text_view),
				  view->buffer->mark,
				  0, TRUE, 0.0, 1.0);
  gtk_text_buffer_insert (buffer, &end,prompt,-1);
  if ( view->buffer->mark == NULL)
    view->buffer->mark = gtk_text_buffer_create_mark (buffer, NULL, &end, TRUE);
  else
    gtk_text_buffer_move_mark (buffer, view->buffer->mark, &end);
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  gtk_text_buffer_apply_tag (view->buffer->buffer,
			     view->buffer->not_editable_tag,
			     &start, &end);
}

/* utility function for nsp_insert_prompt  */

#ifdef NSP_WITH_MAIN_GTK_THREAD
static gboolean nsp_insert_idle_prompt(gpointer user_data)
{
  View *view = (View *) user_data;
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_enter ();
#endif
  nsp_insert_prompt(view,(const char *) view->user_data);
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_leave();
#endif
  nsp_textview_awake_for_end_print_text(view);
  return FALSE;
}
#endif

/* It is safe to call this function from
 * non gtk thread.
 */

#ifdef NSP_WITH_MAIN_GTK_THREAD
static void nsp_insert_prompt_thread(View *view,const char *prompt)
{
  if (  g_thread_self() == nsp_gtk_main_thread())
    {
      nsp_insert_prompt(view,prompt);
    }
  else
    {
      view->user_data= (gpointer) prompt;
      g_idle_add(nsp_insert_idle_prompt,(gpointer) view);
      nsp_textview_wait_for_end_print_text(view);
    }
}
#endif

static View *views[2]={NULL,NULL};

View *nsp_create_main_text_view_(void)
{
  View *view;
  Buffer *buffer = nsptv_create_buffer ();
  view = nsptv_create_view (buffer);
  nsptv_buffer_unref (buffer);
#ifdef NSP_WITH_MAIN_GTK_THREAD
  SetScilabIO(nsp_print_to_textview_thread);
#else
  SetScilabIO(nsp_print_to_textview);
#endif
  nsp_set_getchar_fun(Xorgetchar_textview);
  nsp_set_readline_fun(readline_textview);
  return view;
}

#ifdef NSP_WITH_MAIN_GTK_THREAD
void nsp_create_main_text_view(void)
{
  views[0] = nsp_create_main_text_view_();
  views[1] = nsp_create_main_text_view_();
}

extern GThread *thread1,*thread2,*thmain;

View *nsptv_get_view(const char *str)
{
  static int use_view= 0;
  GThread *self= g_thread_self();
  if ( self == thread1 )
    {
      printf("appel de thread1 %s\n",str);
      use_view= 0;
    }
  else if ( self == thread2 )
    {
      printf("appel de thread2 %s\n",str);
      use_view= 1;
    }
  else
    {
      printf("appel de main %s\n",str);
    }
  return views[use_view];
}
#else
void nsp_create_main_text_view(void)
{
  views[0] = nsp_create_main_text_view_();
}

View *nsptv_get_view(const char *str)
{
  return views[0];
}
#endif

/* insert a graphic file in the textview
 *
 *
 */

int nsp_insert_pixbuf_from_file(char *filename)
{
  GtkTextIter start,end;
  GdkPixbuf*pixbuf;
  View *view= nsptv_get_view("nsp_insert_pixbuf_from_file");
  if ( view != NULL)
    {
      pixbuf =  gdk_pixbuf_new_from_file(filename,NULL);
      gtk_text_buffer_get_end_iter(view->buffer->buffer,&start);
      gtk_text_buffer_get_end_iter(view->buffer->buffer,&end);
      gtk_text_buffer_insert_pixbuf (view->buffer->buffer, &end, pixbuf);
      gtk_text_buffer_insert (view->buffer->buffer, &end, "\n",-1);
      if ( view->buffer->mark != NULL)
	gtk_text_buffer_get_iter_at_mark (view->buffer->buffer, &start,view->buffer->mark);
      else
	gtk_text_buffer_get_end_iter(view->buffer->buffer,&start);

      gtk_text_buffer_apply_tag (view->buffer->buffer,
				 view->buffer->center_tag,
				 &start, &end);
      g_object_unref (pixbuf);
    }
  return 0;
}

static void readline_textview(Tokenizer *T,char *prompt, char *buffer, int *buf_size, int *len_line, int *eof)
{
  static int use_prompt=1;
  static int tty =0, init_flag = TRUE, enter=0;
  char * line=NULL ;
  if(init_flag) {
    /* the next line is useful for cut and paste
     * ehrlich juin 2001
     */
    setvbuf(stdin, NULL, _IONBF, 0);
    /* fd=fileno(stdin); */
    tty = isatty(fileno(stdin));
    init_flag = FALSE;
#if defined(__MINGW32__) || defined(__APPLE__)
    tty=1;
#endif
  }

  set_is_reading(TRUE);

  if( !tty)
    {
      static int first = 0;
      /* if not an interactive terminal use fgets
       * should be changed for gtk events FIXME
       */
      if ( nsp_from_texmacs() == TRUE )
	{
	  if ( first == 0 )
	    {
	      fputs("\002verbatim:",stdout);
	      first++;
	    }
	  /* FIXME: prompt should take care of pause */
	  fputs("\002channel:prompt\005-nsp->\005",stdout);
	  fflush (stdout);
	}
      else
	{
	  fputs("-nsp->",stdout);
	  fflush (stdout);
	}
      *eof = (fgets(buffer, *buf_size, stdin) == NULL);
      *len_line = strlen(buffer);
      /* remove newline character if there */
      if(*len_line >= 2)
	{
	  if ( buffer[*len_line - 2] == '\r' && buffer[*len_line - 1] == '\n' )
	    *len_line -= 2;
	  else if ( buffer[*len_line - 1] == '\n') (*len_line)--;
	}
      else if( *len_line >= 1)
	{
	  if ( buffer[*len_line - 1] == '\n') (*len_line)--;
	}
      if ( nsp_from_texmacs() == TRUE )  fputs("\002verbatim: ",stdout);
      return;
    }
  /* reentrant counter */
  enter++;

  if ( sigsetjmp(my_env,1))
    {
      /* return from longjmp: we get here if there's a menu command
       * to be executed by nsp
       */
      if ( dequeue_nsp_command(buffer,*buf_size) == FAIL)
	{
	  *eof = -1;
	  use_prompt=0;
	  *len_line=0;
	  goto end;
	}
      else
	{
	  *eof = FALSE;
	  use_prompt=0;
	  *len_line = strlen(buffer);
	  goto end;
	}
    }
  else
    {
      signal (SIGINT, controlC_handler_void);
      line = readline_textview_internal((use_prompt) ? prompt : "" );
      use_prompt=1;
      signal (SIGINT, controlC_handler);
    }

  if ( line == NULL)
    {
      *len_line= 1;
      /* strncpy(buffer,"\n",1); */
      buffer[0]='\n';
      *eof = FALSE;
    }
  else
    {
      *len_line= strlen(line);
      strncpy(buffer,line,*buf_size);
      /* Do not free line on reentrant calls  */
      if ( enter == 1 )
	{
	  free(line);
	  line = NULL;
	}
      *eof = FALSE;
    }
  if(get_echo_mode()==0)  set_echo_mode(TRUE);
  set_is_reading(FALSE);
 end:
  enter--;
  return;
}

/**
 * nsp_textview_destroy:
 * @void:
 *
 * this is to be called in  sci_clear_and_exit;
 *
 **/

void nsp_textview_destroy_internal(void)
{
  View *view= nsptv_get_view("nsp_textview_destroy_internal");
  /* printf("inside nsp_textview_destroy_internal\n"); */
  nsptv_clear_history(view);
  nsptv_buffer_unref (view->buffer);
  gtk_widget_destroy (view->window);
  g_free (view);
  view = NULL;
}

#ifdef NSP_WITH_MAIN_GTK_THREAD
static gboolean nsptv_idle_destroy(gpointer user_data)
{
  GAsyncQueue *queue=  user_data;
  int x=0;
  printf("inside nsptv_idle_destroy\n");
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_enter ();
#endif
  nsp_textview_destroy_internal();
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_leave();
#endif
  g_async_queue_push(queue,&x);
  return FALSE;
}
#endif

/* It is safe to call this function from non gtk thread.
 */
void nsp_textview_destroy(void)
{
  View *view= nsptv_get_view("nsp_textview_destroy");
  if ( nsp_get_in_text_view() == FALSE ) return;
  if ( view == NULL ) return;
  /* printf("inside nsp_textview_destroy\n"); */
#ifdef NSP_WITH_MAIN_GTK_THREAD
  if ( g_thread_self() == thmain )
    {
      nsp_textview_destroy_internal();
    }
  else
    {
      GAsyncQueue *queue= g_async_queue_new ();
      g_idle_add(nsptv_idle_destroy,(gpointer) queue);
      g_async_queue_pop(queue);
      g_async_queue_unref(queue);
    }
#else
  nsp_textview_destroy_internal();
#endif
}

/**
 * nsp_textview_gtk_main:
 * @void:
 *
 * Enters a main loop with texview. We quit the
 * main loop when a nsp_comman is found in the command queue.
 *
 **/

#ifndef NSP_WITH_MAIN_GTK_THREAD
static GMainLoop *nsp_textview_loop=NULL;

static gboolean timeout_command (void *v);

static void  nsp_textview_gtk_main(void)
{
  guint timer;
  if (nsp_textview_loop==NULL) {
    nsp_textview_loop = g_main_loop_new (NULL, TRUE);
  }
  timer= g_timeout_add(100,  (GSourceFunc) timeout_command ,nsp_textview_loop);
  /* at that point we are in a  GDK_THREADS_ENTER(); */
  /* GDK_THREADS_LEAVE(); */
  g_main_loop_run (nsp_textview_loop);
  /* GDK_THREADS_ENTER();*/
  g_source_remove(timer);
}

static void  nsp_textview_gtk_main_quit(void)
{
  /* this one is called by a callback */
  g_main_loop_quit(nsp_textview_loop);
}

static gboolean  timeout_command (void *v)
{
  if ( checkqueue_nsp_command() == TRUE)
    {
      /* GDK_THREADS_ENTER(); */
      g_main_loop_quit ((GMainLoop *) v);
      /* GDK_THREADS_LEAVE();*/
    }
  return TRUE;
}
#endif

#ifdef NSP_WITH_MAIN_GTK_THREAD

static gint nsptv_timeout_command_thread(gpointer user_data)
{
  View *view =  user_data;
  if ( checkqueue_nsp_command() == TRUE)
    {
      nsp_textview_awake_for_text(view);
    }
  return TRUE;
}

static void nsp_textview_wait_for_text(View *view)
{
  guint timer;
  /* wait for text entered in textview or command generated by a callback */
  timer= g_timeout_add(100,  (GtkFunction) nsptv_timeout_command_thread,view);
  g_async_queue_pop(view->read_queue);
  g_source_remove(timer);
}

static void nsp_textview_awake_for_text(View *view)
{
  int x=0;
  g_async_queue_push(view->read_queue,&x);
}

static void nsp_textview_wait_for_end_print_text(View *view)
{
  g_async_queue_pop(view->write_queue);
}

static void nsp_textview_awake_for_end_print_text(View *view)
{
  int x=0;
  g_async_queue_push(view->write_queue,&x);
}

#endif
