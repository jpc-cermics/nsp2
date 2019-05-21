/* -*- Mode: C;
 *  test-widget.c
 *
 *  Copyright (C) 2001 -  Mikael Hermansson<tyan@linux.se>
 *  Copyright (C) 2003 - Gustavo Giraldez <gustavo.giraldez@gmx.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 *  This file is a modified version of the test-widget.c file of gtksourceview
 *  with added functionality for nsp. Some of them inherited from testext.c demo file
 *  from the gtk distribution (see gtktextview.c).
 *  Copyright (C) 2010-2019 Jean-Philippe Chancelier Enpc/Cermics
 *
 */

#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <nsp/nsp.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <gio/gio.h>
#include <glib/gstdio.h>
#if GTK_CHECK_VERSION (3,0,0)
#include <gtksourceview/gtksource.h>
#else
#include <gtksourceview/gtksourceview.h>
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcebuffer.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <gtksourceview/gtksourcestyleschememanager.h>
#include <gtksourceview/gtksourceprintcompositor.h>
#endif

#if GTK_CHECK_VERSION (3,0,0)
#define gtk_source_iter_forward_search gtk_text_iter_forward_search
#define GtkSourceSearchFlags GtkTextSearchFlags
#define GTK_SOURCE_SEARCH_CASE_INSENSITIVE GTK_TEXT_SEARCH_TEXT_ONLY

#else
#include <gtksourceview/gtksourceiter.h>
#endif
#ifdef HAVE_GTKSOURCEVIEW_GUTTER
#include <gtksourceview/gtksourcegutter.h>
#endif

#include <nsp/object.h>
#include <nsp/smatrix.h>
#include <nsp/plist.h>
#include <nsp/parse.h>
#include <nsp/nsptcl.h>
#include <nsp/system.h> /* FSIZE */

extern void nsp_eval_str_in_terminal(const gchar *str, int execute_silently);

/* Global list of open windows */
static GList *windows = NULL;
static GtkSourceStyleScheme *style_scheme = NULL;
static int execute_silently= TRUE;

/* Private data structures */
#define MARK_TYPE_2      "two"

static void view_set_title (GtkTextView  *view, int read_only);
static char *buffer_pretty_name (GtkSourceBuffer *buffer);
static gboolean save_buffer (GtkSourceBuffer *buffer,const char *filename );
static GtkSourceLanguage *nsp_gtksource_language (void);

static void open_file_cb (GtkAction *action, gpointer  user_data);
static void close_cb (GtkAction *action, gpointer user_data);
static void execute_selection_cb (GtkAction *action, gpointer user_data);
static void execute_cb (GtkAction *action, gpointer user_data);
static void execute_tag_error(GtkSourceBuffer *buffer,int line);
static void save_as_cb   (GtkAction *action, gpointer user_data);
static void save_cb   (GtkAction *action, gpointer user_data);
static void print_file_cb(GtkAction *action, gpointer user_data);
static void find_cb(GtkAction *action, gpointer user_data);
static void replace_cb(GtkAction *action, gpointer user_data);
static void numbers_toggled_cb (GtkAction *action, gpointer user_data);
static void marks_toggled_cb (GtkAction *action, gpointer user_data);
static void margin_toggled_cb (GtkAction *action, gpointer user_data);
static void hl_bracket_toggled_cb(GtkAction *action, gpointer user_data);
static void hl_line_toggled_cb (GtkAction *action, gpointer user_data);
static void draw_spaces_toggled_cb(GtkAction *action, gpointer user_data);
static void wrap_lines_toggled_cb (GtkAction *action, gpointer user_data);
static void auto_indent_toggled_cb (GtkAction *action, gpointer user_data);
static void insert_spaces_toggled_cb(GtkAction *action, gpointer user_data);
static void silent_execute_toggled_cb (GtkAction *action, gpointer user_data);
static void tabs_toggled_cb (GtkAction *action,GtkAction *current, gpointer user_data);
static void indent_toggled_cb(GtkAction *action,GtkAction *current, gpointer user_data);

#ifdef HAVE_GTKSOURCEVIEW_CONTEXT_CLASS
static void forward_string_cb (GtkAction *action, gpointer user_data);
static void backward_string_cb (GtkAction *action, gpointer user_data);
#endif

static GtkWidget *
create_view_window (GtkSourceBuffer *buffer, GtkSourceView *from, const char *comment, int close);

/* Actions & UI definition ---------------------------------------------------- */

static GtkActionEntry buffer_action_entries[] = {
  { "Open", GTK_STOCK_OPEN, "_Open", "<control>O",
    "Open a file", G_CALLBACK (open_file_cb) },
  { "Save", GTK_STOCK_SAVE, "Save", NULL,NULL, G_CALLBACK(save_cb)},
  { "SaveAs", GTK_STOCK_SAVE_AS, "Save _As...", NULL,NULL, G_CALLBACK(save_as_cb)},
  { "Close", GTK_STOCK_CLOSE, "_Close", NULL, "Close edit window", G_CALLBACK (close_cb) },
  {"Execute",NULL, "_Execute...", "<control>l" ,NULL, G_CALLBACK (execute_cb)},
  {"ExecuteSelection",NULL, "Execute Selection","<control>y" ,NULL, G_CALLBACK (execute_selection_cb)},
};

static GtkActionEntry view_action_entries[] = {
  { "FileMenu", NULL, "_File", NULL, NULL, NULL },
  { "Print", GTK_STOCK_PRINT, "_Print", NULL /*"<control>P"*/ ,
    "Print the current file", G_CALLBACK (print_file_cb) },
  { "ViewMenu", NULL, "_View", NULL, NULL, NULL },
  { "ExecMenu", NULL, "Tools", NULL, NULL, NULL },
  { "TabWidth", NULL, "_Tab Width", NULL, NULL, NULL },
  { "IndentWidth", NULL, "I_ndent Width", NULL, NULL, NULL },
  { "SmartHomeEnd", NULL, "_Smart Home/End", NULL, NULL, NULL },
  { "Find", GTK_STOCK_FIND, "_Find", NULL /* "<control>F */,
    "Find", G_CALLBACK (find_cb) },
  { "Replace", GTK_STOCK_FIND_AND_REPLACE, "Search and _Replace", "<control>R",
    "Search and Replace", G_CALLBACK (replace_cb) },
#ifdef HAVE_GTKSOURCEVIEW_CONTEXT_CLASS
  { "ForwardString", NULL, "_Forward to string toggle", "<control>S",
    "Forward to the start or end of the next string", G_CALLBACK (forward_string_cb) },
  { "BackwardString", NULL, "_Backward to string toggle", "<control><shift>S",
    "Backward to the start or end of the next string", G_CALLBACK (backward_string_cb) }
#endif
};

static GtkToggleActionEntry toggle_entries[] = {
  { "HlBracket", NULL, "Highlight Matching _Bracket", NULL,
    "Toggle highlighting of matching bracket",
    G_CALLBACK (hl_bracket_toggled_cb), FALSE },
  { "ShowNumbers", NULL, "Show _Line Numbers", NULL,
    "Toggle visibility of line numbers in the left margin",
    G_CALLBACK (numbers_toggled_cb), FALSE },
  { "ShowMarks", NULL, "Show Line _Marks", NULL,
    "Toggle visibility of marks in the left margin",
    G_CALLBACK (marks_toggled_cb), FALSE },
  { "ShowMargin", NULL, "Show Right M_argin", NULL,
    "Toggle visibility of right margin indicator",
    G_CALLBACK (margin_toggled_cb), FALSE },
  { "HlLine", NULL, "_Highlight Current Line", NULL,
    "Toggle highlighting of current line",
    G_CALLBACK (hl_line_toggled_cb), FALSE },
  { "DrawSpaces", NULL, "_Draw Spaces", NULL,
    "Draw Spaces",
    G_CALLBACK (draw_spaces_toggled_cb), FALSE },
  { "WrapLines", NULL, "_Wrap Lines", NULL,
    "Toggle line wrapping",
    G_CALLBACK (wrap_lines_toggled_cb), FALSE },
  { "AutoIndent", NULL, "Enable _Auto Indent", NULL,
    "Toggle automatic auto indentation of text",
    G_CALLBACK (auto_indent_toggled_cb), FALSE },
  { "InsertSpaces", NULL, "Insert _Spaces Instead of Tabs", NULL,
    "Whether to insert space characters when inserting tabulations",
    G_CALLBACK (insert_spaces_toggled_cb), FALSE },
  { "SilentExecute", NULL, "Execute code silently", NULL,
    "Toggle silent execution code option",
    G_CALLBACK (silent_execute_toggled_cb), TRUE }
};

static GtkRadioActionEntry tabs_radio_entries[] = {
  { "TabWidth2", NULL, "2", NULL, "Set tabulation width to 2 spaces", 2 },
  { "TabWidth4", NULL, "4", NULL, "Set tabulation width to 4 spaces", 4 },
  { "TabWidth6", NULL, "6", NULL, "Set tabulation width to 6 spaces", 6 },
  { "TabWidth8", NULL, "8", NULL, "Set tabulation width to 8 spaces", 8 },
  { "TabWidth10", NULL, "10", NULL, "Set tabulation width to 10 spaces", 10 },
  { "TabWidth12", NULL, "12", NULL, "Set tabulation width to 12 spaces", 12 }
};

static GtkRadioActionEntry indent_radio_entries[] = {
  { "IndentWidthUnset", NULL, "Use Tab Width", NULL, "Set indent width same as tab width", -1 },
  { "IndentWidth2", NULL, "2", NULL, "Set indent width to 2 spaces", 2 },
  { "IndentWidth4", NULL, "4", NULL, "Set indent width to 4 spaces", 4 },
  { "IndentWidth6", NULL, "6", NULL, "Set indent width to 6 spaces", 6 },
  { "IndentWidth8", NULL, "8", NULL, "Set indent width to 8 spaces", 8 },
  { "IndentWidth10", NULL, "10", NULL, "Set indent width to 10 spaces", 10 },
  { "IndentWidth12", NULL, "12", NULL, "Set indent width to 12 spaces", 12 }
};

static GtkRadioActionEntry smart_home_end_entries[] = {
  { "SmartHomeEndDisabled", NULL, "Disabled", NULL,
    "Smart Home/End disabled", GTK_SOURCE_SMART_HOME_END_DISABLED },
  { "SmartHomeEndBefore", NULL, "Before", NULL,
    "Smart Home/End before", GTK_SOURCE_SMART_HOME_END_BEFORE },
  { "SmartHomeEndAfter", NULL, "After", NULL,
    "Smart Home/End after", GTK_SOURCE_SMART_HOME_END_AFTER },
  { "SmartHomeEndAlways", NULL, "Always", NULL,
    "Smart Home/End always", GTK_SOURCE_SMART_HOME_END_ALWAYS }
};

static const gchar *view_ui_description =
  "<ui>"
  "  <menubar name=\"MainMenu\">"
  "    <menu action=\"FileMenu\">"
  "      <!--"
  "      <menuitem action=\"PrintPreview\"/>"
  "      -->"
  "    </menu>"
  "    <menu action=\"ViewMenu\">"
  "      <menuitem action=\"HlBracket\"/>"
  "      <menuitem action=\"ShowNumbers\"/>"
  "      <menuitem action=\"ShowMarks\"/>"
  "      <menuitem action=\"ShowMargin\"/>"
  "      <menuitem action=\"HlLine\"/>"
  "      <menuitem action=\"DrawSpaces\"/>"
  "      <menuitem action=\"WrapLines\"/>"
  "      <separator/>"
  "      <menuitem action=\"AutoIndent\"/>"
  "      <menuitem action=\"InsertSpaces\"/>"
  "      <separator/>"
  "      <menu action=\"TabWidth\">"
  "        <menuitem action=\"TabWidth2\"/>"
  "        <menuitem action=\"TabWidth4\"/>"
  "        <menuitem action=\"TabWidth6\"/>"
  "        <menuitem action=\"TabWidth8\"/>"
  "        <menuitem action=\"TabWidth10\"/>"
  "        <menuitem action=\"TabWidth12\"/>"
  "      </menu>"
  "      <menu action=\"IndentWidth\">"
  "        <menuitem action=\"IndentWidthUnset\"/>"
  "        <menuitem action=\"IndentWidth2\"/>"
  "        <menuitem action=\"IndentWidth4\"/>"
  "        <menuitem action=\"IndentWidth6\"/>"
  "        <menuitem action=\"IndentWidth8\"/>"
  "        <menuitem action=\"IndentWidth10\"/>"
  "        <menuitem action=\"IndentWidth12\"/>"
  "      </menu>"
  "      <separator/>"
  "      <menu action=\"SmartHomeEnd\">"
  "        <menuitem action=\"SmartHomeEndDisabled\"/>"
  "        <menuitem action=\"SmartHomeEndBefore\"/>"
  "        <menuitem action=\"SmartHomeEndAfter\"/>"
  "        <menuitem action=\"SmartHomeEndAlways\"/>"
  "      </menu>"
#ifdef HAVE_GTKSOURCEVIEW_CONTEXT_CLASS
  "      <separator/>"
  "      <menuitem action=\"ForwardString\"/>"
  "      <menuitem action=\"BackwardString\"/>"
#endif
  "    </menu>"
  "  </menubar>"
  "</ui>";

static const gchar *buffer_file_ui_description =
  "<ui>"
  "  <menubar name=\"MainMenu\">"
  "    <menu action=\"FileMenu\">"
  "      <menuitem action=\"Open\"/>"
  "      <menuitem action=\"Save\"/>"
  "      <menuitem action=\"SaveAs\"/>"
  "      <menuitem action=\"Print\"/>"
  "      <menuitem action=\"Find\"/>"
  "      <menuitem action=\"Replace\"/>"
  "      <separator/>"
  "      <menuitem action=\"Close\"/>"
  "    </menu>"
  "    <menu action=\"ViewMenu\">"
  "    </menu>"
  "    <menu action=\"ExecMenu\">"
  "      <menuitem action=\"Execute\" />\n"
  "      <menuitem action=\"ExecuteSelection\" />\n"
  "      <menuitem action=\"SilentExecute\" />\n"
  "    </menu>"
  "  </menubar>"
  "</ui>";

static const gchar *buffer_smatrix_ui_description =
  "<ui>"
  "  <menubar name=\"MainMenu\">"
  "    <menu action=\"FileMenu\">"
  "      <menuitem action=\"Print\"/>"
  "      <menuitem action=\"Find\"/>"
  "      <menuitem action=\"Replace\"/>"
  "      <separator/>"
  "      <menuitem action=\"Close\"/>"
  "    </menu>"
  "    <menu action=\"ViewMenu\">"
  "    </menu>"
  "  </menubar>"
  "</ui>";

/* File loading code ----------------------------------------------------------------- */

static void error_dialog (GtkWindow *parent, const gchar *msg, ...)
{
  va_list ap;
  gchar *tmp;
  GtkWidget *dialog;

  va_start (ap, msg);
  tmp = g_strdup_vprintf (msg, ap);
  va_end (ap);

  dialog = gtk_message_dialog_new (parent,
				   GTK_DIALOG_DESTROY_WITH_PARENT,
				   GTK_MESSAGE_ERROR,
				   GTK_BUTTONS_OK,
				   "%s", tmp);
  g_free (tmp);

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

static gboolean
gtk_source_buffer_load_file (GtkSourceBuffer *source_buffer,
			     const gchar     *filename,
			     GError         **error)
{
  GtkTextIter iter;
  gchar *buffer,*buffer_utf8 ;
  GError *error_here = NULL;
  g_return_val_if_fail (GTK_IS_TEXT_BUFFER (source_buffer), FALSE);
  g_return_val_if_fail (filename != NULL, FALSE);

  if (!g_file_get_contents (filename, &buffer, NULL, &error_here))
    {
      error_dialog (NULL, "%s\nFile %s", error_here->message, filename);
      g_propagate_error (error, error_here);
      return FALSE;
    }
  if ((buffer_utf8= nsp_string_to_utf8(buffer)) == NULL)
    {
      Sciprintf("File %s is not utf8 \n",filename);
      g_free(buffer);
      return FALSE;
    }

  gtk_source_buffer_begin_not_undoable_action (source_buffer);
  gtk_text_buffer_set_text (GTK_TEXT_BUFFER (source_buffer), buffer_utf8, -1);
  gtk_source_buffer_end_not_undoable_action (source_buffer);
  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (source_buffer), FALSE);

  /* move cursor to the beginning */
  gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER (source_buffer), &iter);
  gtk_text_buffer_place_cursor (GTK_TEXT_BUFFER (source_buffer), &iter);

  /*
  {
    GtkTextIter start, end;
    char *text;
    gtk_text_buffer_get_bounds (GTK_TEXT_BUFFER (source_buffer), &start, &end);
    text = gtk_text_buffer_get_text (GTK_TEXT_BUFFER (source_buffer), &start, &end, TRUE);
    g_assert (!strcmp (text, buffer));
    g_free (text);
  }
  */
  g_free (buffer);
  if ( buffer_utf8 != buffer) g_free (buffer_utf8);
  return TRUE;
}

static void
remove_all_marks (GtkSourceBuffer *buffer)
{
  GtkTextIter s, e;

  gtk_text_buffer_get_bounds (GTK_TEXT_BUFFER (buffer), &s, &e);

  gtk_source_buffer_remove_source_marks (buffer, &s, &e, NULL);
}

static GtkSourceLanguage *
get_language_for_file (GtkTextBuffer *buffer, const gchar *filename)
{
  GtkSourceLanguageManager *manager;
  GtkSourceLanguage *language;
  GtkTextIter start, end;
  gchar *text;
  gchar *content_type;
  gboolean result_uncertain;

  gtk_text_buffer_get_start_iter (buffer, &start);
  if (gtk_text_buffer_get_char_count (buffer) < 1024)
    gtk_text_buffer_get_end_iter (buffer, &end);
  else
    gtk_text_buffer_get_iter_at_offset (buffer, &end, 1024);
  text = gtk_text_buffer_get_slice (buffer, &start, &end, TRUE);

  content_type = g_content_type_guess (filename,
				       (guchar*) text,
				       strlen (text),
				       &result_uncertain);
  if (result_uncertain)
    {
      g_free (content_type);
      content_type = NULL;
    }

  manager = gtk_source_language_manager_get_default ();
  language = gtk_source_language_manager_guess_language (manager,
							 filename,
							 content_type);
  /*
  g_message ("Detected '%s' mime type for file %s, chose language %s",
	     content_type ? content_type : "(null)",
	     filename,
	     language ? gtk_source_language_get_id (language) : "(none)");
  */
  g_free (content_type);
  g_free (text);
  return language;
}

static GtkSourceLanguage *
get_language_by_id (const gchar *id)
{
  GtkSourceLanguageManager *manager;
  manager = gtk_source_language_manager_get_default ();
  return gtk_source_language_manager_get_language (manager, id);
}

static GtkSourceLanguage *
get_language (GtkTextBuffer *buffer, const gchar *filename)
{
  GtkSourceLanguage *language = NULL;
  GtkTextIter start, end;
  gchar *text;
  gchar *lang_string;
  const char *ext;
  gtk_text_buffer_get_start_iter (buffer, &start);
  end = start;
  gtk_text_iter_forward_line (&end);

#define LANG_STRING "gtk-source-lang:"
  text = gtk_text_iter_get_slice (&start, &end);
  lang_string = strstr (text, LANG_STRING);
  if (lang_string != NULL)
    {
      gchar **tokens;

      lang_string += strlen (LANG_STRING);
      g_strchug (lang_string);

      tokens = g_strsplit_set (lang_string, " \t\n", 2);

      if (tokens != NULL && tokens[0] != NULL)
	language = get_language_by_id (tokens[0]);

      g_strfreev (tokens);
    }

  if (!language && filename != NULL )
    language = get_language_for_file (buffer, filename);

  ext= nsp_get_extension(filename);
  if (!language || strcmp(ext,".sci")== 0 || strcmp(ext,".sce")==0 )
    language = nsp_gtksource_language ();

  g_free (text);
  return language;
}

static gboolean
open_file (GtkSourceBuffer *buffer, const gchar *filename)
{
  GtkSourceLanguage *language = NULL;
  gchar *freeme = NULL;
  gboolean success = FALSE;

  if ( filename == NULL)
    {
       remove_all_marks (buffer);
       language = nsp_gtksource_language ();
       gtk_source_buffer_set_language (buffer, language);
       return TRUE;
    }

  if (!g_path_is_absolute (filename))
    {
      gchar *curdir = g_get_current_dir ();
      freeme = g_build_filename (curdir, filename, NULL);
      filename = freeme;
      g_free (curdir);
    }

  remove_all_marks (buffer);

  success = gtk_source_buffer_load_file (buffer, filename, NULL);

  /* this will set language to nsp if filename == NULL */

  language = get_language (GTK_TEXT_BUFFER (buffer), filename);
  if (language == NULL)
    g_print ("No language found for file `%s'\n", filename);
  gtk_source_buffer_set_language (buffer, language);

  if ( success )
    {
      GtkWidget *view;
      g_object_set_data_full (G_OBJECT (buffer),
			      "filename", g_strdup (filename),
			      (GDestroyNotify) g_free);
      view =  g_object_get_data (G_OBJECT (buffer), "buffer_view");
      if ( view)
	view_set_title (GTK_TEXT_VIEW(view), FALSE);
    }
  g_free (freeme);
  return success;
}

/* S is assumed to be a utf8 matrix
 */

static gboolean
gtk_source_buffer_load_smatrix (GtkSourceBuffer *source_buffer,
				NspSMatrix *S)
{
  int i;
  GtkTextIter iter;

  g_return_val_if_fail (GTK_IS_TEXT_BUFFER (source_buffer), FALSE);
  g_return_val_if_fail (S != NULL, FALSE);

  gtk_source_buffer_begin_not_undoable_action (source_buffer);
  gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER (source_buffer), &iter);
  for ( i = 0 ; i < S->mn ; i++)
    {
      gtk_text_buffer_insert(GTK_TEXT_BUFFER (source_buffer),&iter, S->S[i],-1);
      gtk_text_buffer_insert(GTK_TEXT_BUFFER (source_buffer),&iter, "\n",-1);
    }
  gtk_source_buffer_end_not_undoable_action (source_buffer);
  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (source_buffer), FALSE);
  /* move cursor to the beginning */
  gtk_text_buffer_get_start_iter (GTK_TEXT_BUFFER (source_buffer), &iter);
  gtk_text_buffer_place_cursor (GTK_TEXT_BUFFER (source_buffer), &iter);
  return TRUE;
}

static gboolean
open_smatrix (GtkSourceBuffer *buffer,const char *title, NspSMatrix *S)
{
  GtkWidget *window;
  GtkSourceLanguage *language = NULL;
  gboolean success = FALSE;
  remove_all_marks (buffer);
  language = nsp_gtksource_language ();
  gtk_source_buffer_set_language (buffer, language);
  success = gtk_source_buffer_load_smatrix(buffer,S);
  /* to keep track that this is a smatrix buffer */
  g_object_set_data(G_OBJECT (buffer),"smatrix",GINT_TO_POINTER(1));
  if ( success == FALSE ) return success;
  window =  g_object_get_data (G_OBJECT (buffer), "buffer_window");
  if ( window)  gtk_window_set_title (GTK_WINDOW (window), title);
  return success;
}


/* get buffer content in a SMatrix  */

static NspSMatrix *save_buffer_in_smatrix(GtkSourceBuffer *buffer)
{
  NspSMatrix *S=NULL;
  GtkTextIter start, end;
  gchar *chars,*chars1;
  gtk_text_buffer_get_iter_at_offset (GTK_TEXT_BUFFER (buffer), &start, 0);
  gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER (buffer), &end);
  chars1= chars = gtk_text_buffer_get_slice (GTK_TEXT_BUFFER (buffer), &start, &end, FALSE);
  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (buffer), FALSE);
  if ((S = nsp_smatrix_create(NVOID,0,0,"",0))== NULLSMAT)
    return S;
  while ( *chars != '\0')
    {
      char *tag = strstr(chars,"\n");
      if(tag != NULL) *tag ='\0';
      if ( nsp_row_smatrix_append_string(S,chars) == FAIL)
	goto err;
      if ( tag == NULL) break;
      chars = tag+1;
    }
  g_free (chars1);
  /* return a non-empty object if buffer is empty */
  if (S->mn==0) {
    if ( nsp_row_smatrix_append_string(S,"") == FAIL)
      goto err;
  }
  return S;
 err:
  if ( S != NULL) nsp_smatrix_destroy(S);
  return NULL;
}


/* View action callbacks -------------------------------------------------------- */


static void execute_cb (GtkAction *action, gpointer user_data)
{
  char *filename;
  GtkSourceBuffer *buffer = user_data;
  GtkTextIter start,end;
  int display=FALSE,echo =FALSE,errcatch=TRUE,rep,pausecatch=TRUE,mtlb=FALSE;
  g_return_if_fail (GTK_IS_TEXT_BUFFER (user_data));

  /* save first */
  /* push_active_window (GTK_WINDOW (view->window)); */

  remove_all_marks (buffer);
  gtk_text_buffer_get_bounds (GTK_TEXT_BUFFER (buffer), &start, &end);
  filename = g_object_get_data (G_OBJECT (buffer), "filename");

  if ( filename )
    {
      if (  save_buffer (buffer,filename)== FALSE) return;
      /* execute the file contents */
      if ( execute_silently == FALSE )
	{
	  display = echo = TRUE;
	}
      rep =nsp_parse_eval_file(filename,display,echo,errcatch,
			       (pausecatch == TRUE) ? FALSE: TRUE,mtlb);
      if ( rep < 0 )
	{
	  char fname[FSIZE+1];
	  /* get the line number of the Error */
	  int line=-1,i;
	  NspSMatrix *error_msg = nsp_lasterror_get();
	  for ( i=0; i < error_msg->mn ; i++)
	    {
	      Sciprintf("%s",error_msg->S[i]);
	      if ( sscanf(error_msg->S[i],"\tline %d of file %s",&line,fname)==2)
		{
		  if ( strcmp(fname,filename) == 0)
		    {
		      execute_tag_error(buffer,line);
		      break;
		    }
		}
	    }
	  /*
	  if ( sscanf(error_msg->S[error_msg->mn-1],"Error: at line %d of file",&line)==1)
	    {
	      execute_tag_error(view,line);
	    }
	  */
	  nsp_lasterror_clear();
	}
    }
  else
    {
      /* execute the file contents through matrix */
      const char *buf_str= gtk_text_iter_get_text (&start, &end);
      nsp_eval_str_in_terminal(buf_str, execute_silently);
    }
  /*
     pop_active_window ();
  */
}

static void execute_selection_cb (GtkAction *action, gpointer user_data)
{
  GtkSourceBuffer *buffer = user_data;
  GtkTextIter start,end;
  const gchar *str;

  g_return_if_fail (GTK_IS_TEXT_BUFFER (user_data));
  /* save first */
  /* push_active_window (GTK_WINDOW (view->window)); */
  remove_all_marks (buffer);

  if (! gtk_text_buffer_get_selection_bounds (GTK_TEXT_BUFFER (buffer), &start, &end))
    return;
  str = gtk_text_iter_get_visible_text (&start, &end);
  nsp_eval_str_in_terminal(str,execute_silently);
  /* pop_active_window (); */
}

/* put line in red
 *
 */

static void execute_tag_error(GtkSourceBuffer *buffer,int line)
{
  GtkWidget *view;
  GSList *mark_list;
  GtkTextIter istart;
  gtk_text_buffer_get_iter_at_line(GTK_TEXT_BUFFER(buffer),&istart,line-1);
  view =  g_object_get_data (G_OBJECT (buffer), "buffer_view");
  if ( view )
    {
      gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(view),&istart,0.0,FALSE,0,0);
    }

  /* get the marks already in the line */
  mark_list = gtk_source_buffer_get_source_marks_at_line(buffer,line-1, MARK_TYPE_2);

  if (mark_list == NULL)
    {
      /* no mark found: create one */
      gtk_source_buffer_create_source_mark (buffer,
					    NULL,
					    MARK_TYPE_2,
					    &istart);
    }
  g_slist_free (mark_list);
}

static void
numbers_toggled_cb (GtkAction *action, gpointer user_data)
{
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_show_line_numbers (
					 GTK_SOURCE_VIEW (user_data),
					 gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)));
}

static void
marks_toggled_cb (GtkAction *action, gpointer user_data)
{
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_show_line_marks (
				       GTK_SOURCE_VIEW (user_data),
				       gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)));
}

static void
margin_toggled_cb (GtkAction *action, gpointer user_data)
{
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_show_right_margin (
					 GTK_SOURCE_VIEW (user_data),
					 gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)));
}

static void
hl_bracket_toggled_cb (GtkAction *action, gpointer user_data)
{
  GtkTextBuffer *buffer;
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  buffer = gtk_text_view_get_buffer (user_data);
  gtk_source_buffer_set_highlight_matching_brackets (
						     GTK_SOURCE_BUFFER (buffer),
						     gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)));
}

static void
hl_line_toggled_cb (GtkAction *action, gpointer user_data)
{
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_highlight_current_line (
					      GTK_SOURCE_VIEW (user_data),
					      gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)));
}

static void
draw_spaces_toggled_cb (GtkAction *action, gpointer user_data)
{
  gboolean draw_spaces;

  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  draw_spaces = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action));

  if (draw_spaces)
    gtk_source_view_set_draw_spaces (GTK_SOURCE_VIEW (user_data),
				     GTK_SOURCE_DRAW_SPACES_ALL);
  else
    gtk_source_view_set_draw_spaces (GTK_SOURCE_VIEW (user_data),
				     0);
}

static void
wrap_lines_toggled_cb (GtkAction *action, gpointer user_data)
{
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_text_view_set_wrap_mode (user_data,
			       gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)) ?
			       GTK_WRAP_WORD : GTK_WRAP_NONE);
}

static void
auto_indent_toggled_cb (GtkAction *action,
			gpointer   user_data)
{
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_auto_indent (
				   GTK_SOURCE_VIEW (user_data),
				   gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)));
}

static void
insert_spaces_toggled_cb (GtkAction *action,
			  gpointer   user_data)
{
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_insert_spaces_instead_of_tabs (
						     GTK_SOURCE_VIEW (user_data),
						     gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action)));
}

static void
silent_execute_toggled_cb (GtkAction *action,
			   gpointer user_data)
{
  g_return_if_fail (GTK_IS_TOGGLE_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  execute_silently = gtk_toggle_action_get_active (GTK_TOGGLE_ACTION (action));

}

static void
tabs_toggled_cb (GtkAction *action,
		 GtkAction *current,
		 gpointer user_data)
{
  g_return_if_fail (GTK_IS_RADIO_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_tab_width (
				 GTK_SOURCE_VIEW (user_data),
				 gtk_radio_action_get_current_value (GTK_RADIO_ACTION (action)));
}

static void
indent_toggled_cb (GtkAction *action,
		   GtkAction *current,
		   gpointer user_data)
{
  g_return_if_fail (GTK_IS_RADIO_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_indent_width (
				    GTK_SOURCE_VIEW (user_data),
				    gtk_radio_action_get_current_value (GTK_RADIO_ACTION (action)));
}

static void
smart_home_end_toggled_cb (GtkAction *action,
			   GtkAction *current,
			   gpointer user_data)
{
  g_return_if_fail (GTK_IS_RADIO_ACTION (action) && GTK_IS_TEXT_VIEW (user_data));
  gtk_source_view_set_smart_home_end (
				      GTK_SOURCE_VIEW (user_data),
				      gtk_radio_action_get_current_value (GTK_RADIO_ACTION (action)));
}

#ifdef HAVE_GTKSOURCEVIEW_CONTEXT_CLASS
/* 2.10.0 ? version */

static void
forward_string_cb (GtkAction *action,
		   gpointer   user_data)
{
  GtkSourceBuffer *buffer;
  GtkSourceView *view;
  GtkTextIter iter;
  GtkTextMark *insert;

  g_return_if_fail (GTK_IS_TEXT_VIEW (user_data));

  view = GTK_SOURCE_VIEW (user_data);
  buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));
  insert = gtk_text_buffer_get_insert (GTK_TEXT_BUFFER (buffer));

  gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer),
				    &iter,
				    insert);

  if (gtk_source_buffer_iter_forward_to_context_class_toggle (buffer,
							      &iter,
							      "string"))
    {
      gtk_text_buffer_place_cursor (GTK_TEXT_BUFFER (buffer), &iter);
      gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (view), insert);
    }
}

static void
backward_string_cb (GtkAction *action,
		    gpointer   user_data)
{
  GtkSourceBuffer *buffer;
  GtkSourceView *view;
  GtkTextIter iter;
  GtkTextMark *insert;

  g_return_if_fail (GTK_IS_TEXT_VIEW (user_data));

  view = GTK_SOURCE_VIEW (user_data);
  buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));
  insert = gtk_text_buffer_get_insert (GTK_TEXT_BUFFER (buffer));

  gtk_text_buffer_get_iter_at_mark (GTK_TEXT_BUFFER (buffer),
				    &iter,
				    insert);

  if (gtk_source_buffer_iter_backward_to_context_class_toggle (buffer,
							       &iter,
							       "string"))
    {
      gtk_text_buffer_place_cursor (GTK_TEXT_BUFFER (buffer), &iter);
      gtk_text_view_scroll_mark_onscreen (GTK_TEXT_VIEW (view), insert);
    }
}
#endif

/* Buffer action callbacks ------------------------------------------------------------ */

static struct {
  char *what;
  char *replacement;
  GtkSourceSearchFlags flags;
} search_data = {
  NULL,
  NULL,

  GTK_SOURCE_SEARCH_CASE_INSENSITIVE
};

static gboolean
search_dialog (GtkWidget            *widget,
	       gboolean              replace,
	       char                **what_p,
	       char                **replacement_p,
	       GtkSourceSearchFlags *flags_p)
{
  GtkWidget *dialog;
  GtkEntry *entry1, *entry2;
  GtkToggleButton *case_sensitive;

  dialog = gtk_dialog_new_with_buttons (replace ? "Replace" : "Find",
					GTK_WINDOW (gtk_widget_get_toplevel (widget)),
					GTK_DIALOG_MODAL,
					GTK_STOCK_CANCEL,
					GTK_RESPONSE_CANCEL,
					GTK_STOCK_OK,
					GTK_RESPONSE_OK,
					NULL);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

  entry1 = g_object_new (GTK_TYPE_ENTRY,
			 "visible", TRUE,
			 "text", search_data.what ? search_data.what : "",
			 "activates-default", TRUE,
			 NULL);
  gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))),
		      GTK_WIDGET (entry1), TRUE, TRUE, 0);
  entry2 = g_object_new (GTK_TYPE_ENTRY,
			 "visible", replace,
			 "text", search_data.replacement ? search_data.replacement : "",
			 "activates-default", TRUE,
			 NULL);
  gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))),
		      GTK_WIDGET (entry2), TRUE, TRUE, 0);

  case_sensitive = g_object_new (GTK_TYPE_CHECK_BUTTON,
				 "visible", TRUE,
				 "label", "Case sensitive",
				 "active", !(search_data.flags & GTK_SOURCE_SEARCH_CASE_INSENSITIVE),
				 NULL);
  gtk_box_pack_start (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))),
		      GTK_WIDGET (case_sensitive), FALSE, FALSE, 0);

  while (TRUE)
    {
      if (gtk_dialog_run (GTK_DIALOG (dialog)) != GTK_RESPONSE_OK)
	{
	  gtk_widget_destroy (dialog);
	  return FALSE;
	}

      if (*gtk_entry_get_text (entry1))
	break;
    }

  g_free (search_data.what);
  *what_p = search_data.what = g_strdup (gtk_entry_get_text (entry1));
  g_free (search_data.replacement);
  *replacement_p = search_data.replacement = g_strdup (gtk_entry_get_text (entry2));
  *flags_p = search_data.flags = gtk_toggle_button_get_active (case_sensitive) ?
    0 : GTK_SOURCE_SEARCH_CASE_INSENSITIVE;

  gtk_widget_destroy (dialog);
  return TRUE;
}

static void
do_search_replace (GtkTextView *view,
		   gboolean     replace)
{
  GtkTextBuffer *buffer = gtk_text_view_get_buffer (view);
  GtkTextIter iter;
  char *what, *replacement;
  GtkSourceSearchFlags flags;

  if (!search_dialog (GTK_WIDGET (view), replace, &what, &replacement, &flags))
    return;

  if (replace)
    {
      gtk_text_buffer_get_iter_at_offset (buffer, &iter, 0);

      while (TRUE)
	{
	  GtkTextIter match_start, match_end;

	  if (!gtk_source_iter_forward_search (&iter, what, flags,
					       &match_start,
					       &match_end,
					       NULL))
	    {
	      break;
	    }

	  gtk_text_buffer_delete (buffer, &match_start, &match_end);
	  gtk_text_buffer_insert (buffer, &match_start, replacement, -1);
	  iter = match_start;
	}
    }
  else
    {
      GtkTextIter match_start, match_end;

      gtk_text_buffer_get_iter_at_mark (buffer, &iter, gtk_text_buffer_get_insert (buffer));

      if (gtk_source_iter_forward_search (&iter, what, flags, &match_start, &match_end, NULL))
	{
	  gtk_text_buffer_select_range (buffer, &match_start, &match_end);
	}
      else
	{
	  GtkTextIter insert = iter;
	  gtk_text_buffer_get_start_iter (buffer, &iter);
	  if (gtk_source_iter_forward_search (&iter, what, flags, &match_start, &match_end, &insert))
	    gtk_text_buffer_select_range (buffer, &match_start, &match_end);
	}
    }
}

static void
find_cb (GtkAction *action,
	 gpointer   user_data)
{
  do_search_replace (user_data, FALSE);
}

static void
replace_cb (GtkAction *action,
	    gpointer   user_data)
{
  do_search_replace (user_data, TRUE);
}

static void
open_file_cb (GtkAction *action, gpointer user_data)
{
  GtkWidget *chooser;
  gint response;
  static gchar *last_dir = NULL;
  g_return_if_fail (GTK_IS_TEXT_BUFFER (user_data));

  chooser = gtk_file_chooser_dialog_new ("Open file...",
					 NULL,
					 GTK_FILE_CHOOSER_ACTION_OPEN,
					 GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					 GTK_STOCK_OPEN, GTK_RESPONSE_OK,
					 NULL);

  if (last_dir != NULL && g_path_is_absolute (last_dir))
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser),
					 last_dir);

  response = gtk_dialog_run (GTK_DIALOG (chooser));

  if (response == GTK_RESPONSE_OK)
    {
      gchar *filename;
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
      if (filename != NULL)
	{
	  g_free (last_dir);
	  last_dir = gtk_file_chooser_get_current_folder (GTK_FILE_CHOOSER (chooser));
	  open_file (GTK_SOURCE_BUFFER (user_data), filename);
	  g_free (filename);
	}
    }
  gtk_widget_destroy (chooser);
}

#define NON_BLOCKING_PAGINATION
#ifndef NON_BLOCKING_PAGINATION

static void
begin_print (GtkPrintOperation        *operation,
	     GtkPrintContext          *context,
	     GtkSourcePrintCompositor *compositor)
{
  gint n_pages;

  while (!gtk_source_print_compositor_paginate (compositor, context))
    ;

  n_pages = gtk_source_print_compositor_get_n_pages (compositor);
  gtk_print_operation_set_n_pages (operation, n_pages);
}

#else

static gboolean
paginate (GtkPrintOperation        *operation,
	  GtkPrintContext          *context,
	  GtkSourcePrintCompositor *compositor)
{
  g_print ("Pagination progress: %.2f %%\n", gtk_source_print_compositor_get_pagination_progress (compositor) * 100.0);

  if (gtk_source_print_compositor_paginate (compositor, context))
    {
      gint n_pages;

      g_assert (gtk_source_print_compositor_get_pagination_progress (compositor) == 1.0);
      g_print ("Pagination progress: %.2f %%\n", gtk_source_print_compositor_get_pagination_progress (compositor) * 100.0);

      n_pages = gtk_source_print_compositor_get_n_pages (compositor);
      gtk_print_operation_set_n_pages (operation, n_pages);



      return TRUE;
    }

  return FALSE;
}

#endif

#define ENABLE_CUSTOM_OVERLAY

static void
draw_page (GtkPrintOperation        *operation,
	   GtkPrintContext          *context,
	   gint                      page_nr,
	   GtkSourcePrintCompositor *compositor)
{
#ifdef ENABLE_CUSTOM_OVERLAY

  /* This part of the code shows how to add a custom overlay to the
     printed text generated by GtkSourcePrintCompositor */

  cairo_t *cr;
  PangoLayout *layout;
  PangoFontDescription *desc;
  PangoRectangle rect;


  cr = gtk_print_context_get_cairo_context (context);

  cairo_save (cr);

  layout = gtk_print_context_create_pango_layout (context);

  pango_layout_set_text (layout, "Draft", -1);

  desc = pango_font_description_from_string ("Sans Bold 120");
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  pango_layout_get_extents (layout, NULL, &rect);

  cairo_move_to (cr,
		 (gtk_print_context_get_width (context) - ((double) rect.width / (double) PANGO_SCALE)) / 2,
		 (gtk_print_context_get_height (context) - ((double) rect.height / (double) PANGO_SCALE)) / 2);

  pango_cairo_layout_path (cr, layout);

  /* Font Outline */
  cairo_set_source_rgba (cr, 0.85, 0.85, 0.85, 0.80);
  cairo_set_line_width (cr, 0.5);
  cairo_stroke_preserve (cr);

  /* Font Fill */
  cairo_set_source_rgba (cr, 0.8, 0.8, 0.8, 0.60);
  cairo_fill (cr);

  g_object_unref (layout);
  cairo_restore (cr);
#endif

  /* To print page_nr you only need to call the following function */
  gtk_source_print_compositor_draw_page (compositor, context, page_nr);
}

static void
end_print (GtkPrintOperation        *operation,
	   GtkPrintContext          *context,
	   GtkSourcePrintCompositor *compositor)
{
  g_object_unref (compositor);
}

#define LINE_NUMBERS_FONT_NAME	"Sans 8"
#define HEADER_FONT_NAME	"Sans 11"
#define FOOTER_FONT_NAME	"Sans 11"
#define BODY_FONT_NAME		"Monospace 9"

/*
  #define SETUP_FROM_VIEW
*/

#undef SETUP_FROM_VIEW


static void
print_file_cb (GtkAction *action, gpointer user_data)
{
  GtkSourceView *view;
  GtkSourceBuffer *buffer;
  GtkSourcePrintCompositor *compositor;
  GtkPrintOperation *operation;
  const gchar *filename;
  gchar *basename;

  g_return_if_fail (GTK_IS_TEXT_VIEW (user_data));

  view = GTK_SOURCE_VIEW (user_data);

  buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));

  filename = g_object_get_data (G_OBJECT (buffer), "filename");
  basename = g_filename_display_basename (filename);

#ifdef SETUP_FROM_VIEW
  compositor = gtk_source_print_compositor_new_from_view (view);
#else

  compositor = gtk_source_print_compositor_new (buffer);

  gtk_source_print_compositor_set_tab_width (compositor,
					     gtk_source_view_get_tab_width (view));

  gtk_source_print_compositor_set_wrap_mode (compositor,
					     gtk_text_view_get_wrap_mode (GTK_TEXT_VIEW (view)));

  gtk_source_print_compositor_set_print_line_numbers (compositor, 1);

  gtk_source_print_compositor_set_body_font_name (compositor,
						  BODY_FONT_NAME);

  /* To test line numbers font != text font */
  gtk_source_print_compositor_set_line_numbers_font_name (compositor,
							  LINE_NUMBERS_FONT_NAME);

  gtk_source_print_compositor_set_header_format (compositor,
						 TRUE,
						 "Printed on %A",
						 "test-widget",
						 "%F");

  gtk_source_print_compositor_set_footer_format (compositor,
						 TRUE,
						 "%T",
						 basename,
						 "Page %N/%Q");

  gtk_source_print_compositor_set_print_header (compositor, TRUE);
  gtk_source_print_compositor_set_print_footer (compositor, TRUE);

  gtk_source_print_compositor_set_header_font_name (compositor,
						    HEADER_FONT_NAME);

  gtk_source_print_compositor_set_footer_font_name (compositor,
						    FOOTER_FONT_NAME);
#endif
  operation = gtk_print_operation_new ();

  gtk_print_operation_set_job_name (operation, basename);

  gtk_print_operation_set_show_progress (operation, TRUE);

#ifndef NON_BLOCKING_PAGINATION
  g_signal_connect (G_OBJECT (operation), "begin-print",
		    G_CALLBACK (begin_print), compositor);
#else
  g_signal_connect (G_OBJECT (operation), "paginate",
		    G_CALLBACK (paginate), compositor);
#endif
  g_signal_connect (G_OBJECT (operation), "draw-page",
		    G_CALLBACK (draw_page), compositor);
  g_signal_connect (G_OBJECT (operation), "end-print",
		    G_CALLBACK (end_print), compositor);

  gtk_print_operation_run (operation,
			   GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
			   NULL, NULL);

  g_object_unref (operation);
  g_free (basename);
}


/* View UI callbacks ------------------------------------------------------------------ */

typedef gboolean (*FileselOKFunc) (const char *filename, gpointer data);

static gboolean filechooser_save_run (GtkWindow    *parent,
				      const char   *title,
				      const char   *start_file,
				      FileselOKFunc func,
				      gpointer      data)
{
  gboolean result = FALSE;
  char *filename=NULL;
  GtkWidget *dialog;

  /* XXX if (!parent)  parent = get_active_window (); */

  dialog = gtk_file_chooser_dialog_new (title,
					parent,
					GTK_FILE_CHOOSER_ACTION_SAVE,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
					NULL);

  /* if (parent)  gtk_window_set_transient_for (GTK_WINDOW (dialog), parent); */

  if (start_file)
    gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog), start_file);

  while (1)
    {
      int rep = gtk_dialog_run (GTK_DIALOG (dialog));
      if ( rep ==  GTK_RESPONSE_ACCEPT )
	{
	  filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
	  if ((*func) (filename,data))
	    {
	      result = TRUE;
	      break;
	    }
	}
      else if ( rep == GTK_RESPONSE_CANCEL )
	{
	  break;
	}
    }
  gtk_widget_destroy (dialog);
  return result;
}

/* save buffer in filename
 */

static gboolean save_buffer (GtkSourceBuffer *buffer,const char *filename )
{
  GtkTextIter start, end;
  gchar *chars;
  gboolean result = FALSE;
  gboolean have_backup = FALSE;
  gchar *bak_filename;
  FILE *file;

  g_return_val_if_fail (filename != NULL, FALSE);
  bak_filename = g_strconcat (filename, "~", NULL);
  if ( g_rename (filename, bak_filename) != 0)
    {
      if (errno != ENOENT)
	{
	  gchar *err = g_strdup_printf ("Cannot back up '%s' to '%s': %s",
					filename, bak_filename, g_strerror (errno));
	  error_dialog (NULL,"%s",err);
	  g_free (err);
          return FALSE;
	}
    }
  else
    have_backup = TRUE;

  file = fopen (filename, "wb"); /* see porting issue bellow for binary */
  if (!file)
    {
      error_dialog (NULL,"Cannot back up '%s' to '%s': %s",filename, bak_filename, g_strerror (errno));
    }
  else
    {
      gtk_text_buffer_get_iter_at_offset (GTK_TEXT_BUFFER (buffer), &start, 0);
      gtk_text_buffer_get_end_iter (GTK_TEXT_BUFFER (buffer), &end);

      chars = gtk_text_buffer_get_slice (GTK_TEXT_BUFFER (buffer), &start, &end, FALSE);

      /* porting issue for win32
       * If the stream is from a file opened in text mode, any linefeed embedded
       * in the output string is translated to carriage-return linefeed on output
       */
      if (fputs (chars, file) == EOF ||
	  fclose (file) == EOF)
	{
	  error_dialog (NULL,"Error writing to '%s': %s",filename, g_strerror (errno));
	}
      else
	{
	  /* Success
	   */
	  result = TRUE;
	  gtk_text_buffer_set_modified (GTK_TEXT_BUFFER (buffer), FALSE);
	}

      g_free (chars);
    }

  if (!result && have_backup)
    {
      if ( g_rename (bak_filename, filename) != 0)
	{
	  error_dialog (NULL,"Error restoring backup file '%s' to '%s': %s\nBackup left as '%s'",
			filename, bak_filename, g_strerror (errno), bak_filename);
	}
    }
  g_free (bak_filename);
  return result;
}


static gboolean
save_as_ok_func (const char *filename, gpointer data)
{
  GtkSourceBuffer *buffer= data;
  char *old_filename = g_object_get_data (G_OBJECT (buffer), "filename");

  if (! old_filename  || strcmp (filename, old_filename) != 0)
    {
      struct stat statbuf;
      if (stat (filename, &statbuf) == 0)
	{
	  gint result;
	  gchar *err = g_strdup_printf ("Overwrite existing file '%s'?", filename);
	  GtkWidget *dialog = gtk_dialog_new_with_buttons (err,
							   NULL,
							   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
							   GTK_STOCK_OK,
							   GTK_RESPONSE_ACCEPT,
							   GTK_STOCK_CANCEL,
							   GTK_RESPONSE_REJECT,
							   NULL);
	  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
	  result = gtk_dialog_run (GTK_DIALOG (dialog));
	  g_free (err);
	  gtk_widget_destroy (dialog);
	  if (result != GTK_RESPONSE_ACCEPT ) return FALSE;
	}
    }

  if (save_buffer (buffer, filename ))
    {
      static GtkSourceLanguage *language;
      /* this will free old value */
      g_object_set_data_full (G_OBJECT (buffer),
			      "filename", g_strdup (filename),
			      (GDestroyNotify) g_free);
      /* language may have changed */
      language = get_language (GTK_TEXT_BUFFER (buffer), filename);
      if ( language != NULL)
	gtk_source_buffer_set_language (buffer, language);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

static void
save_as_cb (GtkAction *action, gpointer user_data)
{
  GtkSourceBuffer *buffer;
  g_return_if_fail (GTK_IS_TEXT_BUFFER (user_data));
  buffer = GTK_SOURCE_BUFFER (user_data);
  filechooser_save_run(NULL, "Save File", NULL, save_as_ok_func, buffer);
}

static void
save_cb (GtkAction *action, gpointer user_data)
{
  GtkSourceBuffer *buffer= user_data;
  char *filename = g_object_get_data (G_OBJECT (buffer), "filename");
  g_return_if_fail (GTK_IS_TEXT_BUFFER (user_data));
  buffer = GTK_SOURCE_BUFFER (user_data);
  if ( filename == NULL)
    save_as_cb(action,user_data);
  else
    save_buffer (buffer, filename );
}

/*
 *
 */

static gboolean check_buffer_saved (GtkAction *action, gpointer user_data)
{
  GtkSourceBuffer *buffer= user_data;
  if (gtk_text_buffer_get_modified (GTK_TEXT_BUFFER (buffer)))
    {
      char *pretty_name = buffer_pretty_name (buffer);
      char *msg = g_strdup_printf ("Save changes to '%s'?", pretty_name);
      gint result;
      g_free (pretty_name);
      GtkWidget *dialog = gtk_dialog_new_with_buttons (msg,
						       NULL,
						       GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
						       GTK_STOCK_YES,
						       GTK_RESPONSE_ACCEPT,
						       GTK_STOCK_NO,
						       GTK_RESPONSE_REJECT,
						       GTK_STOCK_CANCEL,
						       GTK_RESPONSE_REJECT,
						       NULL);
      gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
      result = gtk_dialog_run (GTK_DIALOG (dialog));
      g_free (msg);
      gtk_widget_destroy (dialog);
      if (result == GTK_RESPONSE_REJECT ) return FALSE;
      save_cb(action, user_data);
    }
  return TRUE;
}

static void
close_cb (GtkAction *action, gpointer user_data)
{
  GMainLoop *loop;
  GtkWidget *window;
  GtkSourceBuffer *buffer= user_data;
  int *hus = g_object_get_data (G_OBJECT (buffer), "smatrix");
  if ( hus != NULL)
    {
      NspSMatrix *S;
      /* this is a smatrix buffer */
      S = save_buffer_in_smatrix(buffer);
      g_object_set_data (G_OBJECT (buffer), "smatrix_value",S);
      /* we do not unref buffer */
    }
  else
    {
      if ( action != NULL)
	check_buffer_saved (action, buffer);
      g_object_unref (buffer);
    }
  window =  g_object_get_data (G_OBJECT (buffer), "buffer_window");
  loop = g_object_get_data (G_OBJECT (window), "main_loop");
  if ( loop != NULL) g_main_loop_quit (loop);
  gtk_widget_destroy (window);
}

/* View UI callbacks ------------------------------------------------------------------ */

static void
update_cursor_position (GtkTextBuffer *buffer, gpointer user_data)
{
  gchar *msg;
  gint row, col, chars, tabwidth;
  GtkTextIter iter, start;
  GtkSourceView *view;
  GtkLabel *pos_label;
  GString *str;
#ifdef HAVE_GTKSOURCEVIEW_CONTEXT_CLASS
  gchar **classes;
  gchar **classes_ptr;
#endif


  g_return_if_fail (GTK_IS_TEXT_VIEW (user_data));

  view = GTK_SOURCE_VIEW (user_data);
  tabwidth = gtk_source_view_get_tab_width (view);
  pos_label = GTK_LABEL (g_object_get_data (G_OBJECT (view), "pos_label"));

  gtk_text_buffer_get_iter_at_mark (buffer,
				    &iter,
				    gtk_text_buffer_get_insert (buffer));

  chars = gtk_text_iter_get_offset (&iter);
  row = gtk_text_iter_get_line (&iter) + 1;

  start = iter;
  gtk_text_iter_set_line_offset (&start, 0);
  col = 0;

  while (!gtk_text_iter_equal (&start, &iter))
    {
      if (gtk_text_iter_get_char (&start) == '\t')
	{
	  col += (tabwidth - (col % tabwidth));
	}
      else
	++col;

      gtk_text_iter_forward_char (&start);
    }

  str = g_string_new ("");

#ifdef HAVE_GTKSOURCEVIEW_CONTEXT_CLASS
  classes = gtk_source_buffer_get_context_classes_at_iter (GTK_SOURCE_BUFFER (buffer),
							   &iter);
  classes_ptr = classes;

  while (classes_ptr && *classes_ptr)
    {
      if (classes_ptr != classes)
	{
	  g_string_append (str, ", ");
	}

      g_string_append_printf (str, "%s", *classes_ptr);
      ++classes_ptr;
    }
  g_strfreev (classes);
#endif

  msg = g_strdup_printf ("char: %d, line: %d, column: %d, classes: %s", chars, row, col, str->str);
  gtk_label_set_text (pos_label, msg);

  g_free (msg);
  g_string_free (str, TRUE);
}

static void
move_cursor_cb (GtkTextBuffer *buffer,
		GtkTextIter   *cursoriter,
		GtkTextMark   *mark,
		gpointer       user_data)
{
  if (mark != gtk_text_buffer_get_insert (buffer))
    return;

  update_cursor_position (buffer, user_data);
}

static gboolean
window_deleted_cb (GtkWidget *widget, GdkEvent *ev, gpointer user_data)
{
  GMainLoop *loop=NULL;
  GtkSourceBuffer *buffer;
  GtkSourceView *view = GTK_SOURCE_VIEW (user_data);
  g_return_val_if_fail (GTK_IS_TEXT_VIEW (user_data), TRUE);
  buffer = GTK_SOURCE_BUFFER ( gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));
  windows = g_list_remove (windows, widget);
  /* deinstall buffer motion signal handlers */
  g_signal_handlers_disconnect_matched (buffer,
					G_SIGNAL_MATCH_DATA,
					0, /* signal_id */
					0, /* detail */
					NULL, /* closure */
					NULL, /* func */
					user_data);
  /* be sure to quit the loop if edit was entered in a wait mode (asynchronous) */
  loop = g_object_get_data (G_OBJECT (widget), "main_loop");
  if ( loop != NULL) g_main_loop_quit (loop);
  /* we return FALSE since we want the window destroyed */
  return FALSE;
}

#ifdef HAVE_GTKSOURCEVIEW_GUTTER
static void
line_mark_activated (GtkSourceGutter *gutter,
                     GtkTextIter     *iter,
                     GdkEventButton  *ev,
                     GtkSourceView   *view)
{
  GtkSourceBuffer *buffer;
  GSList *mark_list;
  const gchar *mark_type = MARK_TYPE_2;

  buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));

  /* get the marks already in the line */
  mark_list = gtk_source_buffer_get_source_marks_at_line (buffer,
							  gtk_text_iter_get_line (iter),
							  mark_type);

  if (mark_list != NULL)
    {
      /* just take the first and delete it */
      gtk_text_buffer_delete_mark (GTK_TEXT_BUFFER (buffer),
				   GTK_TEXT_MARK (mark_list->data));
    }
  else
    {
      /* no mark found: create one */
      gtk_source_buffer_create_source_mark (buffer,
					    NULL,
					    mark_type,
					    iter);
    }
  g_slist_free (mark_list);
}
#endif

#if 0
static void
drag_data_received_cb (GtkWidget        *widget,
		       GdkDragContext   *context,
		       gint              x,
		       gint              y,
		       GtkSelectionData *selection_data,
		       guint             info,
		       guint             timestamp,
		       gpointer          data)
{
  /* GtkSourceView *view = GTK_SOURCE_VIEW (data); */
  char *drop= (gchar *) gtk_selection_data_get_data (selection_data);
  Sciprintf("drop: %s\n",drop);
}
#endif

/* Window creation functions  */


#ifdef HAVE_GTKSOURCEVIEW_GUTTER

#if 0
static gchar *
mark_tooltip_func (GtkSourceMark *mark,
		   gpointer	  user_data)
{
  GtkTextBuffer *buf;
  GtkTextIter iter;
  gint line, column;

  buf = gtk_text_mark_get_buffer (GTK_TEXT_MARK (mark));

  gtk_text_buffer_get_iter_at_mark (buf, &iter, GTK_TEXT_MARK (mark));
  line = gtk_text_iter_get_line (&iter) + 1;
  column = gtk_text_iter_get_line_offset (&iter);
  /*   if (strcmp (gtk_source_mark_get_category (mark), MARK_TYPE_1) == 0) */
  return g_strdup_printf ("<b>Line</b>: %d\n<i>Column</i>: %d", line, column);
}
#endif

static void
add_source_mark_pixbufs (GtkSourceView *view)
{
  GdkColor color;
  gdk_color_parse ("pink", &color);
  /*
  gtk_source_view_set_mark_category_background (view, MARK_TYPE_2, &color);
  gtk_source_view_set_mark_category_icon_from_stock (view, MARK_TYPE_2, GTK_STOCK_NO);
  gtk_source_view_set_mark_category_priority (view, MARK_TYPE_2, 2);
  gtk_source_view_set_mark_category_tooltip_markup_func (view,
							 MARK_TYPE_2,
							 mark_tooltip_func,
							 NULL,
							 NULL);
  */
}
#endif

static void button_ok_clicked(GtkWidget *widget, void *user_data)
{
  close_cb (NULL,user_data);
}

static void button_cancel_clicked(GtkWidget *widget, void *user_data)
{
  GMainLoop *loop;
  GtkWidget *window;
  GtkSourceBuffer *buffer= user_data;
  window =  g_object_get_data (G_OBJECT (buffer), "buffer_window");
  loop = g_object_get_data (G_OBJECT (window), "main_loop");
  if ( loop != NULL) g_main_loop_quit (loop);
  gtk_widget_destroy (window);
}

static GtkWidget *
create_view_window (GtkSourceBuffer *buffer, GtkSourceView *from, const char *comment,
		    int close)
{
  GtkWidget *window, *sw, *view, *vbox, *pos_label,*descr_label, *menu;
  PangoFontDescription *font_desc = NULL;
  GtkAccelGroup *accel_group;
  GtkActionGroup *action_group;
  GtkUIManager *ui_manager;
  GError *error;

  g_return_val_if_fail (GTK_IS_TEXT_BUFFER (buffer), NULL);
  g_return_val_if_fail (from == NULL || GTK_IS_TEXT_VIEW (from), NULL);

  /* window */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_container_set_border_width (GTK_CONTAINER (window), 0);
  gtk_window_set_title (GTK_WINDOW (window), "GtkSourceView Demo");
  windows = g_list_append (windows, window);

  /* view */
  view = gtk_source_view_new_with_buffer (buffer);

  if (style_scheme)
    gtk_source_buffer_set_style_scheme (buffer, style_scheme);

  g_signal_connect (buffer, "mark-set", G_CALLBACK (move_cursor_cb), view);
  g_signal_connect (buffer, "changed", G_CALLBACK (update_cursor_position), view);
#ifdef HAVE_GTKSOURCEVIEW_GUTTER
  g_signal_connect (view,   "line-mark-activated", G_CALLBACK (line_mark_activated), view);
#endif
#if 0
  g_signal_connect (view,
		    "drag_data_received",
		    G_CALLBACK (drag_data_received_cb),
		    view);
#endif

  g_signal_connect (window, "delete-event", (GCallback) window_deleted_cb, view);

  /* action group and UI manager */
  action_group = gtk_action_group_new ("ViewActions");
  gtk_action_group_add_actions (action_group, view_action_entries,
				G_N_ELEMENTS (view_action_entries), view);
  gtk_action_group_add_toggle_actions (action_group, toggle_entries,
				       G_N_ELEMENTS (toggle_entries), view);
  gtk_action_group_add_radio_actions (action_group, tabs_radio_entries,
				      G_N_ELEMENTS (tabs_radio_entries),
				      -1, G_CALLBACK (tabs_toggled_cb), view);
  gtk_action_group_add_radio_actions (action_group, indent_radio_entries,
				      G_N_ELEMENTS (indent_radio_entries),
				      -1, G_CALLBACK (indent_toggled_cb), view);
  gtk_action_group_add_radio_actions (action_group, smart_home_end_entries,
				      G_N_ELEMENTS (smart_home_end_entries),
				      -1, G_CALLBACK (smart_home_end_toggled_cb), view);

  ui_manager = gtk_ui_manager_new ();
  gtk_ui_manager_insert_action_group (ui_manager, action_group, 0);
  g_object_unref (action_group);

  /* save a reference to the ui manager in the window for later use */
  g_object_set_data_full (G_OBJECT (window), "ui_manager",
			  ui_manager, (GDestroyNotify) g_object_unref);

  /* save a reference to the window in the buffer for later use
   * but we could have more than one window for a unique buffer
   */
  g_object_set_data (G_OBJECT (buffer), "buffer_window", window);
  g_object_set_data (G_OBJECT (buffer), "buffer_view", view);

  accel_group = gtk_ui_manager_get_accel_group (ui_manager);
  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);

  error = NULL;
  if (!gtk_ui_manager_add_ui_from_string (ui_manager, view_ui_description, -1, &error))
    {
      g_message ("building view ui failed: %s", error->message);
      g_error_free (error);
      exit (1);
    }

  /* misc widgets */
  vbox = gtk_vbox_new (0, FALSE);
  sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (sw),
				       GTK_SHADOW_IN);
  pos_label = gtk_label_new ("Position");
  g_object_set_data (G_OBJECT (view), "pos_label", pos_label);
  if ( comment != NULL )
    {
      descr_label = gtk_label_new (comment);
      g_object_set_data (G_OBJECT (view), "descr_label", descr_label);
    }
  menu = gtk_ui_manager_get_widget (ui_manager, "/MainMenu");
  /* layout widgets */
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_box_pack_start (GTK_BOX (vbox), menu, FALSE, FALSE, 0);
  if ( comment != NULL )
    gtk_box_pack_start (GTK_BOX (vbox), descr_label, FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), sw, TRUE, TRUE, 0);
  gtk_container_add (GTK_CONTAINER (sw), view);
  gtk_box_pack_start (GTK_BOX (vbox), pos_label, FALSE, FALSE, 0);

  if (close )
    {
      GtkWidget *button,*hbox;
      hbox = gtk_hbox_new (0, FALSE);
      button = gtk_button_new_from_stock(GTK_STOCK_OK);
      g_signal_connect (button,"clicked",G_CALLBACK (button_ok_clicked),buffer);
      gtk_box_pack_end (GTK_BOX (hbox), button, FALSE, FALSE, 0);
      button = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
      g_signal_connect (button,"clicked",G_CALLBACK (button_cancel_clicked),buffer);
      gtk_box_pack_end (GTK_BOX (hbox), button, FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
    }

  /* setup view */
  font_desc = pango_font_description_from_string ("monospace");
  if (font_desc != NULL)
    {
      gtk_widget_modify_font (view, font_desc);
      pango_font_description_free (font_desc);
    }
  /* default value used */
  gtk_source_view_set_tab_width (GTK_SOURCE_VIEW(view),2);

  /* change view attributes to match those of from */
  if (from)
    {
      gchar *tmp;
      gint i;
      GtkAction *action;

      action = gtk_action_group_get_action (action_group, "ShowNumbers");
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
				    gtk_source_view_get_show_line_numbers (from));

      action = gtk_action_group_get_action (action_group, "ShowMarks");
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
				    gtk_source_view_get_show_line_marks (from));

      action = gtk_action_group_get_action (action_group, "ShowMargin");
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
				    gtk_source_view_get_show_right_margin (from));

      action = gtk_action_group_get_action (action_group, "HlLine");
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
				    gtk_source_view_get_highlight_current_line (from));

      action = gtk_action_group_get_action (action_group, "WrapLines");
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
				    gtk_text_view_get_wrap_mode (GTK_TEXT_VIEW (from)) != GTK_WRAP_NONE);

      action = gtk_action_group_get_action (action_group, "AutoIndent");
      gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action),
				    gtk_source_view_get_auto_indent (from));

      action = gtk_action_group_get_action (action_group, "InsertSpaces");
      gtk_toggle_action_set_active (
				    GTK_TOGGLE_ACTION (action),
				    gtk_source_view_get_insert_spaces_instead_of_tabs (from));

      tmp = g_strdup_printf ("TabWidth%d", gtk_source_view_get_tab_width (from));
      action = gtk_action_group_get_action (action_group, tmp);
      if (action)
	gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);
      g_free (tmp);

      i = gtk_source_view_get_indent_width (from);
      tmp = i < 0 ? g_strdup ("IndentWidthUnset") : g_strdup_printf ("IndentWidth%d", i);
      action = gtk_action_group_get_action (action_group, tmp);
      if (action)
	gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);
      g_free (tmp);
    }

#ifdef HAVE_GTKSOURCEVIEW_GUTTER
  add_source_mark_pixbufs (GTK_SOURCE_VIEW (view));
#endif

  /* update the name */
  view_set_title (GTK_TEXT_VIEW (view),FALSE);

  gtk_widget_show_all (vbox);

  return window;
}

static GtkWidget *
create_main_window (GtkSourceBuffer *buffer,int flag,const char *comment,int close)
{
  GtkWidget *window;
  GtkAction *action;
  GtkUIManager *ui_manager;
  GtkActionGroup *action_group;
  GList *groups;
  GError *error;
  const gchar *buffer_ui_description;

  buffer_ui_description = (flag) ? buffer_file_ui_description :
    buffer_smatrix_ui_description;

  window = create_view_window (buffer, NULL,comment,close);
  ui_manager = g_object_get_data (G_OBJECT (window), "ui_manager");

  /* buffer action group */
  action_group = gtk_action_group_new ("BufferActions");
  gtk_action_group_add_actions (action_group, buffer_action_entries,
				G_N_ELEMENTS (buffer_action_entries), buffer);
  gtk_ui_manager_insert_action_group (ui_manager, action_group, 1);
  g_object_unref (action_group);

  /* merge buffer ui */
  error = NULL;

  if (!gtk_ui_manager_add_ui_from_string (ui_manager, buffer_ui_description, -1, &error))
    {
      g_message ("building buffer ui failed: %s", error->message);
      g_error_free (error);
      exit (1);
    }

  /* preselect menu checkitems */
  groups = gtk_ui_manager_get_action_groups (ui_manager);
  /* retrieve the view action group at position 0 in the list */
  action_group = g_list_nth_data (groups, 0);

  action = gtk_action_group_get_action (action_group, "HlBracket");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);

  action = gtk_action_group_get_action (action_group, "ShowNumbers");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);

  action = gtk_action_group_get_action (action_group, "ShowMarks");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);

  action = gtk_action_group_get_action (action_group, "ShowMargin");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), FALSE);

  action = gtk_action_group_get_action (action_group, "AutoIndent");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);

  action = gtk_action_group_get_action (action_group, "InsertSpaces");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), FALSE);

  action = gtk_action_group_get_action (action_group, "TabWidth2");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);

  action = gtk_action_group_get_action (action_group, "IndentWidthUnset");
  gtk_toggle_action_set_active (GTK_TOGGLE_ACTION (action), TRUE);

  return window;
}

#ifdef TEST
static void test_get_language (void);
#endif

/**
 * nsp_edit:
 * @fname: file name of file to be edited or %NULL
 * @read_only: a boolean
 * @wait:  a boolean
 *
 * creates a new edit function
 */

int nsp_edit(const char *fname,int read_only, int wait)
{
  GtkWidget *window;
  GtkSourceBuffer *buffer;
  GMainLoop *loop=NULL;
#ifdef TEST
  test_schemes () ;
  test_get_language ();
#endif
  /* create buffer */
  buffer = gtk_source_buffer_new (NULL);
  open_file (buffer, fname);
  /* create first window */
  window = create_main_window (buffer,TRUE,NULL,FALSE);
  gtk_window_set_default_size (GTK_WINDOW (window), 600, 400);
  gtk_widget_show (window);
  if ( wait == TRUE )
    {
      loop = g_main_loop_new (NULL,FALSE);
    }
  g_object_set_data (G_OBJECT (window), "main_loop", loop);
  if ( wait == TRUE )
    {
      GDK_THREADS_LEAVE();
      g_main_loop_run (loop);
      GDK_THREADS_ENTER();
    }
  return 0;
}

NspSMatrix *nsp_edit_smatrix(const char *title,const char *comment, NspSMatrix *S)
{
  NspSMatrix *Res;
  int wait = TRUE;
  GtkWidget *window;
  GtkSourceBuffer *buffer;
  GMainLoop *loop=NULL;
  /* create buffer */
  buffer = gtk_source_buffer_new (NULL);
  open_smatrix (buffer,title,S);
  /* create first window */
  window = create_main_window (buffer,FALSE,comment,TRUE);
  gtk_window_set_default_size (GTK_WINDOW (window), 600, 400);
  gtk_window_set_title (GTK_WINDOW (window), title);
  gtk_widget_show (window);
  if ( wait == TRUE )
    {
      loop = g_main_loop_new (NULL,FALSE);
    }
  g_object_set_data (G_OBJECT (window), "main_loop", loop);
  if ( wait == TRUE )
    {
      GDK_THREADS_LEAVE();
      g_main_loop_run (loop);
      GDK_THREADS_ENTER();
    }

  Res = g_object_get_data (G_OBJECT (buffer), "smatrix_value");
  g_object_unref (buffer);
  return Res;
}

int nsp_sourceview_cleanup(void)
{
  /* cleanup
  g_list_foreach (windows, (GFunc) gtk_widget_destroy, NULL);
  g_list_free (windows);
  g_object_unref (buffer);
  g_free (style_scheme_id);
  */
  return 0;
}

/*
 * list the schemes
 */

#ifdef TEST
static void test_schemes()
{
  const gchar * const * schemes;
  GtkSourceStyleSchemeManager *sm;
  sm = gtk_source_style_scheme_manager_get_default ();
  schemes = gtk_source_style_scheme_manager_get_scheme_ids (sm);
  g_print ("Available style schemes:\n");
  while (*schemes != NULL)
    {
      const gchar* const *authors;
      gchar *authors_str = NULL;
      style_scheme = gtk_source_style_scheme_manager_get_scheme (sm, *schemes);
      authors = gtk_source_style_scheme_get_authors (style_scheme);
      if (authors != NULL)
	authors_str = g_strjoinv (", ", (gchar **)authors);

      g_print (" - [%s] %s: %s\n",
	       gtk_source_style_scheme_get_id (style_scheme),
	       gtk_source_style_scheme_get_name (style_scheme),
	       gtk_source_style_scheme_get_description (style_scheme) ?
	       gtk_source_style_scheme_get_description (style_scheme) : "");

      if (authors_str) {
	g_print ("   by %s\n",  authors_str);
	g_free (authors_str);
      }
      ++schemes;
    }
  g_print("\n");
}

/*
 * test function which list the languages
 */

static void test_get_language (void)
{
  GtkSourceLanguageManager *lm;
  const gchar * const *ids;
  lm = gtk_source_language_manager_get_default ();
  ids = gtk_source_language_manager_get_language_ids (lm);
  g_assert (ids != NULL);
  g_print ("Available languages:\n");
  while (*ids != NULL)
    {
      GtkSourceLanguage *lang1, *lang2;
      g_print ("[%s]\n",*ids);
      lang1 = gtk_source_language_manager_get_language (lm, *ids);
      g_assert (lang1 != NULL);
      g_assert (GTK_IS_SOURCE_LANGUAGE (lang1));
      g_assert_cmpstr (*ids, == , gtk_source_language_get_id (lang1));
      /* langs are owned by the manager */
      lang2 = gtk_source_language_manager_get_language (lm, *ids);
      g_assert (lang1 == lang2);
      ++ids;
    }
}
#endif


static GtkSourceLanguage *nsp_gtksource_language (void)
{
  static  GtkSourceLanguageManager *lm=NULL;
  GtkSourceLanguage *nsp;
  gchar *lang_dirs[] = {NULL, NULL};
  char nsp_lang[FSIZE+1];
  if ( lm == NULL )
    {
      nsp_path_expand("NSP/config/language-specs",nsp_lang,FSIZE);
      lang_dirs[0]= nsp_lang;
      lm = gtk_source_language_manager_new ();
      gtk_source_language_manager_set_search_path (lm, lang_dirs);
    }
  nsp = gtk_source_language_manager_get_language(lm,"nsp");
  return nsp;
}

static void view_set_title (GtkTextView   *view, int read_only)
{
  GtkWidget *window;
  GtkSourceBuffer *buffer = GTK_SOURCE_BUFFER (gtk_text_view_get_buffer (GTK_TEXT_VIEW (view)));
  char *pretty_name = buffer_pretty_name (buffer);
  char *title;
  if (read_only == TRUE )
    title = g_strconcat ("nsp view - ", pretty_name, NULL);
  else
    title = g_strconcat ("nsp edit - ", pretty_name, NULL);
  window =  g_object_get_data (G_OBJECT (buffer), "buffer_window");
  if ( window)   gtk_window_set_title (GTK_WINDOW (window), title);
  g_free (pretty_name);
  g_free (title);
}

/* return a name to store in the view
 */

static char *buffer_pretty_name (GtkSourceBuffer *buffer)
{
  static int untitled_serial=1;
  char *filename = g_object_get_data (G_OBJECT (buffer), "filename");
  if (filename)
    {
      char *p;
      char *result = g_path_get_basename (filename);
      p = strchr (result, '/');
      if (p)
	*p = '\0';
      return result;
    }
  else
    {
      int *hus = g_object_get_data (G_OBJECT (buffer), "untitled_serial"),us;
      if ( hus == NULL)
	{
	  us = untitled_serial++;
	  g_object_set_data (G_OBJECT (buffer), "untitled_serial", GINT_TO_POINTER(us));
	}
      else
	{
	  us = GPOINTER_TO_INT(hus);
	}
      return  ( us == 1 ) ?  g_strdup ("Untitled"): g_strdup_printf ("Untitled #%d", us);
    }
}
