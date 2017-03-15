/* Nsp
 * Copyright (C) 1998-2016 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Graphic library
 * jpc@cermics.enpc.fr
 *
 * Initialize a graphic window
 * Common code for Gtk/OpenGL and Cairo
 *--------------------------------------------------------------------------*/

/*
 * initgraphic : initialize graphic window
 * If v2 is not a nul pointer *v2 is the window number to create
 * EntryCounter is used to check for first Entry + to know the next
 * available window number
 */
#ifdef PERICAIRO
#include <cairo-pdf.h>
#include <cairo-ps.h>
#include <cairo-svg.h>
#endif

static NspFigure *nsp_initgraphic(const char *string,GtkWidget *win,GtkWidget *box,int *v2,
				  int *wdim,int *wpdim,double *viewport_pos,int *wpos, void *data, void *Fig);

static void *initgraphic(const char *string, int *v2,int *wdim,int *wpdim,double *viewport_pos,
			 int *wpos,char mode, void *data,void *Fig)
{
  return nsp_initgraphic(string,NULL,NULL,v2,wdim,wpdim,viewport_pos,wpos,data,Fig);
}

/* used when a graphic window is to be inserted in a more complex
 * widget hierarchy
 */

#if defined(PERIGL) || defined(PERIGTK3GL)
#define nsp_graphic_new_new nsp_graphic_new_gl_new
#endif

#if defined(PERICAIRO) && !defined(PERIGTK3GL)
int nsp_graphic_new_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos);
int nsp_graphic_new_cairo_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  return nsp_graphic_new_new(win,box,v2,wdim,wpdim,viewport_pos,wpos);
}
/* #define nsp_graphic_new_new nsp_graphic_new_new */
#endif

int nsp_graphic_new_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  nsp_initgraphic("",win,box,&v2,wdim,wpdim,viewport_pos,wpos,NULL,NULL);
  return  nsp_get_win_counter()-1;
}

static NspFigure *nsp_initgraphic(const char *string,GtkWidget *win,GtkWidget *box,int *v2,
				  int *wdim,int *wpdim,double *viewport_pos,int *wpos, void *data,void *Fig)
{
  char *name=NULL,*driver=NULL;
  const gchar *gname = "unknown";
  int i;
  GdkColor white={0,0,0,0};
  GdkColor black={0,65535,65535,65535};
  BCG *NewXgc ;
  /* Attention ici on peut faire deux fenetre de meme num�ro � r�gler ? XXXXX */
  int WinNum = ( v2 != (int *) NULL && *v2 != -1 ) ? *v2 : nsp_get_win_counter();
  gui_private *private ;

  /* private Xgc data initialized to 0 */
  if ( ( private = calloc(1,sizeof(gui_private)))== NULL)
    {
      Sciprintf("initgraphics: running out of memory \n");
      return NULL;
    }
  /* default values  */
  private->resize = 0;
  private->zzin_expose= FALSE;
  private->protect= FALSE;
  private->draw= FALSE;
  private->draw_init= TRUE;
  private->gcol_bg = white;
  private->gcol_fg = black;
#if defined(PERIGL) || defined(PERIGTK3GL)
  private->gdk_only= FALSE;
  private->gl_only= FALSE;
#endif
  private->invalidated.width = 0;
  private->invalidated.height = 0;
  private->invalidated.x = 0;
  private->invalidated.y = 0;

  if (( NewXgc = window_list_new(private) ) == (BCG *) 0)
    {
      Sciprintf("initgraphics: unable to alloc\n");
      return NULL;
    }
  NewXgc->CurWindow = WinNum;

  /* the graphic engine associated to this graphic window */
#if defined(PERIGL) || defined(PERIGTK3GL)
  NewXgc->graphic_engine = &GL_gengine ;
#else
#if defined(PERICAIRO) && !defined(PERIGTK3GL)
  NewXgc->graphic_engine = &Cairo_gengine;
#else
  NewXgc->graphic_engine = &Gtk_gengine;
#endif
#endif
  NewXgc->actions = &nsp_gc_actions;

  start_sci_gtk(); /* be sure that gtk is started */

  NewXgc->CurLineWidth=0;
  NewXgc->CurPattern=0;
  NewXgc->CurColor=1;
  NewXgc->CurPixmapStatus=0;
  NewXgc->CurVectorStyle=0;
  NewXgc->CurDrawFunction=0;
  NewXgc->ClipRegionSet=0;
  NewXgc->CurDashStyle=0;
  NewXgc->IDLastPattern=0;
  NewXgc->Numcolors=0;
  NewXgc->NumBackground=0;
  NewXgc->NumForeground=0;
  NewXgc->NumHidden3d=0;
  NewXgc->Autoclear=0;
  /* default colormap not instaled */
  NewXgc->CmapFlag = -1;
  /* default resize not yet defined */
  NewXgc->CurResizeStatus = -1; /* to be sure that next will initialize */
  NewXgc->CurColorStatus = -1;  /* to be sure that next will initialize */
  for ( i = 0 ; i < 4 ; i++) NewXgc->zrect[i]=0;
  NewXgc->figure_bg_draw = TRUE;
  /* cairo graphics without window */
#ifdef PERICAIRO
  if ( string == NULL || string[0]=='\0' )
    {
#endif
      if ( win != NULL )
	{
	  gtk_nsp_graphic_window(FALSE,NewXgc,"unix:0",win,box,wdim,wpdim,viewport_pos,wpos);
	}
      else
	{
	  gtk_nsp_graphic_window(TRUE,NewXgc,"unix:0",NULL,NULL,wdim,wpdim,viewport_pos,wpos);
	}
#ifdef PERICAIRO
    }
  else
    {
      /* when string is non null then surface can be a transmited surface */
      cairo_t *cairo_cr = data;
      if ( cairo_cr == NULL)
	{
	  cairo_surface_t *surface;
	  surface = cairo_pdf_surface_create (string,400,600 );
	  if ( surface != NULL)
	    {
	      cairo_cr = cairo_create (surface);
	      /* cairo_scale(cairo_cr,0.5,0.5); */ /* ZZ */
	      cairo_surface_destroy (surface);
	    }
	}
      NewXgc->private->cairo_drawable_cr = cairo_cr;
    }
#endif

  /* next values are to be set since initialize_gc
   * action depend on the current state defined by these
   * variables. For pixmap, resizestatus and colorstatus
   * initialize performs a switch from old value to new value
   */
  /* Default value is without Pixmap */
  NewXgc->CurPixmapStatus = 0;
#if defined(PERIGL) || defined(PERIGTK3GL)
  NewXgc->private->drawable = gtk_widget_get_window(NewXgc->private->drawing);
#endif
  /* initialize a pango_layout */
  nsp_fonts_initialize(NewXgc);
  nsp_initialize_gc(NewXgc);
  NewXgc->graphic_engine->xset_pixmapOn(NewXgc,0);
  
  /* attach a figure to the graphic window */
  if ( private->window != NULL)
    {
      gname = gtk_window_get_title (GTK_WINDOW(private->window));
    }

  if (( name =new_nsp_string(gname)) == NULLSTRING)
    return NULL;
  if (( driver =new_nsp_string("Gtk")) == NULLSTRING)
    return NULL;
  if ( Fig == NULL )
    {
      Fig = nsp_figure_create("fig",name,driver,NewXgc->CurWindow,NULL,NULL,TRUE,NULL,NULL,
			      TRUE,NULL,NewXgc,NULL);
      if ( Fig == NULL) return NULL;
    }
  else
    {
      ((NspFigure *) Fig)->obj->id = NewXgc->CurWindow;
      ((NspFigure *) Fig)->obj->Xgc = NewXgc;
    }
  NewXgc->figure = Fig;
  nsp_set_current_figure(Fig);
  nsp_set_win_counter(WinNum);
  gdk_flush();
  return Fig;
}


/*
 * partial or full creation of a graphic nsp widget
 * if is_top == FALSE a partial widget (vbox) is created
 */

static GtkTargetEntry target_table[] = {
  { "GTK_TREE_MODEL_ROW",1,10},
  { "STRING",    1, 11},
  { "text/plain", 1,12}
};

static guint n_targets = sizeof(target_table) / sizeof(target_table[0]);


static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos);

static GtkWidget *nsp_gtk_vbox_new(void)
{
#if GTK_CHECK_VERSION(3,0,0)
  return gtk_box_new(GTK_ORIENTATION_VERTICAL,0);
#else 
  return gtk_vbox_new (FALSE, 0);
#endif
}

static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  static char gwin_name[100];
  gint iw, ih;
  GtkWidget *scrolled_window;
  GtkWidget *vbox;
#if defined(PERIGL) 
  guint mode = GDK_GL_MODE_RGB | GDK_GL_MODE_DEPTH | GDK_GL_MODE_STENCIL;
  GdkGLConfig *glconfig;

  glconfig = gdk_gl_config_new_by_mode (mode | GDK_GL_MODE_DOUBLE);
  if (glconfig == NULL)
    {
      glconfig = gdk_gl_config_new_by_mode (mode);
      if (glconfig == NULL)
	{
	  /* FIXME XXX */
	  g_print ("*** No appropriate OpenGL-capable visual found.\n");
	  return;
	}
    }
  /* examine_gl_config_attrib(glconfig); */
#endif

  /* initialise pointers */
  dd->private->drawing = NULL;
  /* dd->private->wgc = NULL; */
  dd->private->gcursor = NULL;
  dd->private->ccursor = NULL;
  /* gdk_rgb_init(); */
  /* gtk_widget_push_visual(gdk_rgb_get_visual()); */
  /* gtk_widget_push_colormap(gdk_rgb_get_cmap()); */

  /* create window etc */
  if ( wdim != NULL )
    {
      dd->CWindowWidth = iw = wdim[0]; /*  / pixelWidth(); */
      dd->CWindowHeight = ih = wdim[1]; /*  pixelHeight(); */
    }
  else if ( wpdim != NULL )
    {
      dd->CWindowWidth = iw = wpdim[0]; /*  / pixelWidth(); */
      dd->CWindowHeight = ih = wpdim[1]; /*  pixelHeight(); */
    }
  else
    {
      dd->CWindowWidth = iw = 600; /*  / pixelWidth(); */
      dd->CWindowHeight = ih = 400; /*  pixelHeight(); */
    }

  if ( is_top == TRUE )
    {
      dd->private->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      sprintf( gwin_name, "Graphic Window %d", dd->CurWindow );
      gtk_window_set_title (GTK_WINDOW (dd->private->window),  gwin_name);
      gtk_window_set_resizable(GTK_WINDOW(dd->private->window), TRUE);
      gtk_widget_realize(dd->private->window);
#if GTK_CHECK_VERSION(3,0,0)
      vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL,0);
#else 
      vbox = gtk_vbox_new (FALSE, 0);
#endif 
      gtk_container_add (GTK_CONTAINER (dd->private->window), vbox);
    }
  else
    {
      dd->private->window = win ;
      sprintf( gwin_name, "Graphic Window %d", dd->CurWindow );
      gtk_window_set_title (GTK_WINDOW (dd->private->window),  gwin_name);
      gtk_window_set_resizable(GTK_WINDOW(dd->private->window), TRUE);
#if GTK_CHECK_VERSION(3,0,0)
      vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL,0);
#else 
      vbox = gtk_vbox_new (FALSE, 0);
#endif 
      if ( wpdim != NULL)
	{
	  gtk_widget_set_size_request (vbox,Min(wpdim[0],iw),Min(wpdim[1],ih));
	}
      else
	{
	  gtk_widget_set_size_request (vbox,iw,ih);
	}
      gtk_box_pack_start (GTK_BOX (box), vbox, TRUE, TRUE, 0);
    }
  
  dd->private->vbox = nsp_gtk_vbox_new();
  gtk_box_pack_start (GTK_BOX (vbox), dd->private->vbox, FALSE, TRUE, 0);
  
  /* menu will be inserted in private->vbox */
  dd->private->menu_entries = graphic_initial_menu(dd->CurWindow );
  dd->private->menubar = NULL;
  create_graphic_window_menu(dd);
  /* toolbar */
  /*
   * nsp_gwin_add_ui_manager(dd,vbox);
   */
  /* status bar in vbox */
  dd->private->CinfoW = gtk_statusbar_new ();
  gtk_box_pack_start (GTK_BOX (vbox), dd->private->CinfoW, FALSE, TRUE, 0);
  
  /* create a new scrolled window. */
  dd->private->scrolled = scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_set_border_width (GTK_CONTAINER (scrolled_window),0);
#if GTK_CHECK_VERSION(3,16,0)
  gtk_scrolled_window_set_overlay_scrolling(GTK_SCROLLED_WINDOW(scrolled_window),FALSE);
#endif
  /* fix min size of the scrolled window */
  if ( wpdim != NULL)
    {
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				      GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
      gtk_widget_set_size_request (scrolled_window,Min(wpdim[0],iw),Min(wpdim[1],ih));
    }
  else
    {
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				      GTK_POLICY_NEVER, GTK_POLICY_NEVER);
      gtk_widget_set_size_request (scrolled_window,iw,ih);
    }

  /* place and realize the scrolled window  */
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);
  
  if ( is_top == TRUE )
    gtk_widget_realize(scrolled_window);
  else
    gtk_widget_show(scrolled_window);
  
  /* create private->drawingarea */
  
#if GTK_CHECK_VERSION(3,0,0) && defined(PERIGTK3GL)
  dd->private->drawing = gtk_gl_area_new ();
  gtk_widget_set_hexpand (dd->private->drawing, TRUE);
  gtk_widget_set_vexpand (dd->private->drawing, TRUE);
#else
  /* standard widget + gtkglext */
  dd->private->drawing = gtk_drawing_area_new();
#endif
    
#if defined(PERIGL) 
  /* Set OpenGL-capability to the widget
   * opengl rendering in the window
   */
  gtk_widget_set_gl_capability (dd->private->drawing,
				glconfig,
				NULL,
				TRUE,
				GDK_GL_RGBA_TYPE);
#else
  /* we use our own double buffer */
  gtk_widget_set_double_buffered (dd->private->drawing , FALSE);

#endif /* defined(PERIGL) */

  /* gtk_widget_set_usize (GTK_WIDGET (dd->private->cairo_drawing),600,400); */
#if 0
  gtk_drag_source_set(dd->private->window,GDK_BUTTON1_MASK |
		      GDK_BUTTON2_MASK | GDK_BUTTON3_MASK,
		      target_table, n_targets ,GDK_ACTION_COPY);
  g_signal_connect ((dd->private->drawing), "drag_data_get",
		    G_CALLBACK (target_drag_data_get), NULL);

  g_signal_connect ((dd->private->drawing), "drag_drop",
		    G_CALLBACK( target_drag_drop), NULL);
#endif

  g_signal_connect ((dd->private->drawing), "drag_data_received",
		    G_CALLBACK (target_drag_data_received), NULL);

  gtk_drag_dest_set (dd->private->drawing,GTK_DEST_DEFAULT_ALL,
		     target_table, n_targets ,GDK_ACTION_COPY
		     | GDK_ACTION_MOVE |GDK_ACTION_LINK);

#if GTK_CHECK_VERSION(3,0,0) && defined(PERIGTK3GL)
  g_signal_connect ((dd->private->drawing), "realize",
		    G_CALLBACK (realize_gtk3gl), (gpointer) dd);
  g_signal_connect ((dd->private->drawing), "unrealize",
		    G_CALLBACK (unrealize_gtk3gl),(gpointer) dd);
#else
  g_signal_connect_after((dd->private->drawing), "realize",
			 G_CALLBACK(realize_event), (gpointer) dd);
#endif
  
  g_signal_connect((dd->private->drawing), "motion-notify-event",
		   G_CALLBACK(locator_button_motion), (gpointer) dd);
  g_signal_connect((dd->private->drawing), "button-press-event",
		   G_CALLBACK(locator_button_press), (gpointer) dd);
  g_signal_connect((dd->private->drawing), "button-release-event",
		   G_CALLBACK(locator_button_release), (gpointer) dd);


  /* GDK_POINTER_MOTION_HINT_MASK is a special mask which is used
   * to reduce the number of GDK_MOTION_NOTIFY events received.
   */

  gtk_widget_set_events(dd->private->drawing, GDK_EXPOSURE_MASK
			| GDK_BUTTON_PRESS_MASK
			| GDK_BUTTON_RELEASE_MASK
			| GDK_POINTER_MOTION_HINT_MASK
			/* get all motions */
			| GDK_POINTER_MOTION_MASK
			/* get motion when pressed buttons move*/
			/* | GDK_BUTTON_MOTION_MASK */
			| GDK_LEAVE_NOTIFY_MASK );

  /* private->drawingarea properties */
  /* min size of the graphic window */
  gtk_widget_set_size_request(GTK_WIDGET (dd->private->drawing), iw, ih);

  /* insert drawing area in the scrolled window */
#if GTK_CHECK_VERSION(3,0,0)
  gtk_container_add(GTK_CONTAINER(scrolled_window),GTK_WIDGET (dd->private->drawing));
#else 
  gtk_scrolled_window_add_with_viewport ( GTK_SCROLLED_WINDOW (scrolled_window),
					  GTK_WIDGET (dd->private->drawing));
#endif 
  if ( is_top == TRUE )
    {
      gtk_widget_realize(dd->private->drawing);
    }
  else
    {
      gtk_widget_show(dd->private->drawing);
    }
  
  /* connect to signal handlers, etc */
#if GTK_CHECK_VERSION(3,0,0) && defined(PERIGTK3GL)
  g_signal_connect(dd->private->drawing, "resize",
		   G_CALLBACK(resize_event), (gpointer) dd);
#else
  g_signal_connect(dd->private->drawing, "configure_event",
		   G_CALLBACK(configure_event), (gpointer) dd);
#endif
  /* 
     g_signal_connect((dd->private->drawing), "size_allocate",
     G_CALLBACK(size_allocate_event), (gpointer) dd);
  */

#if GTK_CHECK_VERSION(3,0,0)
#if defined(PERIGTK3GL)
  g_signal_connect((dd->private->drawing), "render",
		   G_CALLBACK(render_callback), (gpointer) dd);
  g_signal_connect((dd->private->scrolled), "draw",
		   G_CALLBACK(scrolled_draw_callback), (gpointer) dd);
#else
  g_signal_connect((dd->private->drawing), "draw",
		   G_CALLBACK(draw_callback), (gpointer) dd);
  g_signal_connect((dd->private->scrolled), "draw",
		   G_CALLBACK(scrolled_draw_callback), (gpointer) dd);
#endif
#else
  g_signal_connect((dd->private->drawing), "expose_event",
		   G_CALLBACK(expose_event_new), (gpointer) dd);
#endif 
  
  /*
   *  g_signal_connect (G_OBJECT (dd->private->cairo_drawing), "paint",
   *  G_CALLBACK (cairo_paint),(gpointer) dd );
   */

  g_signal_connect((dd->private->window), "destroy",
		   G_CALLBACK(sci_destroy_window), (gpointer) dd);

  g_signal_connect((dd->private->window), "delete_event",
		   G_CALLBACK(sci_delete_window), (gpointer) dd);

  g_signal_connect ((dd->private->window), "key_press_event",
		    G_CALLBACK(key_press_event_new), (gpointer) dd);

  /* show everything */

  if ( is_top == TRUE )
    {
      /* create offscreen drawable : Already done in the realize_event
       */
      if ( wpos != NULL )
	gtk_window_move (GTK_WINDOW(dd->private->window),wpos[0],wpos[1]);
      gtk_widget_realize(dd->private->window);
      gtk_widget_show_all(dd->private->window);
    }
  else
    {
      /* we need here to realize the dd->private->drawing
       * this will create offscreen drawable : in realize_event
       * and the initialize_gc
       */
      gtk_widget_realize(dd->private->drawing);
    }


#if GTK_CHECK_VERSION(3,0,0)
  {
    /* need dd->private->window to be realized */
    GQuark quark = g_quark_from_string("xgc");
    g_object_set_qdata_full(G_OBJECT(gtk_widget_get_window(dd->private->drawing)), quark, (gpointer) dd  , NULL);
    /* gtk_widget_get_window may return NULL is widget is not realized */
    gdk_window_set_invalidate_handler (
				       gtk_widget_get_window(dd->private->drawing),
				       nsp_drawing_invalidate_handler);
  }
#endif
  
  if ( viewport_pos != NULL )
    {
      gtk_adjustment_set_value( gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (scrolled_window)),
				(gfloat) viewport_pos[0]);
      gtk_adjustment_set_value( gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (scrolled_window)),
				(gfloat) viewport_pos[1]);
    }
  
  if ( wpdim != NULL)
    {
      dd->CurResizeStatus = 0;
    }
  else
    {
      dd->CurResizeStatus = 1;
    }
}
