/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 * store a list of graphic contexts
 * this is gui independant the gui dependant part is
 * delegated to private member of winxgc
 *--------------------------------------------------------------------------*/

#include "nsp/graphics-new/Graphics.h"

extern int window_list_check_top(BCG *,void *) ;
extern int window_list_check_drawing(BCG *dd,void *win);

/*
 * default values for scales.
 */

static window_scale_list  default_scale =
  {
  scale_flag: 0,
  scale_flag3d:  0,
  scale_3drot_flag:  0,
  wdim:  {600,400},
  subwin_rect:  {0.0,0.0,1.0,1.0},
  frect:   {0.0,0.0,1.0,1.0},
  zfrect:  {0.0,0.0},
  axis:  {1/8.0,1/8.0,1/8.0,1/8.0},
  xtics:  {0.0,1.0,0.0,10},
  ytics:  {0.0,1.0,0.0,10},
  strflag:  '5',
  Wxofset1: 75.0,
  Wyofset1: 53.0,
  Wscx1: 450.0,
  Wscy1: 318.0,
  logflag:   {'n','n'},
  Irect:  {75,53,450,318},
  Waaint1: {2,10,2,10},
  m: {{1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0}},
  bbox1:  {-1.0,1.0,-1.0,1.0,-1.0,1.0},
  c: {0.0,0.0,0.0},
  alpha:35.0, theta: 45.0,
  metric3d:1,                   /* added by es */
  cosa:1.0, sina: 0.0           /* test: cosa, sina */
  };

/**
 * WindowList:
 * @winxgc: graphic context #BCG
 * @next: next window.
 * @prev: previous window.
 *
 * A doubly linked list of graphic context #BCG of graphic windows
 *
 */

typedef struct _window_list WindowList;

struct  _window_list
{
  BCG winxgc;
  WindowList *next;
  WindowList *prev;
} ;

static WindowList *The_List  = (WindowList *) NULL;
static window_scale_list *new_wcscale ( window_scale_list *val);
static WindowList  *window_list_search_w_int(WindowList *listptr, int i);
static int window_list_search_topelevel_int(WindowList *,void *,int);
static void scale_copy (window_scale_list *s1, window_scale_list *s2);
static int window_list_search_from_drawing__(WindowList *listptr,void *win);

/**
 * check_graphic_window_new:
 *
 * returns the BCG structure associated with
 * the current graphic window. If no current graphic
 * window exists, one is created
 *
 * Return value: the current BCG to be used
 **/

extern Gengine Cairo_gengine; /* XXXXX */

BCG *check_graphic_window_new(void)
{
  BCG *loc =  window_list_get_first();
  if ( loc == NULL )
    {
#ifdef THREAD_VERSION
      gdk_threads_enter();
#endif
      Cairo_gengine.xset_curwin(0,TRUE);
#ifdef THREAD_VERSION
      gdk_threads_leave();
#endif
    }
  return  window_list_get_first();
}


/**
 * set_graphic_window:
 * @num: an integer
 *
 * The current graphic window is set to window @num
 * If the window does not exist it is created.
 *
 * Return value: the new graphic context associated to window @num.
 **/

BCG *set_graphic_window_new(int num)
{
  Cairo_gengine.xset_curwin(Max(0,num),TRUE);
  return  window_list_get_first();
}


/**
 * window_list_new:
 * @private: extra information for #BCG
 *
 * Creates a new graphix context. The scales are also
 * initialized with default scale.
 *
 * Return value: a new graphic context
 **/

BCG *window_list_new(void *private)
{
  WindowList *loc =  (WindowList *) MALLOC (sizeof(WindowList));
  if ( loc == NULL)
    {
      Sciprintf("window_list_new_entry_int: running out of memory\n");
      return NULL;
    }
  else
    {
      loc->winxgc.scales = NULL;
      if (( loc->winxgc.scales = new_wcscale(&default_scale))== NULL)
	{
	  Sciprintf("set_window_scale: running out of memory\n");
	  return NULL;
	}
      loc->winxgc.private = private ;
      loc->winxgc.CurWindow = 0;
      loc->winxgc.CmapFlag  = 1;
      loc->winxgc.EventHandler[0] = '\0';
      loc->winxgc.queue.in=0;
      loc->winxgc.queue.out=0;
      loc->winxgc.queue.size=MaxCB;
      loc->next = The_List ;
      if ( The_List != NULL) The_List->prev = loc;
      loc->prev = NULL;
      The_List = loc ;
    }
  return &loc->winxgc;
}

/**
 * window_list_win_to_front:
 * @win:
 *
 * move the graphic context associated to window @win to the front of
 * the window list. Which is the place where information about the
 * current window is stored.
 *
 **/

BCG * window_list_win_to_front(int win)
{
  WindowList *loc  =  window_list_search_w_int(The_List,win);
  if ( loc != NULL)
    {
      if ( loc->prev != NULL)
	(loc->prev)->next = loc->next ;
      if ( loc->next != NULL) (loc->next)->prev = loc->prev ;
      /* reinsert loc on top */
      loc->next = The_List ;
      loc->next->prev = loc ;
      loc->prev = NULL;
      The_List = loc;
      return &loc->winxgc;
    }
  else
    return NULL;
}

/**
 * window_list_remove:
 * @num:
 *
 * Free the entry in window list for window number @num.
 * note that scales are destroyed here and private data
 * are destroyed elsewhere
 *
 **/

void window_list_remove(int num)
{
  WindowList *L1= The_List ;
  while ( L1 != (WindowList *) NULL)
    {
      if ( L1->winxgc.CurWindow == num )
	{
	  if ( L1->prev != NULL)
	    {
	      if ( L1->next != NULL) L1->next->prev = L1->prev;
	      L1->prev->next = L1->next ;
	    }
	  else
	    {
	      The_List = (WindowList *) L1->next ;
	      if ( L1->next != NULL) L1->next->prev = NULL;
	    }
	  /* now L1 is to be freed */
	  FREE(L1->winxgc.scales)
	  FREE(L1);
	  break;
	}
      L1 = (WindowList *) L1->next;
    }
  return;
}

/**
 * window_list_search_new:
 * @winnum:
 *
 * Search a graphic context for window @winnum
 *
 *
 * Return value: %NULL or a graphic context
 **/

BCG *window_list_search_new(int winnum)
{
  WindowList *loc = window_list_search_w_int(The_List,(Max(0,winnum)));
  if ( loc == NULL) return NULL;
  return  &(loc->winxgc);
}

static WindowList  *window_list_search_w_int(WindowList *listptr, int i)
{
  while ( listptr != NULL)
    {
      if ((listptr->winxgc.CurWindow) == i) return( listptr);
      listptr = listptr->next;
    }
  return NULL;
}

/**
 * window_list_search_from_drawing
 * @win: address of the drawing widget of a graphic window
 *
 * Search a graphic context for a window described by its
 * drawing widget address. The window id is returned or
 * -1 in case of failure.
 *
 *
 * Return value: an integer
 **/

int window_list_search_from_drawing(void *win)
{
  return window_list_search_from_drawing__(The_List,win);
}

static int window_list_search_from_drawing__(WindowList *listptr,void *win)
{
  if (listptr == (WindowList  *) NULL) return -1;
  if ( window_list_check_drawing(&listptr->winxgc,win) == TRUE)
    return listptr->winxgc.CurWindow;
  return window_list_search_from_drawing__((WindowList *) listptr->next,win);
}

/**
 * window_list_search_toplevel:
 * @win: a void pointer
 *
 * returns the number of graphic windows which share
 * the same toplevel widget.
 *
 * Return value: an integer
 **/

int window_list_search_toplevel(void *win)
{
  return window_list_search_topelevel_int(The_List,win,0);
}

static int window_list_search_topelevel_int(WindowList *listptr,void *win, int count)
{
  if (listptr == (WindowList  *) NULL)
    {
      return count;
    }
  else
    {
      if ( window_list_check_top(&listptr->winxgc,win) == TRUE) count++;
      return window_list_search_topelevel_int((WindowList *) listptr->next,win,count);
    }
}


/**
 * window_list_get_first:
 * @void:
 *
 * returns the first graphic context found in the window list
 *
 * Return value: %NULL or a graphic context
 **/

BCG *window_list_get_first(void)
{
  return ( The_List == (WindowList *) NULL) ?  (BCG *) 0 :  &(The_List->winxgc);
}


/**
 * window_list_get_ids:
 * @Num: int pointer
 * @Ids: pointer to an array of integers
 * @flag: 0 or 1
 *
 * If @flag is equal to zero then returns in @Num the number
 * of graphic windows in the graphic window list.
 * If @flag is equal to one then @Ids must be a pointer to
 * an allocated array of proper size (size returned by the call with @flag =0)
 * which is then filled with the ids of the graphic windows.
 *
 **/

void window_list_get_ids(int *Num, int *Ids, int flag)
{
  WindowList *listptr = The_List;
  *Num = 0;
  if ( flag == 0 )
    {
      while ( listptr != (WindowList  *) 0 )
	{
	  (*Num)++;
	  listptr = (WindowList *) listptr->next;
	}
    }
  else
    {
      while ( listptr != (WindowList  *) 0 )
	{
	  Ids[*Num] = listptr->winxgc.CurWindow;
	  listptr =  (WindowList *)listptr->next;
	  (*Num)++;
	}
    }
}


/**
 * window_list_get_max_id:
 *
 * returns the highest id of graphic windows or -1 if no
 * graphic windows.
 *
 * Returns: an integer
 **/

int window_list_get_max_id(void)
{
  WindowList *listptr = The_List;
  int Num = -1;
  while ( listptr != (WindowList  *) 0 )
    {
      Num = Max(listptr->winxgc.CurWindow,Num);
      listptr =  (WindowList *)listptr->next;
    }
  /* Sciprintf("Max Id : %d \n",Num); */
  return(Num);
}

/*
 * Graphic scales
 */

/**
 * xgc_add_default_scale:
 * @Xgc:  a graphic context
 *
 * adds a copy of default scale at the begining of the window scale list
 * of graphic context @Xgc.
 *
 * Returns: %OK or %FAIL.
 **/

int xgc_add_default_scale(BCG *Xgc)
{
  if (( Xgc->scales = new_wcscale(&default_scale))== NULL)
    {
      Sciprintf("set_window_scale: running out of memory\n");
      return FAIL;
    }
  return OK;
}

/**
 * xgc_reset_scales_to_default:
 * @Xgc:  a graphic context
 *
 * reset the scale to default scale.
 *
 * Returns: %OK or %FAIL.
 **/

int xgc_reset_scales_to_default(BCG *Xgc)
{
  if ( Xgc->scales == NULL) return FAIL;
  scale_copy(Xgc->scales,&default_scale);
  return OK;
}

/**
 * new_wcscale:
 * @val: the scale to use for copy
 *
 * returns a new window_scale_list which is a copy of val.
 *
 * Return value: a new #window_scale_list or %NULL
 **/

static window_scale_list *new_wcscale(window_scale_list *val)
{
  window_scale_list *new ;
  if ((new = MALLOC(sizeof(window_scale_list))) == NULL) return NULL;
  scale_copy(new,val);
  return new;
}

/**
 * scale_copy:
 * @s1: a #window_scale_list
 * @s2: a #window_scale_list
 *
 * Uses @s2 to update @s1
 *
 **/

static void scale_copy( window_scale_list *s1,window_scale_list *s2)
{
  int i,j;
  s1->scale_flag=s2->scale_flag;
  s1->scale_flag3d=s2->scale_flag3d;
  s1->scale_3drot_flag=s2->scale_3drot_flag;
  s1->wdim[0]=  s2->wdim[0];
  s1->wdim[1]=  s2->wdim[1];
  s1->Irect=s2->Irect;
  for (i=0; i< 4; i++)
    {
      s1->subwin_rect[i]=s2->subwin_rect[i];
      s1->frect[i]=s2->frect[i];
      s1->Waaint1[i]=s2->Waaint1[i];
      s1->xtics[i]=s2->xtics[i];
      s1->ytics[i]=s2->ytics[i];
      s1->axis[i]=s2->axis[i];
    }
  s1->strflag = s2->strflag;
  for (i=0; i< 2; i++) s1->zfrect[i]=s2->zfrect[i];
  for (i=0; i< 3; i++)
    for (j=0; j< 3; j++)
      s1->m[i][j]=s2->m[i][j];
  for (i=0; i< 6; i++)
    s1->bbox1[i]=s2->bbox1[i] ;
  s1->Wxofset1=s2->Wxofset1;
  s1->Wyofset1=s2->Wyofset1;
  s1->Wscx1=s2->Wscx1;
  s1->Wscy1=s2->Wscy1;
  s1->logflag[0] = s2->logflag[0];
  s1->logflag[1] = s2->logflag[1];
  s1->cosa = s2->cosa;
  s1->sina = s2->sina;
  s1->alpha = s2->alpha;
  s1->theta = s2->theta;
  for ( i = 0 ; i < 3 ; i++) s1->c[i]= s2->c[i];
  s1->metric3d = s2->metric3d;
}

/**
 * nsp_scale_copy:
 * @scale1: a #nsp_gcscale
 * @scale2: a #nsp_gcscale
 *
 * Uses @scale2 to update @scale1
 *
 **/

void nsp_scale_copy(nsp_gcscale *scale1,nsp_gcscale *scale2)
{
  scale_copy(scale1,scale2);
}

/**
 * nsp_scale_default:
 * @scale1: a #nsp_gcscale
 *
 * Uses default_scale to update @scale1
 *
 **/

void nsp_scale_default(nsp_gcscale *scale1)
{
  scale_copy(scale1,&default_scale);
}


/**
 * getscale2d:
 * @Xgc:
 * @WRect:
 * @FREct:
 * @logscale:
 * @ARect:
 *
 * get current scale values.
 *
 * Returns: 0
 **/

int getscale2d(BCG *Xgc,double WRect[4],double FRect[4],char *logscale,double ARect[4])
{
  int i;
  static double ten=10.0;
  logscale[0] = Xgc->scales->logflag[0];
  logscale[1] = Xgc->scales->logflag[1];
  for ( i=0; i < 4 ; i++)
    {
      WRect[i]=Xgc->scales->subwin_rect[i];
      FRect[i]=Xgc->scales->frect[i];
      ARect[i]=Xgc->scales->axis[i];
    }
  if (logscale[0]=='l')
    {
      FRect[0]=pow(ten,FRect[0]);
      FRect[2]=pow(ten,FRect[2]);
    }
  if (logscale[1]=='l')
    {
      FRect[1]=pow(ten,FRect[1]);
      FRect[3]=pow(ten,FRect[3]);
    }
  return(0);
}


/**
 * set_scale:
 * @Xgc: a graphic context
 * @flag: flag to indicate which arguments to use
 * @subwin: subwindow specification
 * @frame_values: array with xmin,ymin,xmax,ymax
 * @aaint:  array with xint,x_subint,y_int,y_subint
 * @logflag: xlogflag,ylogflag
 * @axis_values: mfact_xl, mfact_xr,mfact_yu,mfact_yd
 *
 * changes selected items in Xgc scale
 * Warning : frame_values[i] must be log10(val[i])
 *           when using log scales
 *
 **/

void set_scale(nsp_gcscale *scale,const int wdim[2],const double subwin[4],
	       const double frame_values[4], const int aaint[4],const char logflag[2],
	       const double axis_values[4])
{
  char wdim_changed= 'f',subwin_changed='f';
  char frame_values_changed='f',aaint_changed='f';
  char logflag_changed='f';
  char axis_changed = 'f';
  int i;

  if ( wdim != NULL  )
    {
      if ( scale->wdim[0] != wdim[0] || scale->wdim[1] != wdim[1])
	{
	  scale->wdim[0] = wdim[0];
	  scale->wdim[1] = wdim[1];
	  wdim_changed='t';
	}
    }

  if ( subwin != NULL)
    {
      subwin_changed='t' ;
      memcpy(scale->subwin_rect, subwin ,4*sizeof(double));
    }

  if ( frame_values != NULL)
    {
      for (i=0; i < 4 ; i++ )
	if ( frame_values[i] != scale->frect[i]) { frame_values_changed='t' ; break;}
      /* if no scales were present and the values given are the same as the
       * default frect values we must register that we are setting a scale
       */
      if ( scale->scale_flag == 0)  frame_values_changed='t' ;
    }

  if ( aaint != NULL )
    {
      for (i=0; i < 4 ; i++ )
	if ( aaint[i] != scale->Waaint1[i]) { aaint_changed='t' ; break;}
      if (  aaint_changed== 't' )
	{
	  for (i=0; i < 4 ; i++) scale->Waaint1[i]=aaint[i];
	}
    }

  if ( logflag != NULL )
    {
      for (i=0; i < 2 ; i++ )
	if ( logflag[i] != scale->logflag[i]) { logflag_changed='t' ; break;}
      if ( logflag_changed== 't' )
	{
	  scale->logflag[0]=logflag[0];      scale->logflag[1]=logflag[1];
	}
    }

  if ( axis_values != NULL )
    {
      for (i=0; i < 4 ; i++ )
	if ( axis_values[i] != scale->axis[i]) { axis_changed='t' ; break;}
      if ( axis_changed == 't')
	{
	  for (i=0; i < 4 ; i++ ) scale->axis[i] = axis_values[i];
	}
    }

  if ( frame_values_changed == 't' )
    {
      for (i=0; i < 4 ; i++) scale->frect[i]=frame_values[i];
      /* the scale is no more a default scale */
      scale->scale_flag = 1;
    }

  if ( wdim_changed == 't' || subwin_changed == 't' || frame_values_changed == 't'
       ||  axis_changed == 't' )
    {
      /* Upgrading constants for 2D transformation */
      double scx,scy,xoff,yoff,val;
      scx =  ((double) scale->wdim[0]*scale->subwin_rect[2]-scale->wdim[0]*scale->subwin_rect[2]
	      *(scale->axis[0]+scale->axis[1]));
      scy =  ((double) scale->wdim[1]*scale->subwin_rect[3]-scale->wdim[1]*scale->subwin_rect[3]
	      *(scale->axis[2]+scale->axis[3]));
      xoff= ((double) scale->wdim[0]*scale->subwin_rect[2])*(scale->axis[0]);
      yoff= ((double) scale->wdim[1]*scale->subwin_rect[3])*(scale->axis[2]);

      scale->Wxofset1= xoff+scale->subwin_rect[0]*((double)scale->wdim[0]);
      scale->Wyofset1= yoff+scale->subwin_rect[1]*((double)scale->wdim[1]);
      scale->Wscx1   = scx;
      scale->Wscy1   = scy;

      val = Abs(scale->frect[0]- scale->frect[2]);
      scale->Wscx1 = (val <=SMDOUBLE) ? scale->Wscx1/SMDOUBLE : scale->Wscx1/val;
      val = Abs(scale->frect[1]- scale->frect[3]);
      scale->Wscy1 = (val <=SMDOUBLE) ? scale->Wscy1/SMDOUBLE : scale->Wscy1/val;

      scale->Irect.x = XScale(scale, scale->frect[0]);
      scale->Irect.y = YScale(scale, scale->frect[3]);
      scale->Irect.width = Abs(XScale(scale, scale->frect[2]) -  XScale(scale, scale->frect[0]));
      scale->Irect.height = Abs(YScale(scale, scale->frect[3]) -  YScale(scale, scale->frect[1]));

    }

  if (  aaint_changed== 't' || frame_values_changed == 't')
    {
      scale->xtics[0] = scale->frect[0];
      scale->xtics[1] = scale->frect[2];
      scale->ytics[0] = scale->frect[1];
      scale->ytics[1] = scale->frect[3];
      scale->xtics[2] = 0.0;
      scale->ytics[2] = 0.0;
      scale->xtics[3] = scale->Waaint1[1];
      scale->ytics[3] = scale->Waaint1[3];
    }
}


/**
 * get_cwindow_dims:
 * @wdims: an integer pointer
 *
 * get dimensions of the current graphic window.
 *
 **/

void get_cwindow_dims( int *wdims)
{
  BCG *Xgc = check_graphic_window_new();
  Xgc->graphic_engine->xget_windowdim(Xgc,wdims,wdims+1);
}


/**
 * window_list_check_queue:
 * @Xgc:  graphic context #BCG
 * @ev: a #nsp_gwin_event
 *
 * checks if there's an already stored event in a
 * graphic window queue. If @Xgc is %NULL all the
 * windows are searched
 * else only Xgc is searched
 *
 *
 * Returns: %OK or %FAIL.
 **/

int window_list_check_queue(BCG *Xgc,nsp_gwin_event *ev)
{
  WindowList *L1= The_List ;
  if ( Xgc == NULL )
    {
      /* search the whole list */
      while ( L1 != (WindowList *) NULL)
	{
	  if ( nsp_queue_empty(&L1->winxgc.queue) == FALSE )
	    {
	      *ev= nsp_dequeue(&L1->winxgc.queue);
	      return OK;
	    }
	  L1 = (WindowList *) L1->next;
	}
    }
  else
    {
      if ( nsp_queue_empty(&Xgc->queue) == FALSE )
	{
	  *ev= nsp_dequeue(&Xgc->queue);
	  return OK;
	}
    }
  return FAIL;
}

/**
 * window_list_clear_queue:
 * @Xgc: graphic context #BCG
 *
 * clears the event queues of the window
 * described by graphic context @Xgc or all
 * the event queues if @Xgc is %NULL.
 *
 **/

void window_list_clear_queue(BCG *Xgc)
{
  WindowList *L1= The_List ;
  if ( Xgc == NULL )
    {
      /* clear all queues */
      while ( L1 != (WindowList *) NULL)
	{
	  nsp_clear_queue(&L1->winxgc.queue);
	  L1 = (WindowList *) L1->next;
	}
    }
  else
    {
      nsp_clear_queue(&Xgc->queue);
    }
}
