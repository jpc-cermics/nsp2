/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------
 * store a list of graphic contexts 
 * this is gui independant the gui dependant part is 
 * delegated to private member of winxgc  
 *--------------------------------------------------------------------------*/

#include "Graphics.h" 

struct BCG *ScilabXgc = (struct BCG *) 0;            /* current BCG */

/*
 * structure for Window List 
 */

typedef  struct  
{
  struct BCG winxgc;
  struct WindowList *next;
} WindowList  ;

static WindowList *The_List  = (WindowList *) NULL;  /* list of windows */

static int set_window_scale(int win,window_scale_list *scale);
static window_scale_list *check_subwin_wcscale( window_scale_list *listptr, double subwin_rect[4]);
static void scale_copy (window_scale_list *s1, window_scale_list *s2);
static integer curwin (void);
static window_scale_list *new_wcscale ( window_scale_list *val);
static int same_subwin (double lsubwin_rect[4],double subwin_rect[4]);
static int set_window_scale(int win,window_scale_list *scale);
static int same_subwin( double lsubwin_rect[4],double subwin_rect[4]);
static void scale_copy( window_scale_list *s1,window_scale_list *s2);

/*-------------------------------------------------------------------------
 * Adds a new entry at the end of the Window List 
 * and returns a pointer to that entry 
 *-------------------------------------------------------------------------*/

static struct BCG *window_list_new_entry_int(WindowList **,void *private );

struct BCG *window_list_new(void *private)
{
  return( window_list_new_entry_int(&The_List,private));
}

static struct BCG *window_list_new_entry_int(WindowList **listptr,void *private )
{ 
  if ( *listptr == (WindowList *) NULL)
    {
      *listptr = (WindowList *) MALLOC (sizeof(WindowList));
      if ( listptr == 0) 
	{
	  Sciprintf("window_list_new_entry_int: running out of memory\n");
	  return((struct BCG *) 0);
	}
      else 
	{ 
	  (*listptr)->winxgc.private = private ;
	  (*listptr)->winxgc.scales = NULL ;
	  (*listptr)->winxgc.CurWindow = 0;
	  (*listptr)->winxgc.CmapFlag  = 1;
	  (*listptr)->winxgc.EventHandler[0] = '\0';
	  (*listptr)->next = (struct WindowList *) NULL ;
	  return(&((*listptr)->winxgc));
	}
    }
  else
    {
      return( window_list_new_entry_int((WindowList **) &((*listptr)->next), private));
    }
}

/*---------------------------------------------------------------------
 * Free the entry in window list for window number num 
 * note that scales and private data are destroyed elsewhere 
 *---------------------------------------------------------------------*/

void window_list_remove(int num)
{
  WindowList *L1= The_List ,*L2= The_List ;
  window_scale_list *loc;
  while ( L1 != (WindowList *) NULL)
    {
      if ( L1->winxgc.CurWindow == num )
	{
	  /* window num found */
	  if ( L1 != L2 )
	    {
	      /* it is not the first entry */
	      L2->next= L1->next ;
	      break;
	    }
	  else 
	    {
	      /* it is the first entry */
	      The_List = (WindowList *) L1->next ;
	      break;
	    }
	}
      else 
	{
	  L2 = L1;
	  L1 = (WindowList *) L1->next;
	}
    }
  /* now L1 is to be freed */
  loc = L1->winxgc.scales ; 
  while ( loc != NULL)
    {
      window_scale_list *next = loc->next;
      FREE(loc);
      loc = next;
    }
  FREE(L1);
  return;
}


/*---------------------------------------------------
 * returns the graphic context of window i 
 * or 0 if this window does not exists
 *---------------------------------------------------*/

static struct BCG *window_list_search_int (WindowList *,int);

struct BCG *window_list_search(int winnum)
{ 
  return window_list_search_int(The_List,Max(0,winnum));
}

static struct BCG *window_list_search_int(WindowList *listptr, int i)
{
  if (listptr == (WindowList  *) NULL)
    {
      return((struct BCG *) 0);
    }
  else 
    { 
      if ((listptr->winxgc.CurWindow) == i)
	{
	  return( &(listptr->winxgc));
	}
      else 
	{
	  return(window_list_search_int((WindowList *) listptr->next,i));
	}
    }
}

/*---------------------------------------------------
 * returns number of graphic windows which shares the same toplevel widget 
 *---------------------------------------------------*/

static int window_list_search_topelevel_int(WindowList *,void *,int);
extern int window_list_check_top(struct BCG *,void *) ;

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


/*----------------------------------------------------------------------
 * get first entry or NULL
 *----------------------------------------------------------------------*/

struct BCG * window_list_get_first()
{
  return ( The_List == (WindowList *) NULL) ?  (struct BCG *) 0 :  &(The_List->winxgc);
}


/*----------------------------------------------------------------------
 * get ids of scilab windows in array Ids,
 * Num gives the number of windows
 * flag == 1 ==> get the Ids 
 * flag == 0 ==> just get the Number Num 
 *----------------------------------------------------------------------*/

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

/*---------------------------------------------------------------------------
 * get the highest id of scilab windows
 * or -1 if no windows 
 *---------------------------------------------------------------------------*/

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

/*------------------------------------------------------------------------
 * Graphic scales 
 --------------------------------------------------------------------------*/

static void scale_copy (window_scale_list *s1, window_scale_list *s2);
static integer curwin (void);
static window_scale_list *new_wcscale ( window_scale_list *val);
static int same_subwin (double lsubwin_rect[4],double subwin_rect[4]);

/* Current Scale */

window_scale_list current_scale = 
{ 
  0,
  {600,400},
  {0.0,0.0,1.0,1.0},
  {0.0,0.0,1.0,1.0},
  {1/8.0,1/8.0,1/8.0,1/8.0},
  {0.0,1.0,0.0,10},
  {0.0,1.0,0.0,10},
  75.0,53.0,450.0,318.0,
  {'n','n'},
  {75,53,450,318},
  {2,10,2,10},
  {{1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0}},
  {0.0,1.0,0.0,1.0,0.0,1.0},
  35.0,45.0,
  1,                 /* added by es */
  (window_scale_list *) 0, /*unused */
  (window_scale_list *) 0 /*unused */
};

/** default values **/

static window_scale_list  default_scale = 
{ 
  0,
  {600,400},
  {0.0,0.0,1.0,1.0},
  {0.0,0.0,1.0,1.0},
  {1/8.0,1/8.0,1/8.0,1/8.0},
  {0.0,1.0,0.0,10},
  {0.0,1.0,0.0,10},
  75.0,53.0,450.0,318.0,
  {'n','n'},
  {75,53,450,318},
  {2,10,2,10},
  {{1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0}},
  {0.0,1.0,0.0,1.0,0.0,1.0},
  35.0,45.0,
  1,                 /* added by es */
  (window_scale_list *) 0, /*unused */
  (window_scale_list *) 0 /*unused */
};

/*----------------------------------------------------------
 * Back to defaults values : fills current scale (Scale)
 * and curwin() scale with default scale.
 *----------------------------------------------------------*/

void current_scale2default()
{
  scale_copy(&current_scale,&default_scale);  
  set_window_scale(curwin(),&current_scale);
}

void set_window_scale_with_default(int win) 
{ 
  set_window_scale(win,&default_scale);
} 

/*------------------------------------------------------------
 * void get_window_scale(i,subwin)
 *   if subwin == NULL : search for existing scale values of window i.
 *   if subwin != NULL : search for existing scale values of subwindow subwin of window i.
 *   If no scale are found we do nothing and return 0
 *   else the scale is copied into current scale current_scale ans we return 1.
 *   a subwindow is described by an array of four doubles 
 *-------------------------------------------------------------*/

int get_window_scale(int win,double *subwin)
{ 
  window_scale_list *listptr;
  struct BCG * winxgc = window_list_search(win);
  if ( winxgc == NULL) return FAIL;
  listptr = winxgc->scales;
  if ( subwin == NULL) 
    {
      scale_copy(&current_scale,listptr);
      return OK;
    }
  else
    {
      window_scale_list *loc = check_subwin_wcscale(listptr,subwin);
      if ( loc != NULL) 
	{
	  scale_copy(&current_scale,loc);
	  return OK;
	}
    }
  return FAIL;
}

/*------------------------------------------------------------
 * void set_window_scale(i,scale)
 * add XXXX (a copy ? ) scale at the begining of window i scale list 
 * (which is also modified) making current_scale the current scale of window i 
 *-------------------------------------------------------------*/

static int set_window_scale(int win,window_scale_list *scale)
{ 
  window_scale_list *loc;
  struct BCG * winxgc = window_list_search(win);
  if ( winxgc == NULL) return FAIL;
  if ( winxgc->scales ==  NULL)
    {
      winxgc->scales = new_wcscale(scale);
      if ( winxgc->scales == 0) 
	{
	  Scistring("set_window_scale: running out of memory\n");
	  return FAIL;
	}
      return OK;
    }
  loc = check_subwin_wcscale(winxgc->scales,scale->subwin_rect); 
  if ( loc != NULL) 
    {
      /* subwin exists we update it with data from scale
       * loc->next and loc->previous are not changed !
       */
      scale_copy(loc,scale);
      /* we move listptr to the top of the list */
      if ( loc != winxgc->scales )
	{
	  /* remove loc */
	  (loc->prev)->next = loc->next ;
	  if ( loc->next != NULL) (loc->next)->prev = loc->prev ;
	  /* reinsert loc on top */
	  loc->next = winxgc->scales;
	  loc->next->prev = loc ;
	  winxgc->scales = loc;
	}      
    }
  else 
    {
      /* subwin does not exists we add it a the begining of the list */
      if (( loc = new_wcscale(scale))== NULL)
	{
	  Scistring("set_window_scale: running out of memory\n");
	  return FAIL;
	}
      else 
	{
	  loc->next = winxgc->scales;
	  if (loc->next != NULL) loc->next->prev = loc;
	  winxgc->scales = loc;
	}
    }
  return OK;
}
 
static window_scale_list *new_wcscale(window_scale_list *val) 
{
  window_scale_list *new ;
  if ((new = MALLOC(sizeof(window_scale_list))) == NULL) return NULL;
  new->next = (window_scale_list *) 0;
  new->prev = (window_scale_list *) 0;
  scale_copy(new,val);
  return new;
}

static window_scale_list *check_subwin_wcscale( window_scale_list *listptr, double subwin_rect[4])
{
  if ( listptr == (window_scale_list  *) NULL)  return NULL;
  if ( same_subwin( listptr->subwin_rect,subwin_rect)) 
    return listptr;
  else 
    return check_subwin_wcscale(listptr->next,subwin_rect);
}

static int same_subwin( double lsubwin_rect[4],double subwin_rect[4])
{
  if ( Abs(lsubwin_rect[0] - subwin_rect[0]) < 1.e-8
       && Abs(lsubwin_rect[1] - subwin_rect[1]) < 1.e-8
       && Abs(lsubwin_rect[2] - subwin_rect[2]) < 1.e-8
       && Abs(lsubwin_rect[3] - subwin_rect[3]) < 1.e-8 ) 
    return 1;
  else 
    return 0;
}

/*------------------------------------------------------------
 * delete the scales associated to window i 
 *-------------------------------------------------------------*/

void window_scale_delete(int win)
{ 

  window_scale_list *loc;
  struct BCG * winxgc = window_list_search(win);
  if ( winxgc == NULL) return ; 
  if ( winxgc->scales ==  NULL) return ;
  loc = winxgc->scales ;
  while ( loc != NULL)
    {
      window_scale_list *next = loc->next;
      FREE(loc);
      loc = next;
    }
  winxgc->scales = NULL;
}

/*-------------------------------------------
 * static void scale_copy(s1,s2) : s1=s2 
 * take care not to change s1->next and s1->previous 
 *-------------------------------------------*/

static void scale_copy( window_scale_list *s1,window_scale_list *s2)
{
  int i,j;
  s1->flag=s2->flag;
  s1->wdim[0]=  s2->wdim[0];
  s1->wdim[1]=  s2->wdim[1];
  for (i=0; i< 4; i++) 
    {
      s1->subwin_rect[i]=s2->subwin_rect[i];
      s1->frect[i]=s2->frect[i];
      s1->WIRect1[i]=s2->WIRect1[i];
      s1->Waaint1[i]=s2->Waaint1[i];
      s1->xtics[i]=s2->xtics[i];
      s1->ytics[i]=s2->ytics[i];
      s1->axis[i]=s2->axis[i];
    }
  for (i=0; i< 3; i++) 
    for (j=0; i< 3; i++) 
      s1->m[i][j]=s2->m[i][j];
  for (i=0; i< 6; i++) 
    s1->bbox1[i]=s2->bbox1[i] ;
  s1->Wxofset1=s2->Wxofset1;
  s1->Wyofset1=s2->Wyofset1;
  s1->Wscx1=s2->Wscx1;
  s1->Wscy1=s2->Wscy1;
  s1->logflag[0] = s2->logflag[0];
  s1->logflag[1] = s2->logflag[1];
}

/*-------------------------------------------
 * return current window : ok if driver is Rec
 *-------------------------------------------*/

static integer curwin()
{
  return nsp_gengine->xget_curwin();
}

/*-------------------------------------------
 * show the recorded scales for window i 
 *-------------------------------------------*/

void show_scales(void)
{
  window_scale_list *loc;
  int win = curwin();
  struct BCG * winxgc = window_list_search(win);
  if ( winxgc == NULL ||  winxgc->scales ==  NULL) 
    {
      Sciprintf("No scales for window %d\r\n",win);
      return ;
    }
  Sciprintf("scales for window %d\r\n",win);
  loc = winxgc->scales ;
  while ( loc != NULL)
    {
      sciprint("\tsubwin=[%5.2f,%5.2f,%5.2f,%5.2f], flag=%d\r\n",
	       loc->subwin_rect[0],loc->subwin_rect[1],loc->subwin_rect[2],loc->subwin_rect[3],
	       loc->flag);      
      loc = loc->next;
    }
}


/*-------------------------------------------
 * setscale2d 
 * uses WRect,FRect,logscale to update 
 * current scale (current_scale) 
 *  WRect gives the subwindow to use 
 *  FRect gives the bounds 
 *  WRect=[<x-upperleft>,<y-upperleft>,largeur,hauteur]
 *    example WRect=[0,0,1.0,1.0] we use all the window 
 *            WRect=[0.5,0.5,0.5,0.5] we use the down right 
 *            quarter of the window 
 *-------------------------------------------*/

int setscale2d(double WRect[4],double FRect[4],char *logscale)
{
  static integer aaint[]={2,10,2,10};
  if (nsp_gengine1.get_driver()=='R') store_Ech(WRect,FRect,logscale);
  if (logscale[0]=='l') 
    {
      FRect[0]=log10(FRect[0]);
      FRect[2]=log10(FRect[2]);
    }
  if (logscale[1]=='l') 
    {
      FRect[1]=log10(FRect[1]);
      FRect[3]=log10(FRect[3]);
    }
  set_scale("tttftf",WRect,FRect,aaint,logscale,NULL);
  return(0);
}


/*-------------------------------------------
 * setscale2d 
 * uses WRect,ARect,FRect,logscale to update 
 * current scale (current_scale) 
 *  WRect gives the subwindow to use 
 *  ARect gives the axis rectangle 
 *  FRect gives the bounds 
 *  WRect=[<x-upperleft>,<y-upperleft>,largeur,hauteur]
 *    example WRect=[0,0,1.0,1.0] we use all the window 
 *            WRect=[0.5,0.5,0.5,0.5] we use the down right 
 *            quarter of the window
 *  logscale : gives xy log axis flags 
 *  each argument can be a null pointer if they are 
 *  not to be changed from their current value 
 * 
 *  Each window can have a set of scales : one for each specified 
 *  subwindow. This routine must take care of properly 
 *  switching from one scale to an other.
 *  
 *-------------------------------------------*/

int Nsetscale2d(double WRect[4],double ARect[4],double FRect[4],char *logscale)
{
  /* if some arguments are null pointer we set them to 
   * the corresponding current_scale value. 
   * this is only important for store_NEch which do not work with null arguments 
   */ 
  /* char flag[] = "tfffff";*/ /* flag to specify which arguments have changed*/
  char flag[7];
  strcpy(flag,"tfffff");
  if ( WRect != NULL) 
    {
      /* a subwindow is specified */
      flag[1]='t';
      if (! same_subwin( WRect, current_scale.subwin_rect))
	{
	  /* we are using a new subwin we keep current state 
	   * in scale list 
	   */
	  set_window_scale(curwin(),&current_scale);
	  /* now we try to extract a previous scale with WRect as subwin 
	   * which becomes the new current_scale if it is found 
	   */
	  if ( get_window_scale(curwin(),WRect) == 0 ) 
	    {
	      /* this is the first time WRect is used : we reset current_scale flag
	       * Note also that if FRect is also specified in this call 
	       * current_scale.flag will be set to 1 bellow 
	       */
	      current_scale.flag = 0;
	    }
	}
    }
  else WRect = current_scale.subwin_rect;
  if ( FRect != NULL) flag[2]='t'; else FRect = current_scale.frect;
  if ( ARect != NULL) flag[5]='t'; else ARect = current_scale.axis;
  if ( logscale != NULL) flag[4] ='t'; else logscale = current_scale.logflag;
  if ( flag[4] == 't' && flag[2] == 't' ) 
    {
      if (logscale[0]=='l') 
	{
	  if ( FRect[0] <= 0 || FRect[2] <= 0 ) 
	    {
	      Scistring("Warning: negative boundaries on x scale with a log scale \n");
	      FRect[0]=1.e-8;FRect[2]=1.e+8;
	    } 
	  FRect[0]=log10(FRect[0]);
	  FRect[2]=log10(FRect[2]);
	}
      if (logscale[1]=='l') 
	{
	  if ( FRect[1] <= 0 || FRect[3] <= 0 ) 
	    {
	      Scistring("Warning: negative boundaries on y scale with a log scale \n");
	      FRect[1]=1.e-8;FRect[3]=1.e+8;
	    } 
	  FRect[1]=log10(FRect[1]);
	  FRect[3]=log10(FRect[3]);
	}
    }
  if (nsp_gengine1.get_driver()=='R') store_NEch(flag,WRect,ARect,FRect,logscale);
  set_scale(flag,WRect,FRect,NULL,logscale,ARect);
  return(0);
}

/* used to send values to Scilab */

int getscale2d(double WRect[4],double FRect[4],char *logscale,double ARect[4])
{
  integer i;
  static double ten=10.0;
  logscale[0] = current_scale.logflag[0];
  logscale[1] = current_scale.logflag[1];
  for ( i=0; i < 4 ; i++) 
    {
      WRect[i]=current_scale.subwin_rect[i];
      FRect[i]=current_scale.frect[i];
      ARect[i]=current_scale.axis[i];
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

/*-------------------------------------------
 * changes selected items in the current scale 
 * flag gives which component must be used for 
 *      upgrading or setting the current scale 
 * flag[0]   : used for window dim upgrade 
 * flag[1:5] : subwin,frame_values,aaint,logflag,axis_values
 * Result: current_scale is changed 
 * Warning : frame_values[i] must be log10(val[i]) 
 *           when using log scales 
 *-------------------------------------------*/

void set_scale(flag,subwin,frame_values,aaint,logflag,axis_values)
     char flag[6];            /* flag[i] = 't' or 'f' */
     double  subwin[4];       /* subwindow specification */
     double  frame_values[4]; /* [xmin,ymin,xmax,ymax] */
     integer aaint[4];        /* [xint,x_subint,y_int,y_subint]*/
     char logflag[2];         /* [xlogflag,ylogflag] */
     double axis_values[4];   /* [mfact_xl, mfact_xr,mfact_yu,mfact_yd]; */
{
  char c;
  char wdim_changed= 'f',subwin_changed='f';
  char frame_values_changed='f',aaint_changed='f';
  char logflag_changed='f';
  char axis_changed = 'f';
  integer wdim[2];
  int i;
  if ( flag[0] == 't'  ) 
    {
      nsp_gengine->xget_windowdim(wdim,wdim+1);
      if ( current_scale.wdim[0] != wdim[0] || current_scale.wdim[1] != wdim[1]) 
	{ 
	  current_scale.wdim[0] = wdim[0]; current_scale.wdim[1] = wdim[1]; 
	  wdim_changed='t';
	}
    }
  if ( flag[1] == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) 
	if ( subwin[i] != current_scale.subwin_rect[i]) { subwin_changed='t' ; break;}
    }
  if ( flag[2] == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) 
	if ( frame_values[i] != current_scale.frect[i]) { frame_values_changed='t' ; break;}
    }
  if ( flag[3] == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) 
	if ( aaint[i] != current_scale.Waaint1[i]) { aaint_changed='t' ; break;}
    }
  if ( flag[4] == 't' ) 
    {
      for (i=0; i < 2 ; i++ ) 
	if ( logflag[i] != current_scale.logflag[i]) { logflag_changed='t' ; break;}
    }
  if ( flag[5] == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) 
	if ( axis_values[i] != current_scale.axis[i]) { axis_changed='t' ; break;}
    }
  if ( axis_changed == 't') 
    {
      for (i=0; i < 4 ; i++ ) current_scale.axis[i] = axis_values[i];
    }

  if ( subwin_changed == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) current_scale.subwin_rect[i] = subwin[i];
    }
  if ( frame_values_changed == 't' ) 
    {
      for (i=0; i < 4 ; i++) current_scale.frect[i]=frame_values[i]; 
      /* the scale is no more a default scale */
      current_scale.flag = 1;
    }
  if ( wdim_changed == 't' || subwin_changed == 't' || frame_values_changed == 't' 
       ||  axis_changed == 't' )
    {
      /* Upgrading constants for 2D transformation */
      double scx,scy,xoff,yoff,val;
      scx =  ((double) current_scale.wdim[0]*current_scale.subwin_rect[2]-current_scale.wdim[0]*current_scale.subwin_rect[2]
	      *(current_scale.axis[0]+current_scale.axis[1]));
      scy =  ((double) current_scale.wdim[1]*current_scale.subwin_rect[3]-current_scale.wdim[1]*current_scale.subwin_rect[3]
	      *(current_scale.axis[2]+current_scale.axis[3]));
      xoff= ((double) current_scale.wdim[0]*current_scale.subwin_rect[2])*(current_scale.axis[0]);
      yoff= ((double) current_scale.wdim[1]*current_scale.subwin_rect[3])*(current_scale.axis[2]);
      
      current_scale.Wxofset1= xoff+current_scale.subwin_rect[0]*((double)current_scale.wdim[0]);
      current_scale.Wyofset1= yoff+current_scale.subwin_rect[1]*((double)current_scale.wdim[1]);
      current_scale.Wscx1   = scx;
      current_scale.Wscy1   = scy;

      val = Abs(current_scale.frect[0]- current_scale.frect[2]);
      current_scale.Wscx1 = (val <=SMDOUBLE) ? current_scale.Wscx1/SMDOUBLE : current_scale.Wscx1/val; 
      val = Abs(current_scale.frect[1]- current_scale.frect[3]);
      current_scale.Wscy1 = (val <=SMDOUBLE) ? current_scale.Wscy1/SMDOUBLE : current_scale.Wscy1/val;

      current_scale.WIRect1[0] = XScale( current_scale.frect[0]);
      current_scale.WIRect1[1] = YScale( current_scale.frect[3]);
      current_scale.WIRect1[2] = Abs(XScale( current_scale.frect[2]) -  XScale( current_scale.frect[0]));
      current_scale.WIRect1[3] = Abs(YScale( current_scale.frect[3]) -  YScale( current_scale.frect[1]));

    }
  if (  aaint_changed== 't' ) 
    {
      for (i=0; i < 4 ; i++) current_scale.Waaint1[i]=aaint[i];
    }
  if ( logflag_changed== 't' ) 
    {
      current_scale.logflag[0]=logflag[0];      current_scale.logflag[1]=logflag[1];
    }
  if (  aaint_changed== 't' || frame_values_changed == 't')
    {
      current_scale.xtics[0] = current_scale.frect[0];
      current_scale.xtics[1] = current_scale.frect[2];
      current_scale.ytics[0] = current_scale.frect[1];
      current_scale.ytics[1] = current_scale.frect[3];
      current_scale.xtics[2] = 0.0;
      current_scale.ytics[2] = 0.0;
      current_scale.xtics[3] = current_scale.Waaint1[1];
      current_scale.ytics[3] = current_scale.Waaint1[3];
    }


  /** current_scale changes are copied int current window scale */
  if ( (c=nsp_gengine1.get_driver()) == 'X' ||  c == 'R' || c == 'I' || c == 'G' || c == 'W' )
    {
      set_window_scale(curwin(),&current_scale);
    }
}

/*--------------------------------------------------------------------
 * Get the current window dimensions.
 *--------------------------------------------------------------------*/

void get_cwindow_dims( int wdims[2])
{
  nsp_gengine->xget_windowdim(wdims,wdims+1);
}

/*--------------------------------------------------------------------
 * use current scale to set the clipping rectangle 
 *--------------------------------------------------------------------*/

void frame_clip_on()
{
  nsp_gengine->xset_clip(current_scale.WIRect1);
}

void frame_clip_off()
{
  nsp_gengine->xset_unclip();
}






