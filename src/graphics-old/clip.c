/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

/*----------------------------------------------------------
 * Clipping functions 
 * to be used to draw a polyline with clipping box 
 * when native clipping do not work. 
 * use nsp_set_clip_box to set the clip box 
 * 
 * FIXME: a remetre ds periFig.c 
 *----------------------------------------------------------*/

#include "nsp/graphics/Graphics.h" 

static void draw_clipped(BCG *Xgc,int j, int *vx, int *vy, int xleft,int xright,int ybot,int ytop);


/* Test a single point to be within the xleft,xright,ybot,ytop bbox.
 * Sets the returned ints 4 l.s.b. as follows:
 * bit 0 if to the left of xleft.
 * bit 1 if to the right of xright.
 * bit 2 if below of ybot.
 * bit 3 if above of ytop.
 * 0 is returned if inside.
 */

static int clip_point(int x, int y,int xleft,int xright,int ybot,int ytop)
{
  int ret_val = 0;

  if (x < xleft) ret_val |= (char)0x01;
  else if (x > xright) ret_val |= (char)0x02;
  if (y < ybot) ret_val |= (char)0x04;
  else if (y > ytop) ret_val |= (char)0x08;
  return ret_val;
}

/* Clip the given line to private->drawing coords defined as xleft,xright,ybot,ytop.
 *   This routine uses the cohen & sutherland bit mapping for fast clipping -
 * see "Principles of Interactive Computer Graphics" Newman & Sproull page 65.
 * return 0  : segment out 
 *       1  : (x1,y1) changed 
 *	2  : (x2,y2) changed 
 *	3  : (x1,y1) and (x2,y2) changed 
 *	4  : segment in 
 */


void clip_line(int x1, int yy1, int x2, int y2, int *x1n, int *yy1n, int *x2n, int *y2n, int *flag, 
	       int xleft,int xright,int ybot,int ytop)
{
  int x, y, dx, dy, x_intr[2], y_intr[2], count, pos1, pos2;
  *x1n=x1;*yy1n=yy1;*x2n=x2;*y2n=y2;*flag=4;
  pos1 = clip_point(x1, yy1,xleft, xright, ybot,ytop);
  pos2 = clip_point(x2, y2,xleft, xright, ybot,ytop);
  if (pos1 || pos2) {
    if (pos1 & pos2) { *flag=0;return;}	  
    /* segment is totally out. */

    /* Here part of the segment MAy be inside. test the intersection
     * of this segment with the 4 boundaries for hopefully 2 intersections
     * in. If non found segment is totaly out.
     */
    count = 0;
    dx = x2 - x1;
    dy = y2 - yy1;

    /* Find intersections with the x parallel bbox lines: */
    if (dy != 0) {
      x = (int) (ybot - y2)  * ((double) dx / (double) dy) + x2;
      /* Test for ybot boundary. */
      if (x >= xleft && x <= xright) {
	x_intr[count] = x;
	y_intr[count++] = ybot;
      }
      x = (ytop - y2) * ((double) dx / (double) dy) + x2; 
      /* Test for ytop boundary. */
      if (x >= xleft && x <= xright) {
	x_intr[count] = x;
	y_intr[count++] = ytop;
      }
    }
    if ( count < 2 ) 
      {
	/* Find intersections with the y parallel bbox lines: */
	if (dx != 0) {
	  y = (xleft - x2) * ((double) dy / (double) dx) + y2;   
	  /* Test for xleft boundary. */
	  if (y >= ybot && y <= ytop) {
	    x_intr[count] = xleft;
	    y_intr[count++] = y;
	  }
	  if ( count < 2 ) 
	    {  
	      y = (xright - x2) * ((double) dy / (double) dx) + y2;  
	      /* Test for xright boundary. */
	      if (y >= ybot && y <= ytop) {
		x_intr[count] = xright;
		y_intr[count++] = y;
	      }
	    }
	}
      }


    if (count == 2) 
      {
	if (pos1 && pos2) {	   /* Both were out - update both */
	  *x1n = x_intr[0];
	  *yy1n = y_intr[0];
	  *x2n = x_intr[1];
	  *y2n = y_intr[1];
	  *flag=3;return;
	}
	else if (pos1) 
	  {       /* Only x1/yy1 was out - update only it */
	    
	    if (dx * (x_intr[1] - x_intr[0]) + dy * (y_intr[1] - y_intr[0]) >= 0) 
	      {
		*x1n = x_intr[0];
		*yy1n = y_intr[0];
		*flag=1;return;
	      }
	    else 
	      {
		*x1n = x_intr[1];
		*yy1n = y_intr[1];
		*flag=1;return;
	      }
	  }
	else 
	  {	         /* Only x2/y2 was out - update only it */
	    if (dx * (x_intr[1] - x_intr[0]) + dy * (y_intr[1] - y_intr[0]) >= 0) 
	      {
		*x2n = x_intr[1];
		*y2n = y_intr[1];
		*flag=2;return;
	      }
	    else 
	      {
		*x2n = x_intr[0];
		*y2n = y_intr[0];
		*flag=2;return;
	      }
	  }
      }
    else 
      {
	/* count != 0 */
	*flag=0;return;
      }
  }
}

/**
 * MyDraw:
 * @Xgc: 
 * @iib: 
 * @iif: 
 * @vx: 
 * @vy: 
 * 
 * draw the polyline from (vx[iib-1],vy[iib-1])->(vx[iif],vy[iif])
 * we know that (vx[iib-1],vy[iib-1]) and (vx[iif],vy[iif]) are out 
 * all the other points are inside. 
 * 
 **/

static void MyDraw(BCG *Xgc,int iib, int iif, int *vx, int *vy, 
		   int xleft,int xright,int ybot,int ytop)
{
  draw_clipped(Xgc,iib,vx,vy ,xleft, xright, ybot,ytop);
  draw_clipped(Xgc,iif,vx,vy,xleft, xright, ybot,ytop); 
  Xgc->graphic_engine->drawpolyline(Xgc,vx + iib,vy +iib,iif-iib,0);
}

/**
 * draw_clipped:
 * @Xgc: 
 * @j: 
 * @vx: 
 * @vy: 
 * 
 * draw the intersection of segment (vx[j-1],vy[j-1])->(vx[j],vy[j]) 
 * with the clip_box;
 * knowing that the origin and destination points are out of the clip box.
 * FIXME: be sure that Xgc->graphic_engine->drawpolyline 
 *        do not call clip_again
 **/

static void draw_clipped(BCG *Xgc,int j, int *vx, int *vy,int xleft,int xright,int ybot,int ytop)
{
  int vxn[2],vyn[2],flag;
  if ( j== 0) return;
  clip_line(vx[j-1],vy[j-1],vx[j],vy[j],&vxn[0],&vyn[0],&vxn[1],&vyn[1],&flag,
	    xleft, xright, ybot,ytop);
  if ( flag != 0) Xgc->graphic_engine->drawpolyline(Xgc,vxn,vyn,2,0);
}

/* 
 *  returns the first (vx[.],vy[.]) point inside 
 *  xleft,xright,ybot,ytop bbox. begining at index ideb
 *  or zero if the whole polyline is out 
 */

static int first_in(int n, int ideb, int *vx, int *vy, int xleft,int xright,int ybot,int ytop)
{
  int i;
  for (i=ideb  ; i < n ; i++)
    {
      if (vx[i]>= xleft && vx[i] <= xright  && vy[i] >= ybot && vy[i] <= ytop)
	{
#ifdef DEBUG
	  Sciprintf("first in %d->%d=(%d,%d)\n",ideb,i,vx[i],vy[i]);
#endif
	  return(i);
	}
    }
  return(-1);
}

/* 
 *  returns the first (vx[.],vy[.]) point outside
 *  xleft,xright,ybot,ytop bbox.
 *  or zero if the whole polyline is out 
 */

static int first_out(int n, int ideb, int *vx, int *vy, int xleft,int xright,int ybot,int ytop)
{
  int i;
  for (i=ideb  ; i < n ; i++)
    {
      if ( vx[i]< xleft || vx[i]> xright  || vy[i] < ybot || vy[i] > ytop) 
	{
#ifdef DEBUG
	  Sciprintf("first out %d->%d=(%d,%d)\n",ideb,i,vx[i],vy[i]);
#endif
	  return(i);
	}
    }
  return(-1);
}

/**
 * nsp_drawpolyline_clip:
 * @Xgc: 
 * @n: 
 * @vx: 
 * @vy: 
 * @onemore: 
 * 
 * can be used to draw a polyline clipped to 
 * be in a rectangle. It uses drawpolyline in the given @Xgc.
 * 
 **/

void nsp_drawpolyline_clip(BCG *Xgc,int *vx, int *vy, int n,int *clip_box,int onemore)
{ 
  int xleft=clip_box[0], xright=clip_box[1], ybot=clip_box[2], ytop=clip_box[3];
  int iib,iif,ideb=0,vxl[2],vyl[2];
#ifdef DEBUG 
  Sciprintf("inside analyze\n");
#endif
  while (1) 
    { 
      int j;
      iib=first_in(n,ideb,vx,vy, xleft, xright, ybot,ytop);
      if (iib == -1) 
	{ 
#ifdef DEBUG
	  Sciprintf("[%d,end=%d] polyline is out\n",(int)ideb,(int)n);
#endif 
	  /* all points are out but segments can cross the box 
	   * we draw each intersected segments in a loop
	   */
	  for (j=ideb+1; j < n; j++) draw_clipped(Xgc,j,vx,vy , xleft, xright, ybot,ytop);
	  break;
	}
      else 
	{
	  if ( iib - ideb > 1) 
	    {
	      /* all points from ideb to iib -1 are out but segments can cross the box 
	       * we draw each intersected segments in a loop
	       */
	      for (j=ideb+1; j < iib; j++) draw_clipped(Xgc,j,vx,vy, xleft, xright, ybot,ytop);
	    };
	}
      iif=first_out(n,iib,vx,vy,  xleft, xright, ybot,ytop);
      if (iif == -1) 
	{
	  /* special case the polyligne is totaly inside */
	  if (iib == 0) 
	    {
	      Xgc->graphic_engine->drawpolyline(Xgc,vx,vy,n,onemore);
	      /* no need to check the las segment here whe can return */
	      return;
	    }
	  else 
	    {
	      /* here the polyline is inside from iib to the end */
	      MyDraw(Xgc,iib,n-1,vx,vy, xleft, xright, ybot,ytop);
	    } 
	  break; 
	}
#ifdef DEBUG
      Sciprintf("Analysed : [%d,%d]\n",(int)iib,(int)iif);
#endif 
      MyDraw(Xgc,iib,iif,vx,vy, xleft, xright, ybot,ytop);
      ideb=iif;
    }
  if (onemore == 1) 
    {
      /* The polyligne is closed we consider the closing segment */
      vxl[0]=vx[n-1];vxl[1]=vx[0];vyl[0]=vy[n-1];vyl[1]=vy[0];
      draw_clipped(Xgc,0,vxl,vyl, xleft, xright, ybot,ytop);
    }
}

