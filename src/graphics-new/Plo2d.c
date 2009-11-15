/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 *--------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include <math.h>
#include <gtk/gtk.h>
#include <nsp/figure.h> 
#include <nsp/axes.h> 
#include <nsp/curve.h> 
#include "nsp/math.h"
#include "nsp/graphics-new/Graphics.h"


/*----------------------------------------------------
 *  legend="leg1@leg2@leg3@...."             
 *  legend contain legends separated by '@'
 *  if nlegend is the number of legends stored in legend
 *  then the function Legends draw  Min(*n1,6,nlegends) legends
 *-----------------------------------------------------*/

static void nsp_legends_box(BCG *Xgc,int n1,const int *style,char * legend,int box[4],int get_box,
			    double xoffset,double yoffset,int pat,int fg,const char *sep);

void nsp_legends(BCG *Xgc,legends_pos pos,int n1,const int *style,const char * legend,const char *sep)
{
  int rect[4],box[4],xx=0,yy=0;
  char *loc;
  double xoffset,yoffset;  
  loc=(char *) MALLOC( (strlen(legend)+1)*sizeof(char));

  Xgc->graphic_engine->boundingbox(Xgc,"pl",xx,yy,rect);

  if ( loc != 0)
    {
      int fg,old_dash,pat;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      old_dash = Xgc->graphic_engine->xset_dash(Xgc,1);
      pat = Xgc->graphic_engine->xset_pattern(Xgc,fg);

      strcpy(loc,legend);

      /* length for the tick zone associated to the legend */
      xoffset= (Xgc->scales->Irect.width)/20.0;
      /* y offset between legends */
      yoffset= rect[3];
      box[0]=box[1]=0;
      nsp_legends_box(Xgc,n1,style,loc,box,TRUE,xoffset,yoffset,pat,fg,sep);
      strcpy(loc,legend);

      switch (pos) 
	{
	case legend_ur: 
	  /* upper right position of the legend box */
	  box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
	  box[1] = Xgc->scales->Irect.y;
	  box[0] -= box[2]+rect[2]/2+ xoffset/2.0;
	  box[1] += xoffset/2.0;
	  nsp_legends_box(Xgc,n1,style,loc,box,FALSE,xoffset,yoffset,pat,fg,sep);
	  break;
	case legend_urm:
	  /* upper right margin */
	  box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
	  box[1] = Xgc->scales->Irect.y;
	  box[0] += xoffset/2.0;
	  box[1] += xoffset/2.0;
	  nsp_legends_box(Xgc,n1,style,loc,box,FALSE,xoffset,yoffset,pat,fg,sep);
	  break;
	case  legend_ul:
	  /* upper left position  of the legend box */
	  box[0] = Xgc->scales->Irect.x;
	  box[1] = Xgc->scales->Irect.y;
	  box[0] += xoffset/2.0;
	  box[1] += xoffset/2.0;
	  nsp_legends_box(Xgc,n1,style,loc,box,FALSE,xoffset,yoffset,pat,fg,sep);
	  break;
	case legend_dr :
	  /* down right position  of the legend box */
	  box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
	  box[1] = Xgc->scales->Irect.y + Xgc->scales->Irect.height;;
	  box[0] -= box[2]+rect[2]/2+ xoffset/2.0;
	  box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
	  nsp_legends_box(Xgc,n1,style,loc,box,FALSE,xoffset,yoffset,pat,fg,sep);
	  break;
	case legend_drm:
	  /* down right margin */
	  box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
	  box[1] = Xgc->scales->Irect.y + Xgc->scales->Irect.height;;
	  box[0] += xoffset/2.0;
	  box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
	  nsp_legends_box(Xgc,n1,style,loc,box,FALSE,xoffset,yoffset,pat,fg,sep);
	  break;
	case legend_dl: 
	  /* down left  position  of the legend box */
	  box[0] = Xgc->scales->Irect.x;
	  box[1] = Xgc->scales->Irect.y  + Xgc->scales->Irect.height;;
	  box[0] += xoffset/2.0;
	  box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
	  nsp_legends_box(Xgc,n1,style,loc,box,FALSE,xoffset,yoffset,pat,fg,sep);
	  break;
	default:
	  Sciprintf("Error: unknow position\n");
	  break;
	}
      FREE(loc);
      Xgc->graphic_engine->xset_dash(Xgc,old_dash);
    }
  else
    {
      Scistring("Legends : running out of memory to store legends\n");
    }
}

/* 
 * draw or compute the bounding box 
 */

static void nsp_legends_box(BCG *Xgc,int n1,const int *style, char * legend,int box[4],
			    int get_box,double xoffset,double yoffset,int pat,int fg,const char *sep)
{
  int i,xs,ys,flag=0,polyx[2],polyy[2],lstyle[1],rect[4],n1count=0;
  double angle=0.0,yi,xi;
  xi= 1.4*xoffset;
  yi= box[1]+ yoffset*(1.25);
  if ( get_box == TRUE )
    {
      box[2]= xi;
    }
  for ( i = 0 ; i < n1 ; i++)
    {  
      xs=inint(box[0]+xi);
      ys=inint(yi);
      if ( i==0) legend=strtok(legend,sep); else legend=strtok((char *)0,sep);
      if (legend != 0 && strcmp(legend,"") != 0 ) 
	{
	  n1count++;
	  if ( get_box == TRUE )
	    {
	      Xgc->graphic_engine->boundingbox(Xgc,legend,xs,ys,rect);
	      box[2]= Max(box[2],xi+rect[2]);
	    }
	  else
	    {
	      Xgc->graphic_engine->xset_pattern(Xgc,fg);
	      Xgc->graphic_engine->displaystring(Xgc,legend,xs,ys,flag,angle);
	      Xgc->graphic_engine->xset_pattern(Xgc,pat);
	      if (style[i] > 0)
		{ 
		  int n=1,p=2;
		  polyx[0]=inint(box[0]);polyx[1]=inint(box[0]+xoffset);
		  polyy[0]=polyy[1]=inint(yi - rect[3]/2.0);
		  lstyle[0]=style[i];
		  Xgc->graphic_engine->drawpolylines(Xgc,polyx,polyy,lstyle,n,p);
		}
	      else
		{ 
		  /* FIXME: should be affected by marksize */
		  int n=1,p=1;
		  polyx[0]=inint(box[0]+xoffset);
		  polyy[0]=inint(yi- rect[3]/2);
		  lstyle[0]=style[i];
		  Xgc->graphic_engine->drawpolylines(Xgc,polyx,polyy,lstyle,n,p);
		}
	    }
	}
      yi += yoffset*(1.5);
    }
  if ( get_box == TRUE )
    {
      box[3]= n1count*1.5*yoffset;
    }
  else 
    {
      /* enlarged box which is drawn  */
      Xgc->graphic_engine->boundingbox(Xgc,"pl",0,0,rect);
      box[0] -= rect[2]/2; 
      box[1] -= rect[3]/2; 
      box[2] += rect[2]; 
      box[3] += rect[3];
      Xgc->graphic_engine->drawrectangle(Xgc,box);
    }
}


