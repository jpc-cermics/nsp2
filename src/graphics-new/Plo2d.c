/* Nsp
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/nsp.h>
#include <gtk/gtk.h>
#include <nsp/figure.h>
#include <nsp/axes.h>
#include <nsp/curve.h>
#include <nsp/math.h>
#include <nsp/graphics-new/Graphics.h>

static void nsp_legends_box(BCG *Xgc,int n1,
			    const int *mark,const int *mark_size,
			    const int *mark_color,const int *width,const int *color,
			    char **legends,int box[4],
			    int get_box,double xoffset,double yoffset,int c_color,int fg,const char *sep);

/*----------------------------------------------------
 *  legend="leg1@leg2@leg3@...."
 *  legend contain legends separated by '@'
 *  if nlegend is the number of legends stored in legend
 *  then the function Legends draw  Min(*n1,6,nlegends) legends
 *-----------------------------------------------------*/

void nsp_legends(BCG *Xgc,legends_pos pos,int n1,
		 const int *mark,const int *mark_size, const int *mark_color,const int *width,const int *color,
		 char **legend,const char *sep)
{
  int fg,old_dash,pat;
  int rect[4],box[4],xx=0,yy=0;
  double xoffset,yoffset;
  Xgc->graphic_engine->boundingbox(Xgc,"pl",xx,yy,rect);

  fg = Xgc->graphic_engine->xget_foreground(Xgc);
  old_dash = Xgc->graphic_engine->xset_dash(Xgc,1);
  pat = Xgc->graphic_engine->xset_pattern(Xgc,fg);

  /* length for the tick zone associated to the legend */
  xoffset= (Xgc->scales->Irect.width)/20.0;
  /* y offset between legends */
  yoffset= rect[3];
  box[0]=box[1]=0;
  nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,TRUE,xoffset,yoffset,pat,fg,sep);

  switch (pos)
    {
    case legend_ur:
      /* upper right position of the legend box */
      box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
      box[1] = Xgc->scales->Irect.y;
      box[0] -= box[2]+rect[2]/2+ xoffset/2.0;
      box[1] += xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,pat,fg,sep);
      break;
    case legend_urm:
      /* upper right margin */
      box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
      box[1] = Xgc->scales->Irect.y;
      box[0] += xoffset/2.0;
      box[1] += xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,pat,fg,sep);
      break;
    case  legend_ul:
      /* upper left position  of the legend box */
      box[0] = Xgc->scales->Irect.x;
      box[1] = Xgc->scales->Irect.y;
      box[0] += xoffset/2.0;
      box[1] += xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,pat,fg,sep);
      break;
    case legend_dr :
      /* down right position  of the legend box */
      box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
      box[1] = Xgc->scales->Irect.y + Xgc->scales->Irect.height;;
      box[0] -= box[2]+rect[2]/2+ xoffset/2.0;
      box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,pat,fg,sep);
      break;
    case legend_drm:
      /* down right margin */
      box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
      box[1] = Xgc->scales->Irect.y + Xgc->scales->Irect.height;;
      box[0] += xoffset/2.0;
      box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,pat,fg,sep);
      break;
    case legend_dl:
      /* down left  position  of the legend box */
      box[0] = Xgc->scales->Irect.x;
      box[1] = Xgc->scales->Irect.y  + Xgc->scales->Irect.height;;
      box[0] += xoffset/2.0;
      box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,pat,fg,sep);
      break;
    default:
      Sciprintf("Error: unknow position\n");
      break;
    }
  Xgc->graphic_engine->xset_dash(Xgc,old_dash);
}

/*
 * draw the legends and the box
 * or just compute the bounding box according to get_box parameter
 */

static void nsp_legends_box(BCG *Xgc,int n1,
			    const int *mark,const int *mark_size,
			    const int *mark_color,const int *width,const int *color,
			    char **legends,int box[4],
			    int get_box,double xoffset,double yoffset,int c_color,int fg,const char *sep)
{
  int c_width =  Xgc->graphic_engine->xget_thickness(Xgc);
  int bg= Xgc->graphic_engine->xget_background(Xgc);
  int xmark[2]={-1,-1};
  int i,xs,ys,flag=0,polyx[2],polyy[2],lstyle[1],rect[4],n1count=0;
  double angle=0.0;
  int xi= 1.4*xoffset;
  int yi= box[1]+ yoffset*(1.25);
  /* get current mark */
  Xgc->graphic_engine->xget_mark(Xgc,xmark);

  /* fill the background */
  if ( get_box == FALSE )
    {
      int nbox[4];
      memcpy(nbox,box,4*sizeof(int));
      Xgc->graphic_engine->boundingbox(Xgc,"pl",0,0,rect);
      nbox[0] -= rect[2]/2;
      nbox[1] -= rect[3]/2;
      nbox[2] += rect[2];
      nbox[3] += rect[3];
      Xgc->graphic_engine->xset_pattern(Xgc,bg);
      Xgc->graphic_engine->fillrectangle(Xgc,nbox);
      Xgc->graphic_engine->xset_pattern(Xgc,c_color);
    }

  if ( get_box == TRUE )
    {
      box[2]= xi;
    }

  for ( i = 0 ; i < n1 ; i++)
    {
      char *legend;
      xs=inint(box[0]+xi);
      ys=inint(yi);
      legend = legends[i];
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
	      Xgc->graphic_engine->displaystring(Xgc,legend,xs,ys,flag,angle
						 ,GR_STR_XLEFT, GR_STR_YBOTTOM);
	      Xgc->graphic_engine->xset_pattern(Xgc,c_color);
	      Xgc->graphic_engine->boundingbox(Xgc,legend,xs,ys,rect);

	      if ( width[i] >= 0 ) Xgc->graphic_engine->xset_thickness(Xgc,width[i]);

	      if ( color[i] != -2 )
		{
		  /* we neeed to draw a line */
		  int n=1,p=2;
		  polyx[0]=inint(box[0]);polyx[1]=inint(box[0]+xoffset);
		  polyy[0]=inint(yi - rect[3]/2.0);
		  polyy[1]=polyy[0];
		  lstyle[0]=color[i];
		  Xgc->graphic_engine->drawpolylines(Xgc,polyx,polyy,lstyle,n,p);
		}
	      if ( mark[i] >= -1 )
		{
		  /* we need a mark */
		  int n=1,p=1;
		  polyx[0]=inint(box[0]+xoffset);
		  polyy[0]=inint(yi- rect[3]/2);
		  lstyle[0]= - mark[i];
		  if ( mark_size[i] >= 0 ) Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark_size[i]);
		  if ( mark_color[i] >= 0) Xgc->graphic_engine->xset_pattern(Xgc, mark_color[i] );
		  Xgc->graphic_engine->drawpolylines(Xgc,polyx,polyy,lstyle,n,p);
		  if ( mark_color[i] >= 0) Xgc->graphic_engine->xset_pattern(Xgc, c_color);
		  if ( mark_size[i] >= 0 ) Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark[1]);
		}
	      if ( width[i] >= 0 ) Xgc->graphic_engine->xset_thickness(Xgc, c_width);
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
