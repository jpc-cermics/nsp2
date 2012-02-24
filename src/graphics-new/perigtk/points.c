/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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
 * Gtk driver 
 *--------------------------------------------------------------------------*/

/*
 * malloc of an arraw of GdkPoint
 */

static GdkPoint *gtk_points = NULL;

static GdkPoint *gtk_get_xpoints(void) { return(gtk_points); }

static int gtk_store_points(int n, int *vx, int *vy, int onemore)
{ 
  int i,n1 = ( onemore == 1) ? n+1 : n;
  if (GtkReallocVector(n1) == 1)
    {
      for (i = 0; i < n; i++){
	gtk_points[i].x =(gint16) Min(Max(-int16max,vx[i]),int16max);
	gtk_points[i].y =(gint16) Min(Max(-int16max,vy[i]),int16max);
      }
      if (onemore == 1) {
	gtk_points[n].x=(gint16) gtk_points[0].x;
	gtk_points[n].y=(gint16) gtk_points[0].y;
      }
      return(1);
    }
  else return(0);
}
#if 1 
static int gtk_store_points_remove_redundent(int n, int *vx, int *vy, int onemore)
{ 
  GdkPoint p;
  int count=0;
  int i,n1 = ( onemore == 1) ? n+1 : n;
  if (GtkReallocVector(n1) == 1)
    {
      for (i = 0; i < n; i++)
	{
	  p.x =(gint16) Min(Max(-int16max,vx[i]),int16max);
	  p.y =(gint16) Min(Max(-int16max,vy[i]),int16max);
	  if ( count > 0) 
	    {
	      if (!( gtk_points[count-1].x == p.x && gtk_points[count-1].y == p.y ))
		{
		  gtk_points[count] = p;
		  count++;
		}
	    }
	  else
	    {
	      gtk_points[count] = p;count++;
	    }
	}
      if (onemore == 1) 
	{
	  gtk_points[count].x=(gint16) gtk_points[0].x;
	  gtk_points[count].y=(gint16) gtk_points[0].y;
	  count++;
	}
      /* Sciprintf("I use %d points out of %d removing redundencies\n",count,n); */
      return count;
    }
  else return -1;
}
#endif 

#define MESSAGE5 "Can't re-allocate point vector" 

static int GtkReallocVector(int n)
{
  if (( gtk_points = graphic_alloc(8,n,sizeof(GdkPoint))) == 0) 
    { 
      Sciprintf(MESSAGE5); return 0;
    }
  return 1;
}
