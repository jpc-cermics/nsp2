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
 * 
 * X11 fonts and gdk 
 *--------------------------------------------------------------------------*/

/*
 * Using X11 Fonts
 */

#define FONTNUMBER 7 
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 10

/* FontsList : storing font informations
 *             the font i with size fsiz is stored at 
 *             FontsList_[i][fsiz]->fid
 */

static GdkFont *FontsList_[FONTNUMBER][FONTMAXSIZE];

/* FontInfoTab : information on fonts 
 *  its name and ok is set to one if the font is loaded in the Xserver 
 *  loadfamily is used for font loading 
 */

struct FontInfo { 
  int ok;
  char fname[100];
} FontInfoTab_[FONTNUMBER];

/* Must be of size FONTMAXSIZE */

static char *size_[] = { "08" ,"10","12","14","18","24"};

/*
 * To set the current font id  and size 
 * load the fonts into X11 if necessary 
 */

typedef  struct  {
  char *alias;
  char *name;
}  FontAlias;

static FontAlias fonttab[] ={
  {"CourR", "-adobe-courier-medium-r-normal--*-%s0-*-*-m-*-iso8859-1"},
  {"Symb", "-adobe-symbol-medium-r-normal--*-%s0-*-*-p-*-adobe-fontspecific"},
  {"TimR", "-adobe-times-medium-r-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimI", "-adobe-times-medium-i-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimB", "-adobe-times-bold-r-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimBI", "-adobe-times-bold-i-normal--*-%s0-*-*-p-*-iso8859-1"},
  {(char *) NULL,( char *) NULL}
};

static void xset_font(BCG *Xgc,int fontid, int fontsize)
{ 
  int i,fsiz,fsiz_sca;
  i = Min(FONTNUMBER-1,Max(fontid,0));
  fsiz = Min(FONTMAXSIZE-1,Max(fontsize,0));
  fsiz_sca = fsiz ;/* XXX fontidscale(fsiz); Scale fonts */
  if ( FontInfoTab_[i].ok !=1 ) 
    { 
      if (i != 6 )
	{
	  loadfamily(fonttab[i].alias,&i);
	}
      else 
	{
	  Sciprintf(" The Font Id %d is not affected \n",(int)i);
	  Sciprintf(" use xlfont to set it \n");
	  return;
	}
    }
  Xgc->fontId = i;
  Xgc->fontSize = fsiz;
  Xgc->private->font = FontsList_[i][fsiz_sca];
}

/* To get the  id and size of the current font */

static void  xget_font(BCG *Xgc,int *font)
{
  font[0] = Xgc->fontId ;
  font[1] = Xgc->fontSize ;
}

static void xset_mark(BCG *Xgc,int number, int size)
{ 
  Xgc->CurHardSymb = Max(Min(SYMBOLNUMBER-1,number),0);
  Xgc->CurHardSymbSize  = Max(Min(FONTMAXSIZE-1,size),0);
}

/* To get the current mark id */

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}

/* Load in X11 a font at size  08 10 12 14 18 24 
 * TimR08 TimR10 TimR12 TimR14 TimR18 TimR24 
 * name is a string if it's a string containing the char % 
 *   it's suposed to be a format for a generic font in X11 string style 
 *   ex :  "-adobe-times-bold-i-normal--%s-*-75-75-p-*-iso8859-1"
 *   and the font is loaded at size 8,10,12,14,18,24
 *   else it's supposed to be an alias for a font name
 *   Ex : TimR and we shall try to load TimR08 TimR10 TimR12 TimR14 TimR18 TimR24 
 *   we first look in an internal table and transmits the string 
 *   to X11 
 */

static void loadfamily(char *name, int *j)
{ 
  int i,flag=1 ;
  /* generic name with % */
  if ( strchr(name,'%') != (char *) NULL)
    {
      loadfamily_n(name,j);
      return;
    }
  else 
    {
      /* our table of alias */
      i=0;
      while ( fonttab[i].alias != (char *) NULL)
	{
	  if (strcmp(fonttab[i].alias,name)==0)
	    {
	      loadfamily_n(fonttab[i].name,j);
	      return ;
	    }
	  i++;
	}
      /* Using X11 Table of aliases */
      for ( i = 0; i < FONTMAXSIZE ; i++)
	{
	  char name1[200];
	  sprintf(name1,"%s%s",name,size_[i]);
	  FontsList_[*j][i]=  gdk_font_load(name1);
	  if  (FontsList_[*j][i]== NULL)
	    { 
	      flag=0;
	      Sciprintf("\n Unknown font : %s",name1);
	      Sciprintf("\n I'll use font: fixed ");
	      FontsList_[*j][i]=  gdk_font_load(name1);
	      if  (FontsList_[*j][i]== NULL)
		{
		  Sciprintf("\n Unknown font : %s\n","fixed");
		  Sciprintf("Please call an X Wizard !");
		}
	    }
	}
      FontInfoTab_[*j].ok = 1;
      if (flag != 0) 
	strcpy(FontInfoTab_[*j].fname,name);
      else
	strcpy(FontInfoTab_[*j].fname,"fixed");
    }
}

static char *size_n_[] = { "8" ,"10","12","14","18","24"};

static void loadfamily_n(char *name, int *j)
{ 
  char name1[200];
  int i,flag=1 ;
  for ( i = 0; i < FONTMAXSIZE ; i++)
    {
      sprintf(name1,name,size_n_[i]);
      FontsList_[*j][i]=  gdk_font_load(name1);
      if  (FontsList_[*j][i]== NULL)
	{ 
	  flag=0;
	  Sciprintf("\n Unknown font : %s",name1);
	  Sciprintf("\n I'll use font: fixed ");
	  FontsList_[*j][i]= gdk_font_load(name1);
	  if  (FontsList_[*j][i]== NULL)
	    {
	      Sciprintf("\n Unknown font : %s\n","fixed");
	      Sciprintf("  Please call an X Wizard !");
	    }
	}
    }
  FontInfoTab_[*j].ok = 1;
  if (flag != 0) 
    strcpy(FontInfoTab_[*j].fname,name);
  else
    strcpy(FontInfoTab_[*j].fname,"fixed");
}

static void queryfamily(char *name, int *j,int *v3)
{ 
  int i ;
  name[0]='\0';
  for (i=0;i<FONTNUMBER;i++) {
    v3[i]=strlen(FontInfoTab_[i].fname);
    if (v3[i] > 0)
      strcat(name,FontInfoTab_[i].fname);
    else
      if (i < 6) {
	v3[i]=strlen(fonttab[i].name);
	strcat(name,fonttab[i].name);
      }
  }
  *j=FONTNUMBER;
}



static void nsp_fonts_finalize(BCG *Xgc)
{
  /* nothing to do */
}

static void nsp_fonts_initialize(BCG *Xgc)
{
  static int first=0;
  if ( first == 0) 
    {
      int fnum;
      loadfamily("CourR",(fnum=0,&fnum));
      LoadSymbFonts();
      loadfamily("TimR",(fnum=2,&fnum));
      /*  the next fonts are loaded when needed  
       *  See xsetfont
       *   loadfamily("TimI",(fnum=3,&fnum));
       *   loadfamily("TimB",(fnum=4,&fnum));
       *   loadfamily("TimBI",(fnum=5,&fnum));
       */
    }
}

/*
 *  We use the Symbol font  for mark plotting
 *  thus we must be able to center a Symbol character at a specified point. 
 *  
 */

typedef  struct { int xoffset[SYMBOLNUMBER];
  int yoffset[SYMBOLNUMBER];} Offset ;

static Offset ListOffset_[FONTMAXSIZE];
static char Marks[] = {
  /*., +,X,*,diamond(filled),diamond,triangle up,triangle down,trefle,circle*/
  (char)0x2e,(char)0x2b,(char)0xb4,(char)0xc5,(char)0xa8,
  (char)0xe0,(char)0x44,(char)0xd1,(char)0xa7,(char)0x4f};

static void LoadSymbFonts(void)
{ 
  int j, i;
  /* Symbol Font is loaded under Id : 1 */
  loadfamily("Symb",(i=1,&i));

  /* We compute the char offset for several chars of the symbol font
   *  in order to be able to center them on a specific point 
   *  we need one offset per symbol
   *  for the font i 
   *  n1=FontsList_[i]->min_char_or_byte2
   *  info on char coded as  oxyy are stored in 
   *  FontsList_[i]->per_char[(char)0xyy-n1]
   *  
   */
  /* if symbol font was not found me must stop */
  if (strcmp(FontInfoTab_[1].fname,fonttab[1].name) != 0) return;
  for (i =0 ; i < FONTMAXSIZE ; i++)
    {    
      if (FontsList_[1][i] != NULL)
	{
	  for (j=0 ; j < SYMBOLNUMBER ; j++)
	    { 
	      gint lbearing, rbearing, iascent, idescent, iwidth;
	      gchar tmp[2] = { (gchar) Marks[j],0};
	      gdk_string_extents(FontsList_[1][i], tmp,
				 &lbearing, &rbearing,
				 &iwidth, &iascent, &idescent);
	      (ListOffset_[i].xoffset)[j] = (rbearing+lbearing)/2;/* ou iwidth/2 ? */
	      (ListOffset_[i].yoffset)[j] = (iascent+idescent)/2;
	    }
	}
    }
}

/*
 * The two next functions send the x and y offsets to center the current
 * symbol at point (x,y) 
 */

static int CurSymbXOffset(BCG *Xgc)
{
  return(-(ListOffset_[Xgc->CurHardSymbSize].xoffset)[Xgc->CurHardSymb]);
}

static int CurSymbYOffset(BCG *Xgc)
{
  return((ListOffset_[Xgc->CurHardSymbSize].yoffset)[Xgc->CurHardSymb]);
}


/* utilities 
 */

 
/*  gdk_draw_text_rot: is comming from R 
 *  ------------------------------------------------------------------
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998-1999   Lyndon Drake
 *                            and the R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

void gdk_draw_text_rot(GdkDrawable *drawable,
		       GdkFont *font,
		       GdkGC *gc,
		       int x, int y,
		       int maxx, int maxy,
		       const gchar *text,
		       gint text_length,
		       double angle)
{
  GdkColor black, white;
  GdkPixmap *pixmap;
  GdkGC *rotgc;
  GdkImage *image;

  int lbearing, rbearing, width, ascent, descent, height;
  int dx, dy;
  int i, j, mini, minj, maxi, maxj;

  double sintheta, costheta;

  /* sanity check */
  if((text == NULL) || (*text == '\0'))
    return;

  /* shortcut horizontal text */
  if(angle == 0.0) {
    gdk_draw_text(drawable, font, gc, x, y, text, text_length);
  }
  else {
    /* text metrics */
    gdk_text_extents(font, text, text_length,
		     &lbearing, &rbearing,
		     &width, &ascent, &descent);
	
    height = ascent + descent;
	
    /* draw text into pixmap */
    pixmap = gdk_pixmap_new(drawable, width, height, 1);
    rotgc = gdk_gc_new(pixmap);
    gdk_gc_set_font(rotgc, font);

    white.pixel = gdk_rgb_xpixel_from_rgb(0xffffffff);
    black.pixel = gdk_rgb_xpixel_from_rgb(0);

    gdk_gc_set_foreground(rotgc, &white);
    gdk_draw_rectangle (pixmap, rotgc, 1, 0, 0, width, height);

    gdk_gc_set_foreground(rotgc, &black);
    gdk_draw_text(pixmap, font, rotgc, 0, ascent, text, text_length);
    image = gdk_image_get(pixmap, 0, 0, width, height); 

    /* precalc cos/sin of angle */
    /* the floor(x * 1000.0 + 0.5) / 1000.0 is a hack to round things off */
    costheta = floor(cos(angle) * 1000.0 + 0.5) / 1000.0;
    sintheta = floor(sin(angle) * 1000.0 + 0.5) / 1000.0;

    /* calculate bounding box for i and j iteration */
    mini = maxi = floor((double)(0 - ascent) * sintheta) + x;
    minj = maxj = floor((double)(0 - ascent) * costheta) + y;

    i = floor((double)width * costheta + (double)(height - ascent) * sintheta) + x;
    j = floor(- (double)width * sintheta + (double)(height - ascent) * costheta) + y;
    if(i < mini) mini = i;
    if(i > maxi) maxi = i;
    if(j < minj) minj = j;
    if(j > maxj) maxj = j;

    i = floor((double)(height - ascent) * sintheta) + x;
    j = floor((double)(height - ascent) * costheta) + y;
    if(i < mini) mini = i;
    if(i > maxi) maxi = i;
    if(j < minj) minj = j;
    if(j > maxj) maxj = j;

    i = floor((double)width * costheta + (double)(0 - ascent) * sintheta) + x;
    j = floor(- (double)width * sintheta + (double)(0 - ascent) * costheta) + y;
    if(i < mini) mini = i;
    if(i > maxi) maxi = i;
    if(j < minj) minj = j;
    if(j > maxj) maxj = j;

    maxi++; maxj++;

    if(mini < 0) mini = 0;
    /* jpc : if(maxi > maxx) maxi = maxx; */
    if(minj < 0) minj = 0;
    /* if(maxj > maxy) maxj = maxy; */

    /* copy pixels */
    for(j = minj; j < maxj; j++) {
      for(i = mini; i < maxi; i++) {
	dx = floor((double)(i - x) * costheta - (double)(j - y) * sintheta);
	dy = floor((double)(i - x) * sintheta + (double)(j - y) * costheta) + ascent;
		
	if((dx >= 0) && (dx < width) && (dy >= 0) && (dy < height) &&
	   (gdk_image_get_pixel(image, dx, dy) == black.pixel)) {
	  gdk_draw_point(drawable, gc, i, j);
	}
      }
    }

    /* clean up */
    gdk_pixmap_unref(pixmap);
    gdk_gc_unref(rotgc);
  }
}


