/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2019 Enpc/Jean-Philippe Chancelier
 *    jpc@cereve.enpc.fr 
 --------------------------------------------------------------------------*/

#ifndef PERIWIN_BCG_OLD  
#define PERIWIN_BCG_OLD  
#include <winuser.h>
#define Window HWND
/* typedef int (*EVTHANDLER) (int win,int x,int y,int ibut);*/

struct BCG 
{ 
  HWND	hWndParent;     /* parent window handle */
  HWND CWindow ;   /** window of the top level widget of the graphic window **/
  HWND Statusbar;    /* status window */
  HDC hdcCompat ;  /** for pixmap version */
  HBITMAP hbmCompat ;  /** for pixmap version */
  MW  lpmw;        /** graphic window menu **/
  int CurWindow ;   /** Id of window **/
  int CWindowWidth ; /** graphic window width **/
  int CWindowHeight ; /** graphic window height **/
  int FontSize ;
  int FontId ;
  /** XID FontXID; **/
  int CurHardSymb;
  int CurHardSymbSize;
  int CurLineWidth;
  int CurPattern;
  int CurColor;
  int CurPixmapStatus;
  int CurResizeStatus;
  int CurVectorStyle;
  int CurDrawFunction;
  int ClipRegionSet;
  int CurClipRegion[4];
  int CurDashStyle;
  char CurNumberDispFormat[20]; 
  int CurColorStatus;
  int IDLastPattern; /* number of last patter or color */
  /** Colormap Cmap; color map of current graphic window */
  int CmapFlag ; /* set to 1 if the Cmap has default colors */
  int Numcolors; /* number of colors */
  /** A reprendre avec des pointeurs comme en X11 XXXXXX **/
  COLORREF *Colors;/* vector of colors */ 
  float *Red; /* red value: between 0 and 1 */
  float *Green; /* green value: between 0 and 1 */

  float *Blue; /* blue value: between 0 and 1 */
  int NumBackground;  /* number of Background */
  int NumForeground; /* number of Foreground in the color table */
  int NumHidden3d;  /* color for hidden 3d facets **/
  char EventHandler[25]; /* name of window event handler */
  /** win95 specific objects **/
  HPEN hPen ;  /** the current pen for graphic window **/
  HBRUSH hBrush; /** the current brush **/
  LPGW   lpgw; /** to store a pointer to the graphwin structure **/
  int Inside_init; /** a flag to inhibit Graproc actions while in 
		     initgraphic **/
  SCROLLINFO vertsi;
  SCROLLINFO horzsi;
  int CWindowWidthView ; /** graphic window width **/
  int CWindowHeightView ; /** graphic window height **/
}  ;


extern BCG *GetWindowXgcNumber  (int i);

#endif /* PERIWIN_BCG  */

