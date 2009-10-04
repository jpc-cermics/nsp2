/**sciSetScrollInfo
 *@description: Sets the dimension of the scroll bars
 * Do not call SetScrollInfo windows function,
 * sciSetScrollInfo do that and more things !
 *@input: BCG *Scilabgc, int sb_ctl, SCROLLINFO *si, BOOLEAN bRedraw
 *@output: int
 *@author: Matthieu PHILIPPE 
 *@date: Dec 1999
 **/
extern int sciSetScrollInfo(BCG *Scilabgc, int sb_ctl, 
				SCROLLINFO *si, BOOLEAN bRedraw);

/**sciGetScrollInfo
 *@description: Returns the dimension of the scroll bars
 * Do not call GetScrollInfo windows function,
 * sciGetScrollInfo do that and more things !
 *@input: BCG *Scilabgc, int sb_ctl, SCROLLINFO *si
 *@output: int
 *@author: Matthieu PHILIPPE 
 *@date: Dec 1999
 **/
extern int sciGetScrollInfo(BCG *Scilabgc, int sb_ctl, SCROLLINFO *si);

/**sciGetScrollInfo
 *@description: Returns the wresize status.
 * 0: it's in scroll bars mode
 * 1: it's in wresize mode
 *@input: NONE
 *@output: int
 *@author: Matthieu PHILIPPE 
 *@date: Dec 1999
 **/
extern int sciGetwresize();

/**sciGetPixmapStatus
 *@description: Returns the pixmap status.
 * 0: it's drawn directly on screen
 * 1: it's drawn by a pixmap first
 *@input: NONE
 *@output: int
 *@author: Matthieu PHILIPPE 
 *@date: Dec 1999
 **/
extern int sciGetPixmapStatus();

/**SciViewportGet
 *@description: used to get panner position through scilab command.
 *
 *@input: BCG *ScilabGC : structure associated to a Scilab Graphic window
 *        int x,y : the x,y point of the graphic window to be moved at 
 *        the up-left position of the viewport
 *
 *@output: NONE
 *
 *@author: 
 *@date:
 **/
extern void SciViewportGet (BCG *ScilabXgc,int *x,int *y);

/**SciViewportMove
 *@description: used to move the panner and the viewport interactively 
 *              through scilab command.
 *
 *@input: BCG *ScilabGC : structure associated to a Scilab Graphic window
 *        int x,y : the x,y point of the graphic window to be moved at 
 *        the up-left position of the viewport
 *
 *@output: NONE
 *
 *@author: 
 *@date:
 **/
extern void SciViewportMove (BCG *ScilabXgc,int x,int y);

/**GPopupResize
 *@description: a little beat different to windowdim. GPopupResize sets the visible
 * window (parents dimension)
 *
 *@input: BCG *ScilabXgc,int *x,int *y , where x,y are
 * the new dimension
 *
 *@output: NONE
 *
 *@see: setwindowdim
 *
 *@author: 
 *@date: 
 **/
extern void GPopupResize (BCG *ScilabXgc,int *x,int *y);
