#ifndef MEN_SCILAB 
#define MEN_SCILAB 

/* Copyright ENPC */

#include <stdio.h>
#include "nsp/machine.h"
#include "nsp/Math.h"
#include "graphics/Graphics.h"
#include "../system/men_Sutils.h"

#if defined(__MWERKS__)||defined(THINK_C)
#define Widget int
#define TRUE 1
#define FALSE 0
#else
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/List.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Toggle.h>
#endif

#include <string.h>

/* used only for message and dialog boxes */

/* choose */

typedef struct {
  char *description;    /** Title **/
  char **strings;       /** items **/
  int nstrings;         /** number of items **/
  char **buttonname;    /** buttons **/
  int nb;               /** number of buttons **/
  int choice;           /** number of selected item **/
}  ChooseMenu ;

/* dialog */

extern char *dialog_str;

typedef struct {
  char *description;      /** Title **/
  char *init;              /** initial value **/
  char **pButName;         /** buttons **/
  int  nb;                /** number of buttons **/
  int  ierr;
}  SciDialog ;


/* Data structure to deal with a set of choices */

typedef struct {
  char *name;
  char *cbinfo ; 
  Widget toggle;
} SciData;

typedef struct {
  struct {
    char *name;
    char *text;
    int   num_toggles;
    int   columns;
    int  (*function)();
    int  default_toggle; /* and is set dynamically to the selected value */
    Widget label;
  } choice;
  SciData *data;
} SciStuff;

extern SciStuff **Everything ;

/* Data structure to deal with message */

typedef struct {
  char *string;            /** texte  **/
  char **pButName;         /** buttons **/
  int  nb;                /** number of buttons **/
  int  ierr;
}  SciMess ;

/* Data structure to deal with mdialog */

/* WARNING: it's not enough to change the following
 * define in order to increase the number of possible items 
 */

#define NPAGESMAX 10000
#define NITEMMAXPAGE 3000

typedef struct {
  char *labels;           /** Title **/
  char **pszTitle;        /** items **/
  char **pszName;         /** buttons **/
  int  nv;                /** number of items **/
  int  ierr;
}  MDialog ;


/** Data structure for MatDialog */

typedef struct {
  char *labels;           /** Title **/
  char **VDesc;           /* Vertical labels */
  char **HDesc;           /* Horizontal lables */
  char **data ;           /* values */
  int  nv;                /** number of items **/
  int  nl,nc;
  int  ierr;
}  MADialog ;

/** Data structure for printDialog **/

typedef struct {
  int numChoice ;
  char *filename ;
  char **PList ;
  char *Pbuffer ;
  int ns;
} PrintDial;


/* "men_choice-n.c.X1" */

extern void C2F(xchoices) (int *,int *,int *,int *,int *,int *,int *,int *,int *);  
extern int TestChoice  (void);  
extern int SciChoice  (char *, char **, int *, int );  

/* "men_choose-n.c.X1" */

extern int TestChoose  (void);  
extern void C2F(xchoose) (int *, int *, int *, int *, int *, int *, int *, int *, int *, int *, int *);  

/* "men_dialog-n.c.X1" */

extern int TestDialog  (void);  
extern void C2F(xdialg) (int *value, int *ptrv, int *nv, int *, int *ptrdesc, int *nd, int *btn, int *ptrbtn, int *nb, int *res, int *ptrres, int *nr, int *ierr);  
extern void xdialg1  (char *, char *valueinit, char **pButName, char *value, int *ok);  

/* "men_getfile-n.c.X1" */

extern int TestGetFile  (void);  
extern void C2F(xgetfile) (char *filemask, char *dirname, char **res, integer *ires, integer *ierr, integer *idir,integer *desc,integer *ptrdesc,integer *nd);

/* "men_madial-n.c.X1" */

extern void C2F(xmatdg) (int *, int *ptrlab, int *nlab, int *value, int *ptrv, int *v, int *ptrdescv, int *h, int *ptrdesch, int *nl, int *nc, int *res, int *ptrres, int *ierr);  
extern int TestMatrixDialogWindow  (void);  

/* "men_mdial-n.c.X1" */

extern int TestmDialogWindow  (void);  
extern void C2F(xmdial) (int *, int *ptrlab, int *nlab, int *value, int *ptrv, int *, int *ptrdesc, int *nv, int *res, int *ptrres, int *ierr);  

/* "men_message-n.c.X1" */

extern int TestMessage  (int n);  
extern void C2F(xmsg) (int *basstrings, int *ptrstrings, int *nstring, int *btn, int *ptrbtn, int *nb, int *nrep, int *ierr);  
extern void C2F(xmsg1) (int *basstrings, int *ptrstrings, int *nstring, int *btn, int *ptrbtn, int *nb, int *ierr);  

/* "men_print-n.c.X1" */

extern int prtdlg  (integer *flag, char *printer, integer *colored, integer *orientation, char *file, integer *ok);  
extern int TestPrintDlg  (void);  
extern int SetPrinterList  (int);  

/* "xmen_Utils-n.c.X1" */

extern void XtMyLoop  (Widget , Display *, int, int *);  
extern void ShellFormCreate  (char *, Widget *, Widget *, Display **);  
extern int ButtonCreate  (Widget, Widget *, XtCallbackProc, XtPointer, char *, char *);  
extern int ViewpLabelCreate  (Widget, Widget *, Widget *, char *);  
extern int ViewpListCreate  (Widget, Widget *, Widget *, char **, int);  
extern int LabelSize  (Widget, int, int , Dimension *, Dimension *);  
extern int AsciiSize  (Widget, int, int , Dimension *, Dimension *);  
extern int SetLabel  (Widget, char *, Dimension , Dimension );  
extern int SetAscii  (Widget, char *, Dimension , Dimension );  

/* "xmen_choice-n.c.X1" */

extern int SciChoiceI  (char *, int *, int );  
extern int SciChoiceCreate  (char **, int *, int );  
extern int AllocAndCopy  (char **, char *);  
extern int SciChoiceFree  (int );  
extern Widget create_choices  (Widget, Widget,int);  

/* "xmen_choose-n.c.X1" */

extern int ExposeChooseWindow  (ChooseMenu *);  

/* "xmen_dialog-n.c.X1" */

extern int DialogWindow  (void);  

/* "xmen_getfile-n.c.X1" */

extern int GetFileWindow  (char *, char **, char *, int, int *,char *);  
extern int sci_get_file_window (char *, char **, char *, int,int, int *,char *);  
extern void XtSpecialLoop  (void);  
extern void cancel_getfile  (void);  
extern int write_getfile  (char *, char *);  
extern int popup_file_panel1  (Widget,char* );  

/* "xmen_madial-n.c.X1" */

extern int MatrixDialogWindow  (void);  

/* "xmen_mdial-n.c.X1" */

extern int mDialogWindow  (void);  

/* "xmen_message-n.c.X1" */

extern int ExposeMessageWindow  (void);  
extern int ExposeMessageWindow1  (void);  

/* "xmen_print-n.c.X1" */

extern void PrintDlgOk  (Widget w, caddr_t , caddr_t );  
extern void SaveDlgOk  (Widget w, caddr_t , caddr_t );  
extern void PrintDlgCancel  (Widget w, caddr_t , caddr_t );  
extern int ExposePrintdialogWindow  (int flag, int *, int *); 


#endif 
