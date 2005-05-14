#ifndef NSP_INC_MENUS 
#define NSP_INC_MENUS 

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <string.h>
#include <stdio.h>
#include "nsp/machine.h"
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/object.h"

#if defined(__MWERKS__)||defined(THINK_C)
#define Widget int
#define TRUE 1
#define FALSE 0
#endif

/* choose */

int nsp_choose(NspSMatrix *Items,NspSMatrix *Title,NspSMatrix *button,int *nrep);
int nsp_choose_(char *choose_title,char **Items,int nItems,char **but_names, int n_but,int *choice);

/* dialog */


int nsp_message(NspSMatrix *Message,NspSMatrix *Buttons,int *rep);
int nsp_message_modeless(NspSMatrix *Message,NspSMatrix *Buttons);
int nsp_message_(char *message,char **butons,int n_buttons);
int nsp_message_modeless_(char *message);

/* mdialog */
/* WARNING: it's not enough to change the following
 * define in order to increase the number of possible items 
 */

#define NPAGESMAX 10000
#define NITEMMAXPAGE 3000

/* choices */

int nsp_choices(char *label, char **items, int *defval, int nitems);

/* print */

int nsp_print_dialog(int  *flag,char *printer, int  *colored, int  *orientation, char *file, int  *ok);
int nsp_menu_export (int *colored,int *orient,char **choices,int n_choices,int *answer,char **filename);
int nsp_menu_print (int *colored,int *orient,char **choices,int n_choices,int *answer);

/* dialog */


int nsp_dialog(NspSMatrix *Title,NspSMatrix *Init,NspObject **Rep);
int nsp_dialog_(char *Title, char * init_value, char **button_name , int * ierr ,char **dialog_str );

/* file */

int  nsp_get_file_window(char *filemask,char **file,char *dirname,int flag,int action,int *ierr,char *title);

/* madialog */

int nsp_matrix_dialog(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h, NspSMatrix *Init_matrix,int *cancel);
int nsp_multi_dialog(NspSMatrix *Title,NspSMatrix *Labels,NspSMatrix  *Init_values, int *cancel);
int nsp_multi_dialog_(const char *labels,char **pszTitle, char **pszName, int  nv, int  *ierr);
int nsp_matrix_dialog_(char *labels,char **Labels_v,char **Labels_h,char **Init, int nl,int nc, int *ierr);

#endif 

