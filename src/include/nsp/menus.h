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
#include <nsp/gtk/gtkcombobox.h>

#if defined(__MWERKS__)||defined(THINK_C)
#define Widget int
#define TRUE 1
#define FALSE 0
#endif

/* choose */

extern int nsp_choose(NspSMatrix *Items,NspSMatrix *Title,NspSMatrix *button,int *nrep);
extern int nsp_choose_(char *choose_title,char **Items,int nItems,char **but_names, int n_but,int *choice);

/* dialog */

extern int nsp_message(NspSMatrix *Message,NspSMatrix *Buttons,int *rep);
extern int nsp_message_modeless(NspSMatrix *Message,NspSMatrix *Buttons);
extern int nsp_message_(char *message,char **butons,int n_buttons);
extern int nsp_message_modeless_(char *message);

/* mdialog */
/* WARNING: it's not enough to change the following
 * define in order to increase the number of possible items 
 */

#define NPAGESMAX 10000
#define NITEMMAXPAGE 3000

/* choices */

extern int nsp_choices(char *label, char **items, int *defval, int nitems);
extern int nsp_choices_with_combobox(char *title,NspList *L);

/* print */

extern int nsp_print_dialog(int  *flag,char *printer, int  *colored, int  *orientation, char *file, int  *ok);
extern int nsp_menu_export (int *colored,int *orient,char **choices,int n_choices,int *answer,char **filename);
extern int nsp_menu_print (int *colored,int *orient,char **choices,int n_choices,int *answer);

/* dialog */


extern int nsp_dialog(NspSMatrix *Title,NspSMatrix *Init,NspObject **Rep);
extern int nsp_dialog_(char *Title, char * init_value, char **button_name , int * ierr ,char **dialog_str );

/* file */

extern int  nsp_get_file_window(char *filemask,char **file,char *dirname,int flag,int action,int *ierr,char *title);

/* madialog */

extern int nsp_matrix_dialog(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h, NspSMatrix *Init_matrix,int *cancel);
extern int nsp_multi_dialog(NspSMatrix *Title,NspSMatrix *Labels,NspSMatrix  *Init_values, int *cancel);
extern int nsp_multi_dialog_(const char *labels,char **pszTitle, char **pszName, int  nv, int  *ierr);
extern int nsp_matrix_dialog_(char *labels,char **Labels_v,char **Labels_h,char **Init, int nl,int nc, int *ierr);

/* menus */

extern void nsp_menus_set(int win_num,const char *button_name,int ne);
extern void nsp_menus_unset(int win_num,const char *button_name,int ne);
extern int nsp_menus_delete_button (int win_num,const char *button_name); 
extern int nsp_menus_add(int win_num,const char * button_name,char ** entries,int ne,int typ,char *fname);

/* men_combo_color */

extern GtkWidget *nsp_gtkcombobox_colormap_new( BCG *Xgc,int init_color);
extern int gtkcombobox_select_color(BCG *Xgc,int init_color) ;


#endif 

