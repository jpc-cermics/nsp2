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

typedef enum { menu_ok , menu_cancel, menu_fail } menu_answer; 

/* choose */

extern int nsp_choose(NspSMatrix *Items,NspSMatrix *Title,NspSMatrix *button,int *nrep);
extern int nsp_choose_(char *choose_title,char **Items,int nItems,char **but_names, int n_but,int *choice);

/* dialog */

extern int nsp_message(NspSMatrix *Message,NspSMatrix *Buttons,int *rep);
extern int nsp_message_modeless(NspSMatrix *Message,NspSMatrix *Buttons);
extern int nsp_message_(char *message,char **butons,int n_buttons);
extern int nsp_message_modeless_(char *message);

/* choices */

extern int nsp_choices(char *label, char **items, int *defval, int nitems);
extern int nsp_choices_with_combobox(char *title,NspList *L,int use_table);

/* print */

int nsp_print_dialog(char **print_command,int *type,int *orientation,int *format);
int nsp_export_dialog(char **file,int *type,int *orientation,int *format);

/* dialog */

extern int nsp_dialog(NspSMatrix *Title,NspSMatrix *Init,NspObject **Rep);
extern int nsp_dialog1(const char *title,const char *init_value,char **answer);

/* file */

extern int nsp_get_file_window(const char *title,const char *dirname,int action,char **file,int *ierr);
extern char * nsp_get_filename_open(const char *title,const char *dirname,char **filters);
extern char * nsp_get_filename_save(const char *title,const char *dirname);


/* madialog */

extern menu_answer nsp_matrix_dialog(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
				     NspSMatrix *Init_matrix,int menu_type,int entry_size);
extern int nsp_multi_dialog(NspSMatrix *Title,NspSMatrix *Labels,NspSMatrix  *Init_values, int *cancel);
extern int nsp_multi_dialog_(const char *labels,char **pszTitle, char **pszName, int  nv, int  *ierr);

/* menus */

extern void nsp_menus_set(int win_num,const char *button_name,int ne);
extern void nsp_menus_unset(int win_num,const char *button_name,int ne);
extern int nsp_menus_delete_button (int win_num,const char *button_name); 
extern int nsp_menus_add(int win_num,const char * button_name,char ** entries,int ne,int typ,char *fname);

/* men_combo_color */

extern GtkWidget *nsp_gtkcombobox_colormap_new( BCG *Xgc,int init_color);
extern int gtkcombobox_select_color(BCG *Xgc,int init_color) ;

/* utilities */

extern void nsp_dialogs_insert_title(const char *title,GtkWidget *vbox);

#endif 

