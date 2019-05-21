#ifndef NSP_INC_MENUS 
#define NSP_INC_MENUS 

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 */

#include <string.h>
#include <stdio.h>
#include <nsp/machine.h>
#include <nsp/math.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/objects.h>
#include <nsp/gtk/gtkcombobox.h>

#if defined(__MWERKS__)||defined(THINK_C)
#define Widget int
#define TRUE 1
#define FALSE 0
#endif

typedef enum { menu_ok , menu_cancel, menu_fail, menu_bad_argument} menu_answer; 

/* choose */

menu_answer nsp_choose(NspSMatrix *Items,NspSMatrix *Title,NspSMatrix *button,int *nrep);
menu_answer nsp_choose_(const char *title,char **Items,int nItems,char **but_names,
			int n_but,int *choice);

/* dialog */

extern menu_answer nsp_message(NspSMatrix *Message,NspSMatrix *Buttons,int *rep);
extern menu_answer nsp_message_modeless(NspSMatrix *Message,NspSMatrix *Buttons);
extern menu_answer nsp_message_(const char *message,char **buttons,int n_buttons,int *rep);
extern menu_answer nsp_message_modeless_(const char *message);


/* choices */

menu_answer nsp_choices_with_combobox(char *title,NspList *L,NspList **Res,int use_table);

/* print */

int nsp_print_dialog(char **print_command,int *type,int *orientation,int *format);
int nsp_export_dialog(char **file,int *type,int *orientation,int *format);

/* dialog */

extern menu_answer nsp_dialog(NspSMatrix *title,NspSMatrix *init,NspObject **answer);
extern menu_answer nsp_dialog1(const char *title,const char *init,char **answer);

/* file */

extern menu_answer nsp_get_file_window(const char *title,const char *dirname,int action,char **file);
extern char * nsp_get_filename_open(const char *title,const char *dirname,char **filters);
extern char * nsp_get_filename_save(const char *title,const char *dirname,const char *filename_in,char **filters);
extern char * nsp_get_filename_folder(const char *title,const char *dirname);


/* madialog */

extern menu_answer nsp_multi_dialog_(const char *title,char **pszTitle, char **pszName,int nv);
extern menu_answer nsp_matrix_dialog(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
				     NspSMatrix *Init_matrix,int menu_type,int entry_size);

/* menus */

extern void nsp_menus_set(int win_num,const char *button_name,int ne);
extern void nsp_menus_unset(int win_num,const char *button_name,int ne);
extern int nsp_menus_delete_button (int win_num,const char *button_name); 
extern int nsp_menus_add(int win_num,const char * button_name,char ** entries,int ne,int typ,char *fname);

/* men_combo_color */

extern GtkWidget *nsp_gtkcombobox_colormap_new( BCG *Xgc,int init_color);
extern int gtkcombobox_select_color_in_table(NspMatrix *table,int init_color) ;
extern int gtkcombobox_select_color(BCG *Xgc,int init_color) ;


/* mdial*/

extern menu_answer nsp_multi_dialog(NspSMatrix *Title,NspSMatrix *Labels,NspSMatrix *Init_values);
extern menu_answer nsp_multi_dialog_(const char *title,char **pszTitle, char **pszName,int nv);

/* utilities */

extern void nsp_dialogs_insert_title(const char *title,GtkWidget *vbox);

/* set the stop button handler */

typedef void (*Stop_menu_handler) (void);

extern Stop_menu_handler set_stop_menu_handler( Stop_menu_handler f) ;
extern void reset_stop_menu_handler(void);

#endif 

