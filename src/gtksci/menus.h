#ifndef NSP_GTKSCI_MENUS 
#define NSP_GTKSCI_MENUS 

/* menus.c:51:OF */ extern void create_plugged_main_menu (void); 
/* menus.c:95:NF */ extern GtkWidget *create_main_menu (GtkWidget *window); 
/* menus.c:133:NF */ extern void MenuFixCurrentWin (int ivalue); 
/* menus.c:189:NF */ extern int nsp_menus_delete_button (int *win_num, char *button_name); 
/* menus.c:230:NF */ extern void nsp_menus_add (int *win_num, char *button_name, char **entries, int *ne, int *typ, char *fname, int *ierr); 
/* menus.c:331:NF */ extern int nsp_menus_set (int *win_num, char *button_name, int *entries, int *ptrentries, int *ne, int *ierr); 
/* menus.c:337:NF */ extern int nsp_menus_unset (int *win_num, char *button_name, int *entries, int *ptrentries, int *ne, int *ierr); 
/* menus.c:719:NF */ extern void *graphic_initial_menu (int winid); 
/* menus.c:827:NF */ extern void scig_print (int winid); 

#endif 

