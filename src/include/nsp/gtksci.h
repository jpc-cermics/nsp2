#ifndef NSP_GTKSCI
#define NSP_GTKSCI

/* about.c:11:OF */ extern void create_nsp_about (void); 

/* click.c */
extern int scig_click_handler_none (int win, int x, int y, int ibut, int motion, int release); 
extern int scig_click_handler_sci (int win, int x, int y, int ibut, int motion, int release); 
typedef int (*Scig_click_handler) (int,int,int,int,int,int);
extern Scig_click_handler set_scig_click_handler (Scig_click_handler f); 
extern void reset_scig_click_handler (void); 
extern int PushClickQueue (int win, int x, int y, int ibut, int motion, int release); 
extern int CheckClickQueue (int *win, int *x, int *y, int *ibut); 
extern int ClearClickQueue (int win); 

/* helpbrowser.c*/ 

extern int Sci_Help (char *mandir, char *locale, char *help_file); 

/* io.c*/ 

extern void Scisncr (char *str); 
extern int sciprint2 (int iv, char *fmt, ...); 
extern void nsp_in_gtk_window (void); 
extern int nsp_is_gtk_window (void); 
extern void nsp_activate_gtk_events_check (void); 
extern int nsp_check_events_activated (void); 
extern void write_scilab (char *s); 
extern int Xorgetchar (void); 
extern void nsp_check_gtk_events (void); 
extern void sci_winch_signal (int n); 

/* x_main.c */ 

void nsp_gtk_init(int argc, char **argv,int no_window);
extern void start_sci_gtk (void); 
extern void sci_clear_and_exit (int n); 
extern void sci_usr1_signal (int n); 
extern void sci_sig_tstp (int n); 
extern int kill_process_group (int pid, int sig); 
extern void getcolordef (int *screenc); 
extern void setcolordef (int screenc); 

/* zzledt-rl.c */ 
extern int using_readline (void); 
extern int get_one_char (char *prompt); 
extern void SciGtkReadLine (char *prompt, char *buffer, int *buf_size, int *len_line, int *eof); 
extern int nsp_read_history (void); 
extern int nsp_write_history (void); 
extern void sci_get_screen_size (int *rows, int *cols); 


/* menus.c */ 
extern void create_plugged_main_menu (void); 
extern void MenuFixCurrentWin (int ivalue); 
extern int nsp_menus_delete_button (int *win_num, char *button_name); 
extern void nsp_menus_add (int *win_num, char *button_name, char **entries, int *ne, int *typ, char *fname, int *ierr); 
extern int nsp_menus_set (int *win_num, char *button_name, int *entries, int *ptrentries, int *ne, int *ierr); 
extern int nsp_menus_unset (int *win_num, char *button_name, int *entries, int *ptrentries, int *ne, int *ierr); 
extern void *graphic_initial_menu (int winid); 
extern void scig_print (int winid); 

#endif 

