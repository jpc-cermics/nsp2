#ifndef SCI_WIN_FN 
#define SCI_WIN_FN 


/*** elsewhere **/

extern int C2F (scilab) (int *nostartup);  
extern void C2F (scilines) (int *nl, int *nc);  
extern int C2F (sciquit) (void);  
extern void C2F(setfbutn)  (char *name,int *rep);
extern void  controlC_handler(int n);

extern void  reset_scig_handler (void);
extern void  reset_scig_click_handler (void);
extern void  reset_scig_deletegwin_handler (void);
extern void  reset_scig_command_handler (void);

typedef int (*Scig_click_handler) (int,int,int,int,int,int);
extern Scig_click_handler set_scig_click_handler (Scig_click_handler f);

/* io.c */

extern void sci_winch_signal(int n) ;
extern void write_scilab(char *s);
extern int Xorgetchar(void);
extern void  nsp_check_gtk_events(void);


/* command.h */

typedef int (*Scig_command_handler) (char *);
extern Scig_command_handler set_scig_command_handler(Scig_command_handler f);
extern void reset_scig_command_handler(void) ;
extern int enqueue_nsp_command( char *command); 
extern int dequeue_nsp_command(char *buf,int buf_len);
extern int checkqueue_nsp_command(void) ;

/* zzledt.c */ 

extern int using_readline(void); 
extern int get_one_char(char *prompt);
extern void SciGtkReadLine(char *prompt, char *buffer, int *buf_size, int *len_line, int *eof);
extern int nsp_read_history();
extern int nsp_write_history();
extern void sci_get_screen_size (int *rows,int *cols);

/* XXXXXXXXX*/ 

#define  sciprint Sciprintf
#define  sciprint_nd Sciprintf
#define  Scistring Sciprintf

/* x_main.c */ 

extern int real_main(int argc, char **argv);
extern void start_sci_gtk(); 
extern void sci_clear_and_exit(int n);
extern void sci_usr1_signal(int n) ;

/* ../sun/inffic.c */ 

extern char *get_sci_data_strings(int n);

/* menu.c */ 


/* extern GtkWidget *create_main_menu( GtkWidget  *window); */
extern void create_plugged_main_menu() ;
extern void MenuFixCurrentWin(int ivalue);
extern int nsp_menus_delete_button(int *win_num,char *button_name);
extern void nsp_menus_add(int *win_num,char * button_name,char ** entries,int * ne,int *typ,char * fname, int *ierr);
extern int nsp_menus_unset(int *win_num,char *button_name,int *entries,int *ptrentries,int *ne,int *ierr);
extern int nsp_menus_set(int *win_num,char *button_name,int *entries,int *ptrentries,int *ne,int *ierr);
extern void * graphic_initial_menu(int winid) ;

/* about.c */

extern void create_scilab_about(void);

#endif 
