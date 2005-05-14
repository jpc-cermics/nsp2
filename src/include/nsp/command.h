#ifndef NSP_INC_GTK_COMMAND_H 
#define NSP_INC_GTK_COMMAND_H 

typedef int (*Scig_command_handler) (char *);
extern int scig_command_handler_none (char *command);
extern Scig_command_handler set_scig_command_handler(Scig_command_handler f);
extern void reset_scig_command_handler() ;
extern int enqueue_nsp_command(char *command);
extern int checkqueue_nsp_command() ;
extern void lockqueue_nsp_command() ;
extern void unlockqueue_nsp_command() ;
extern int dequeue_nsp_command(char *buf,int buf_len);

#endif 
