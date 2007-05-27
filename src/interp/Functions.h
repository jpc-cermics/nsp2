#ifndef NSP_INC_EVAL_F
#define NSP_INC_EVAL_F

#include "nsp/tokenizer.h"

extern char *getenv ();

extern char *nsp_force_prompt(void);
extern void inc_pause_prompt(void);
extern void dec_pause_prompt(void);
extern void zero_pause_prompt();

extern char *tgetstr ();
extern int gchar_no_echo (void);
extern int get_echo_mode ();
extern int get_echo_mode (void);
extern int get_is_reading (void);
extern int ioctl ();
extern int tgetent ();
extern char *nsp_prompt(void);
extern char *nsp_force_prompt(void);
extern void controlC_handler (int sig);
extern void set_echo_mode (int mode);
extern void set_is_reading (int mode);

#ifdef NSP_INC_PList 

#include "nsp/tokenizer.h"

/* LibsTab.c */

extern void nsp_init_macro_table(void);
extern int nsp_enter_macros(const char *dirname,int recursive,int compile);
extern int nsp_delete_macros(const char *Dir);
extern const char *nsp_get_libdir(int num);
extern void nsp_macro_table_reset_cache(void);
/* Parse.c */
extern void plist_name_to_local_id(PList List,NspBHash *H,int rec);

#endif

#endif

