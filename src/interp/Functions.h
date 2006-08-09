#ifndef NSP_INC_EVAL_F
#define NSP_INC_EVAL_F

#include "nsp/tokenizer.h"

extern char *nsp_keycode2str();
extern char *nsp_keycode2str(int keyc);
extern char *nsp_opcode2str(int code);
extern char *nsp_opcode2nickname(int code);
extern char *TokenCode2Name (int key);
extern char *getenv ();
extern char *tgetstr ();

extern int CompOp (void);
extern int Getlin (char *prompt);
extern int nsp_is_code_keyword(int keyc);
extern int IsJustName ();
extern int IsMatOp (int *op);
extern int nsp_is_or_op(Tokenizer *T,int *op);
extern int nsp_is_nsp_keyword(char *id);
extern char * nsp_astcode_to_string(int type);
extern int NextToken (void);
extern int nsp_print_opname(int code);

extern char *nsp_force_prompt(void);
extern void inc_pause_prompt(void);
extern void dec_pause_prompt(void);
extern void zero_pause_prompt();

extern int backch (void);
extern int gchar_no_echo (void);
extern int get_echo_mode ();
extern int get_echo_mode (void);
extern int get_is_reading (void);
extern int ioctl ();
extern int tgetent ();
extern int viewch (void);
extern int C2F(xscion) (int *x);
extern char *nsp_prompt(void);
extern char *nsp_force_prompt(void);
extern void Stsync (int i);
extern void controlC_handler (int sig);
extern void set_echo_mode (int mode);
extern void set_is_reading (int mode);
extern int GetChar (void);
extern int backch (void);
extern int Getlin (char *str);
extern int SciReadLine (char *prompt,char *buffer, int *buf_size, int *eof);

#ifdef NSP_INC_PList 

#include "nsp/tokenizer.h"
extern int nsp_parse(Tokenizer *T,NspHash *symb_table,PList *plist);
extern int IsEmptyMat (PList *plist);
extern int is_mlhs(PList plist,PList *plist1, int *kount);
extern int PListLength (PList *L);
extern int nsp_parse_add(PList *plist, int op, int arity,int line);
extern int ParseAddDouble (PList *plist);
extern int nsp_parse_add_list(PList *plist, PList *l);
extern int nsp_parse_add_name(PList *plist, char *str);
extern int nsp_parse_add_string(PList *plist, char *str);
extern int nsp_parse_add_object(PList *plist, NspObject *obj );
extern int ParseCheckFCall (PList *plist);
extern char *nsp_function_name(PList plist);
extern int ParseLfact (PList *plist);

extern int check_simple_listeval(PList plist);
extern int nsp_check_simple_mlhs(PList L);

/* LibsTab.c */

extern void nsp_init_macro_table(void);
extern int nsp_enter_macros(const char *dirname,int recursive,int compile);
extern int nsp_delete_macros(const char *Dir);

/* Parse.c */

extern int parse_top(Tokenizer *T,NspHash *symb_table,PList *plist);
extern void plist_name_to_local_id(PList List,NspHash *H);


#endif
#endif

