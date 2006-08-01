#ifndef NSP_INC_EVAL_F
#define NSP_INC_EVAL_F

#include "nsp/tokenizer.h"

extern char *Keycode2str ();
extern char *Keycode2str (int keyc);
extern char *OpCode2Str (int code);
extern char *OpCode2NickN (int code);
extern char *TokenCode2Name (int key);
extern char *getenv ();
extern char *tgetstr ();

extern int CompOp (void);
extern int Getlin (char *prompt);
extern int IsCodeKeyword (int keyc);
extern int IsJustName ();
extern int IsMatOp (int *op);
extern int IsOrOp (Tokenizer *T,int *op);
extern int IsSciKeyWord (char *id);
extern int NextToken (void);
extern int PrintOPname (int code);

extern char *ForcePrompt(void);
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
extern char *Prompt (void);
extern char *ForcePrompt (void);
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
extern int Parse (Tokenizer *T,NspHash *symb_table,PList *plist);
extern int IsEmptyMat (PList *plist);
extern int IsMlhs (PList plist,PList *plist1, int *kount);
extern int PListLength (PList *L);
extern int ParseAdd (PList *plist, int op, int arity,int line);
extern int ParseAddDouble (PList *plist);
extern int ParseAddList (PList *plist, PList *l);
extern int ParseAddName (PList *plist, char *str);
extern int ParseAddString (PList *plist, char *str);
extern int ParseAddObject(PList *plist, NspObject *obj );
extern int ParseCheckFCall (PList *plist);
extern char *FunctionName (PList plist);
extern int ParseLfact (PList *plist);

extern int check_simple_listeval(PList plist);
extern int nsp_check_simple_mlhs(PList L);

/* LibsTab.c */

extern void  InitMacroTable  (void);
extern int EnterMacros(const char *dirname,int recursive,int compile);
extern int DeleteMacros(const char *Dir);

/* Parse.c */

extern int parse_top(Tokenizer *T,NspHash *symb_table,PList *plist);
extern void plist_name_to_local_id(PList List,NspHash *H);


#endif
#endif

