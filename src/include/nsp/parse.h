#ifndef NSP_INC_PARSE 
#define NSP_INC_PARSE 

#include <nsp/eval.h> 

extern int nsp_parse(Tokenizer *T,NspBHash *symb_table,PList *plist);
extern int nsp_parse_top(Tokenizer *T,NspBHash *symb_table,PList *plist);
extern PList nsp_parse_expr(NspSMatrix *M);
extern char *nsp_function_name(PList plist);

extern int nsp_check_simple_mlhs(PList L);
extern int nsp_check_simple_listeval(PList plist);
extern int nsp_check_simple_listeval(PList plist);
extern int nsp_check_is_mlhs(PList plist, PList *plist1, int *kount);
extern int nsp_check_is_mlhs(PList plist, PList *plist1, int *kount);
extern char * nsp_check_unique_name_in_mlhs(PList L);

extern const char *nsp_astcode_to_nickname(int code);
extern const char *nsp_astcode_to_name(int code);
extern char * nsp_astcode_to_string(int type);
extern int nsp_is_nsp_keyword(const char *id);
extern int nsp_is_code_keyword(int keyc);
extern int nsp_print_opname(int code);


extern NspSMatrix *nsp_lasterror_get(void) ;
extern void nsp_lasterror_clear(void);

#endif 
