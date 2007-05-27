#ifndef NSP_INC_PARSE 
#define NSP_INC_PARSE 



extern int nsp_parse_eval_file(char *Str, int display,int echo, int errcatch, int pause,int mtlb);
extern int nsp_parse_eval_from_string(char *Str,int display,int echo, int errcatch,int pause);
extern int nsp_parse_eval_from_smat(NspSMatrix *M,int display,int echo, int errcatch,int pause);
extern int nsp_parse_eval_from_std(int display);
extern int nsp_parse_eval_dir(const char *Dir, char *Fname);
extern int nsp_parse_eval_dir_full(const char *Dir);

extern int nsp_parse(Tokenizer *T,NspBHash *symb_table,PList *plist);
extern int nsp_parse_top(Tokenizer *T,NspBHash *symb_table,PList *plist);
extern PList nsp_parse_expr(NspSMatrix *M);
extern char *nsp_function_name(PList plist);

extern int nsp_check_simple_mlhs(PList L);
extern int nsp_check_simple_listeval(PList plist);
extern int nsp_check_simple_listeval(PList plist);
extern int nsp_check_is_mlhs(PList plist, PList *plist1, int *kount);


extern const char *nsp_astcode_to_nickname(int code);
extern const char *nsp_astcode_to_name(int code);
extern char * nsp_astcode_to_string(int type);
extern int nsp_is_nsp_keyword(const char *id);
extern int nsp_is_code_keyword(int keyc);
extern int nsp_print_opname(int code);


#endif 
