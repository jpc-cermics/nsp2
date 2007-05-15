#ifndef NSP_INC_PARSE 
#define NSP_INC_PARSE 

extern int TokenLineSet(int l);
extern int nsp_parse_eval_file(char *Str, int display,int echo, int errcatch, int pause,int mtlb);
extern int nsp_parse_eval_from_string(char *Str,int display,int echo, int errcatch,int pause);
extern int nsp_parse_eval_from_smat(NspSMatrix *M,int display,int echo, int errcatch,int pause);
extern int nsp_parse_eval_from_std(int display);
extern int nsp_parse_eval_dir(const char *Dir, char *Fname);
extern int nsp_parse_eval_dir_full(const char *Dir);


#endif 
