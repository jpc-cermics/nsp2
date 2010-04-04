#ifndef NSP_INC_EVAL 
#define NSP_INC_EVAL 

#include <nsp/object.h> 

extern int nsp_parse_eval_file(char *Str, int display,int echo, int errcatch, int pause,int mtlb);
extern int nsp_parse_eval_from_string(const char *Str,int display,int echo, int errcatch,int pause);
extern int nsp_parse_eval_from_smat(NspSMatrix *M,int display,int echo, int errcatch,int pause);
extern int nsp_parse_eval_from_multistring(const char *Str,int display,int echo, int errcatch,int pause);

extern int nsp_parse_eval_from_std(int display);
extern int nsp_parse_eval_dir(const char *Dir, char *Fname);
extern int nsp_parse_eval_dir_full(const char *Dir);
extern int nsp_gtk_eval_function(NspPList *func,NspObject *args[],int n_args,
				 NspObject  *ret[],int *nret);
extern int nsp_gtk_eval_function_by_name(const char *name,NspObject *args[],int n_args,
					 NspObject  *ret[],int *nret);

#endif 
