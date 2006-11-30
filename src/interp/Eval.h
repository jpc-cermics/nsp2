#ifndef SCI_EVAL 
#define SCI_EVAL 

#include "nsp/interf.h" 
#include "nsp/datas.h" 
#include "nsp/accelerated_tab.h" 

extern int nsp_eval(PList L1,Stack, int first, int rhs,int lhs,int display);
extern int nsp_store_result(char *str,Stack stack, int first);
extern int nsp_eval_arg(PList L,Stack, int i,int rhs,int lhs,int display);
extern int nsp_eval_func(NspObject *O,const char *str,int msuffix, Stack, int first, int rhs, int opt, int lhs);
extern int nsp_eval_maybe_accelerated_op(char *opname, int msuffix, AcceleratedTab *tab,
					 Stack stack, int first, int rhs, int opt, int lhs);
extern int nsp_eval_maybe_accelerated_binop(const char *opname, int opcode,
					    Stack stack, int first, int rhs, int opt, int lhs);
extern int nsp_eval_dotplus(Stack, int first, int rhs, int opt, int lhs);

extern int nsp_eval_macro(NspObject *OF,Stack,int first,int rhs,int opt,int lhs);
extern int nsp_eval_method(char *str, Stack stack, int first, int rhs, int opt, int lhs);
extern int nsp_interfaces(int i, int num, Stack, int rhs, int opt, int lhs);
extern int nsp_check_stack(Stack, int rhs, int opt, int lhs,char *message,char *name);
extern int nsp_eval_macro_body(NspObject *OF, Stack stack, int first, int rhs, int opt, int lhs);
/* int SearchInOPt (char *str,Stack, int first, int nargs,int *wrong_pos); */
extern void nsp_set_dollar(NspObject *O, int j);
extern void nsp_build_funcname(const char *str,Stack,int first,int rhs,char *name);
extern void nsp_build_funcnameij(const char *str,Stack,int first,int i,int j,char *name);
extern int nsp_eval_extract(Stack,int first,int rhs,int opt,int lhs);
extern int nsp_eval_extract_cells(Stack stack, int first, int rhs, int opt, int lhs);
extern void nsp_void_seq_object_destroy(Stack stack,int from, int to);

extern int reorder_stack(Stack stack, int ret) ;
extern int nsp_parser_get_line(PList L);

extern int nsp_parse_eval_dir(const char *Dir, char *Fname);
extern int nsp_parse_eval_dir_full(const char *Dir);

#endif 
