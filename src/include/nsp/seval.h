#ifndef NSP_INC_SEVAL 
#define NSP_INC_SEVAL 

#include <nsp/interf.h> 
#include <nsp/datas.h> 
#include <nsp/accelerated_tab.h> 
#include <nsp/eval.h> 
#include <nsp/plist.h> 

extern int nsp_eval(PList L1,Stack, int first, int rhs,int lhs,int display);
extern int nsp_eval_arg(PList L,Stack *stack, int i,int rhs,int lhs,int display);
extern int nsp_eval_func(NspObject *O,const char *str,int msuffix, Stack stack, 
			 int first, int rhs, int opt, int lhs);
extern int nsp_eval_maybe_accelerated_op(char *opname, int msuffix, accelerated_ops tab_id,
					 Stack stack, int first, int rhs, int opt, int lhs);
extern int nsp_eval_maybe_accelerated_binop(const char *opname, int opcode, Stack stack, int first,
					    int rhs, int opt, int lhs);
extern int nsp_eval_dotplus(Stack, int first, int rhs, int opt, int lhs);
extern int nsp_eval_macro(NspObject *OF,Stack,int first,int rhs,int opt,int lhs);
extern int nsp_eval_method(char *str, Stack stack, int first, int rhs, int opt, int lhs);
extern int nsp_eval_macro_body(NspObject *OF, Stack stack, int first, int rhs, int opt, int lhs,int display);
extern int nsp_eval_extract(Stack,int first,int rhs,int opt,int lhs);
extern int nsp_eval_extract_cells(Stack stack, int first, int rhs, int opt, int lhs);

extern int nsp_store_result(char *str,Stack stack, int first);
extern int nsp_store_object(NspObject *Ob);
extern int nsp_interfaces(int i, int num, Stack, int rhs, int opt, int lhs);
extern int nsp_check_stack(Stack, int rhs, int opt, int lhs,char *message,char *name);

extern void nsp_build_funcname(const char *str,Stack *stack,int first,int rhs,char *name);
extern void nsp_build_funcnameij(const char *str,Stack *stack,int first,int i,int j,char *name);
extern void nsp_void_seq_object_destroy(Stack stack,int from, int to);

extern int nsp_reorder_stack(Stack stack, int ret) ;
extern int nsp_parser_get_line(PList L);

#endif 
