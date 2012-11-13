#ifndef NSP_INC_NSPTHREADS
#define NSP_INC_NSPTHREADS

#include <gtk/gtk.h>
#include <nsp/nsp.h>
#include <nsp/interf.h>

/* utilities to send the interface execution to 
 * the gtk thread 
 */

typedef struct _nsp_thread_interface nsp_thread_interface;

struct _nsp_thread_interface {
  Stack *stack;
  function *fun;
  int i, rhs, opt, lhs, ans;
  GAsyncQueue *queue;
};

typedef struct _nsp_thread_method nsp_thread_method;

struct _nsp_thread_method {
  Stack *stack;
  NspObject *Obj;
  nsp_method *fun;
  int rhs, opt, lhs, ans;
  GAsyncQueue *queue;
};


/* should be moved in config.h */
#if 0
#define NSP_WITH_MAIN_GTK_THREAD 
#endif

#ifdef NSP_WITH_MAIN_GTK_THREAD
extern GThread *thread1,*thread2,*thmain;
#endif 

extern void nsp_check_threads(const char *str);
extern GThread *nsp_gtk_main_thread(void);
extern int nsp_interface_executed_in_main_thread(int i, function f,Stack *stack, 
						 int rhs, int opt, int lhs);
extern int nsp_new_interp(GThread *thread,int argc,char **argv);

extern int nsp_method_executed_in_main_thread(NspObject *Ob, nsp_method f,
					      Stack *stack, int rhs, int opt, int lhs);

#endif 




