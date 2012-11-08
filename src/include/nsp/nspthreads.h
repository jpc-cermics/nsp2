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
  int i,rhs, opt, lhs, ans;
  GAsyncQueue *queue;
};

/* should be moved in config.h */
/* #define NSP_WITH_MAIN_GTK_THREAD */

#ifdef NSP_WITH_MAIN_GTK_THREAD
extern GThread *thread1,*thread2,*thmain;
#endif 

extern void nsp_check_threads(const char *str);
extern GThread *nsp_gtk_main_thread(void);
extern int nsp_interface_executed_in_main_thread(int i, function f,Stack *stack, 
						 int rhs, int opt, int lhs);

#endif 




