#ifndef NSP_INC_ACCELERATEDTAB
#define NSP_INC_ACCELERATEDTAB

#include "interf.h"   /* ici pour le type function */

/* enum codes for accelerated ops */

typedef enum { 
  undef_tab=-1,
  concatr_tab=0, 
  concatd_tab=1,
  extract_tab=2,
  extractelts_tab=3,
  extractcols_tab=4,
  extractrows_tab=5,
  resize2vect_tab=6,
  deleteelts_tab=7,
  deletecols_tab=8,
  deleterows_tab=9,
  tozero_tab=10,
  setrowscols_tab=11} accelerated_ops;

typedef struct _AcceleratedTab AcceleratedTab;

struct _AcceleratedTab
{
  int ops_id; /* this should be equal to the indice in table */
  const char *opname;
  int arity;
  const char **accelerated_types;
  int length;
  function **func;
} ;

extern AcceleratedTab accelerated_tabs[];

extern function *nsp_get_fast_function(AcceleratedTab *tab, int type_id);
extern int nsp_init_accelerated_tabs(void);

#endif 
