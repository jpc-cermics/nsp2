#ifndef NSP_INC_ACCELERATEDTAB
#define NSP_INC_ACCELERATEDTAB

#include "interf.h"   /* ici pour le type function */

typedef struct _AcceleratedTab AcceleratedTab;

struct _AcceleratedTab
{
  char *opname;
  int arity;
  int nb_accelerated_types;
  int *accelerated_types;
  int length;
  function **func;
} ;

extern AcceleratedTab concatr_tab; 
extern AcceleratedTab concatd_tab; 
extern AcceleratedTab extract_tab; 
extern AcceleratedTab extractelts_tab;
extern AcceleratedTab extractcols_tab;
extern AcceleratedTab extractrows_tab;
extern AcceleratedTab resize2vect_tab;
extern AcceleratedTab deleteelts_tab;
extern AcceleratedTab deletecols_tab;
extern AcceleratedTab deleterows_tab;
extern AcceleratedTab tozero_tab;
extern AcceleratedTab setrowscols_tab;

function *nsp_get_fast_function(AcceleratedTab *tab, int type_id);
int nsp_init_accelerated_tabs(void);

#endif 
