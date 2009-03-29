#ifndef NSP_INC_INDEX
#define NSP_INC_INDEX 

/*
 * This Software is GPL (Copyright ENPC 1998-2008) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

typedef enum { index_malloc_fail, index_wrong_object, index_wrong_value } index_vector_error;

typedef struct _index_vector index_vector ;

struct  _index_vector {
  int *val; /* array of integer pointer ind is "0-based" */
  int min;  /* min value of array "1-based" i.e min(val) +1 */
  int max;  /* max value of array "1-based" i.e max(val) +1*/
  int nval; /* size of array */
  int flag; /* TRUE if array goes from min to max with step 1 */
  int iwork;/* index of cache vector to use */
  index_vector_error error;/* error index */
};

int *nsp_get_index_vector_work(int work);

#endif 










  
  
  

  





