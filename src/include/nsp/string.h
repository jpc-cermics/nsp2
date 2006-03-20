#ifndef NSP_INC_STRING
#define NSP_INC_STRING

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

typedef char * nsp_string ; /* a string i.e char * */
typedef const char *nsp_const_string ;  /* a constant string */

extern nsp_string new_nsp_string(nsp_const_string str); 
extern nsp_string nsp_basic_to_string(nsp_const_string str); 
extern nsp_string nsp_string_copy(nsp_const_string str); 
extern void nsp_string_destroy(nsp_string *str); 
extern nsp_string new_nsp_string_n(int n); 
extern int nsp_string_resize(nsp_string *hstr, unsigned int n); 
extern nsp_string nsp_string_to_utf8(nsp_string str);
extern nsp_string nsp_new_string(nsp_const_string bytes,int length);


#endif 

