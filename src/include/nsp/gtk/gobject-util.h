/* -*- Mode: C -*- */
#ifndef INC_NSP_NspGObject_util
#define INC_NSP_NspGObject_util

/*----------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2008 ) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 *
 * A set of macros to deal with GList and GSList 
 *----------------------------------------------------------------------*/

/**
 * NSP_LIST_FROM_GLIST: 
 * @sel_code: 
 * @free_name: 
 *
 * This macro is used to buil a #NspList from a GList in the context of 
 * an interface i.e the #NspList is stored on the stack and returned by 
 * this macro. All the items contained in the GList are suposed to be of the same type. 
 * The user must provide in @sel_code the code which is to be used to create 
 * a #NspObject from an item contained in the GList 
 * and @free_name is the function name to be used for freeing 
 * the memory associated to the GList. 
 * The GList is assumed to be named list and the current GList item list is 
 * named tmp.
 * Note that @free_name is only called in case of error.
 * 
 */

/* XXX even in case of success free_name(list); should be performed */

#define NSP_LIST_FROM_GLIST(list, sel_code, free_name)	\
  if (( nsp_list =nsp_list_create(NVOID) ) == NULLLIST) \
    { \
      free_name(list); \
      return RET_BUG; \
    } \
  for (tmp = list; tmp != NULL; tmp = tmp->next)  \
    { \
      NspObject *gtk_obj = (NspObject *)sel_code ;\
      if (gtk_obj == NULL) goto clean ;  \
      if ( nsp_list_end_insert(nsp_list, gtk_obj) == FAIL ) goto clean; \
    } \
  MoveObj(stack,1,(NspObject *)nsp_list); \
  return 1; \
  clean :  \
    { \
      free_name(list); \
      nsp_list_destroy(nsp_list); \
      return RET_BUG; \
    } 

/**
 * NSP_OBJ_LIST_FROM_GLIST: 
 * @sel_code: 
 * @free_name: 
 *
 * This macro is similar to NSP_LIST_FROM_GLIST() except that this macro 
 * returns the builded #NspList.
 */

#define NSP_OBJ_LIST_FROM_GLIST(list, sel_code, free_name)	\
  if (( nsp_list =nsp_list_create(NVOID) ) == NULLLIST) \
    { \
      free_name(list); \
      return NULLOBJ; \
    } \
  for (tmp = list; tmp != NULL; tmp = tmp->next)  \
    { \
      NspObject *gtk_obj = (NspObject *)sel_code ;\
      if (gtk_obj == NULL) goto clean ;  \
      if ( nsp_list_end_insert(nsp_list, gtk_obj) == FAIL ) goto clean; \
    } \
  return (NspObject *) nsp_list; \
  clean :  \
    { \
      free_name(list); \
      nsp_list_destroy(nsp_list); \
      return NULL; \
    } 

/* Build a NspList */ 

#define NSP_LIST_DEC NspList *nsp_list;

#define NSP_LIST(iter_code , sel_code ) \
  if ( (nsp_list = nsp_list_create(NVOID)) == NULLLIST )  return RET_BUG; \
  iter_code \
    { NspObject *nsp_node; \
      sel_code ; \
      if ( nsp_node == NULL) goto clean ;  \
      if ( nsp_list_end_insert(nsp_list, nsp_node) == FAIL ) goto clean; \
    } 

#define NSP_OBJ_LIST(iter_code , sel_code ) \
  if ( (nsp_list =nsp_list_create(NVOID)) == NULLLIST )  return NULL; \
  iter_code \
    { NspObject *nsp_node; \
      sel_code ; \
      if ( nsp_node == NULL) goto clean ;  \
      if ( nsp_list_end_insert(nsp_list, nsp_node) == FAIL ) goto clean; \
    } 

#define NSP_LIST_CLEAN \
  clean :  \
    { \
       nsp_list_destroy(nsp_list); \
      return RET_BUG; \
    }

#define NSP_OBJ_LIST_CLEAN \
  clean :  \
    { \
      nsp_list_destroy(nsp_list); \
      return NULL; \
    }


#endif 

