#ifndef NSP_INC_TYPE_OBJECT_INLINE
#define NSP_INC_TYPE_OBJECT_INLINE

/* NSP_INLINE is set to static inline if 
 * inline is used or to nothing if no inlining
 */

NSP_OBJECT_INLINED int check_cast(const void *obj,NspTypeId id)
{
  /* down to basetype */
  NspTypeBase *type = ((NspObject *)obj)->basetype;
  /* speed up with a do while by bruno */
  do
    { 
      if ( type->id == id ) return TRUE;
      /* walk up and try to match */
    }
  while ((type= type->surtype) != NULL );
  return FALSE;
}

NSP_OBJECT_INLINED int nsp_get_id_from_object(NspObject *Obj)
{
  NspTypeBase *type = Obj->basetype;
  return type->id;
}

#endif
