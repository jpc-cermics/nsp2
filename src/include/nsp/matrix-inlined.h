#ifndef NSP_INC_TYPE_MATRIX_INLINE
#define NSP_INC_TYPE_MATRIX_INLINE

/* NSP_MATRIX_INLINE is set to static inline if 
 * inline is used or to nothing if no inlining
 */

NSP_MATRIX_INLINED NspMatrix *GetMatCopy(Stack stack, int i)
{
  NspMatrix *M= Mat2double(matrix_object(stack.val->S[stack.first+i-1]));
  if ( M== NULLMAT) { ArgMessage (stack, i);return M;}
  /**/ return (NspMatrix *)  MaybeObjCopy (&stack.val->S[stack.first+i-1]);
}

NSP_MATRIX_INLINED NspMatrix *GetMat(Stack stack, int i)
{
  NspMatrix *M= Mat2double(matrix_object(stack.val->S[stack.first+i-1]));
  if ( M== NULLMAT) ArgMessage (stack, i);
  return M;
}

#endif
