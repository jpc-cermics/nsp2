#ifndef SCI_FUNTAB
#define SCI_FUNTAB

/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

int EnterFunction (char *str,int Int,int Num);
void DeleteFunction (char *str);
void DeleteFunctionS(int);
int FindFunction (char *str,int *Int,int *Num);
void InitFunctionTable (void) ;
int  FindFunctionB (char *key,int Int,int Num);
void  InitFunctionTable  (void);

#endif
