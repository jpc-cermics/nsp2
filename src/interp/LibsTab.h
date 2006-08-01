#ifndef SCI_MACROTAB
#define SCI_MACROTAB

extern int EnterMacros(const char *dirname,int recursive,int compile);
extern int DeleteMacros (const char *Dir);
extern NspObject *FindMacro (const char *str);
extern void InitMacroTable (void) ;

#endif
