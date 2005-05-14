#ifndef NSP_INC_PARSE 
#define NSP_INC_PARSE 

extern int TokenLineSet(int l);

extern int ParseEvalDir(const char *Dir, char *Fname);
extern int ParseEvalDirFull(const char *Dir);
extern int ParseEvalFromStd(int display);
extern int ParseEvalFromSMatrix(NspSMatrix *M,int display,int echo, int error,int pause);
extern int ParseEvalFile(char *Str, int display,int echo, int error,int pause);
extern int ParseEvalFromStr(char *Str, int display,int echo, int error,int pause);


#endif 
