#ifndef SCI_PARSE 
#define SCI_PARSE 

extern int TokenLineSet(int l);

extern int ParseEvalDir(char *Dir, char *Fname);
extern int ParseEvalFromStd(int display);
extern int ParseEvalFromSMatrix(NspSMatrix *M,int display,int echo, int error);
extern int ParseEvalFile(char *Str, int display,int echo, int error);
extern int ParseEvalFromStr(char *Str, int display,int echo, int error);


#endif 
