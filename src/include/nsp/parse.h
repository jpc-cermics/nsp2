#ifndef SCI_PARSE 
#define SCI_PARSE 

extern int TokenLineSet(int l);

int ParseEvalFile(char *Str, int display);
int ParseEvalFromStr(char *Str, int display);
int ParseEvalDir(char *Dir, char *Fname);
int ParseEvalLoop(int display);
int ParseEvalFromStd(int display);

#endif 
