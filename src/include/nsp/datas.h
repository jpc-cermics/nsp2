#ifndef SCI_DATAS 
#define SCI_DATAS 

extern NspList *Datas ;

int NewFrame(void);
void DeleteFrame(void);
void FramesInfo(void);
void FrameInfo(void);
void FramesPrint(void);
void FramePrint(void);
int  FrameReplaceObj(NspObject *A);
int  GlobalFrameReplaceObj(NspObject *A);
NspObject *FramesSearchObj(char *str);
NspObject *FrameSearchObj(String *str);
NspObject *GlobalFrameSearchObj(String *str);
NspObject *FrameSearchObjAndRemove(String *str);
int SearchObjAndMoveToNextFrame(String *str);
int MoveObjToNextFrame(NspObject *O);


#endif



