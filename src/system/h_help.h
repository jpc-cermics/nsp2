#ifndef __SCIHELP
#define __SCIHELP

#include "../machine.h" 

/**************************************
 * extern data 
 **************************************/

extern char   **helpTopicInfo ;
extern int      nTopicInfo ;

#define MAX_HELP_CHAPTERS 100

extern char* helpInfo[MAX_HELP_CHAPTERS];
extern char* helpPaths[MAX_HELP_CHAPTERS];
extern int   nInfo;

#define APROPOSMAX 100
#define MAXTOPIC 56

typedef struct {
  char name[MAXTOPIC];
  char *HelpTopic[APROPOSMAX];
  int  Where[APROPOSMAX];
  int  nTopic;
} Apropos;

extern  Apropos AP ;

#ifndef WIN32

extern void initHelpActions();
extern void initHelpPanel();
extern void setHelpShellState();

#else

extern void SciCallHelp  (char *helpPath,char *Topic);
extern int HelpGetPath  (char* line,char *Path,char *Tit);

#endif /* WIN32 */

extern int Sci_Help (char *name);
extern int Sci_Apropos (char *name);
extern int Help_Init   (void);
extern int setHelpTopicInfo  (int n);
extern void HelpActivate (int ntopic);
extern int initHelpDatas  (void);
extern void SciCallHelp  (char *helpPath,char *Topic);
extern int HelpGetPath  (char* line,char *Path,char *Tit);
extern int SetAproposTopics  (char *);

#endif /*  __SCIHELP */

