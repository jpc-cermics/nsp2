#ifndef SCI_ADDINTER 
#define SCI_ADDINTER 

/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 *********************************************************************/

/** the first dynamic interface is at position DYN_INTERF_START+1 **/
#define DYN_INTERF_START 500

#define MAXINTERF 50

typedef struct 
{
  char name[NAME_MAXL];  /** name of interface **/
  int (*func)();         /** entrypoint for the interface **/
  int Nshared;           /** id of the shared library **/
  int ok;                /** flag set to 1 if entrypoint can be used **/
} Iel;

extern Iel DynInterf[MAXINTERF];
extern int LastInterf;


void AddInter (char **files, char *iname, char **enames, int *err);
int BlankInterface (int i, char *fname, int first, int rhs, int opt, int lhs);
void RemoveInterf (int Nshared);


#endif 
