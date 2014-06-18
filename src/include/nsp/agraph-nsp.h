/* -*- Mode: C -*- */
#ifndef NSP_INC_NspAgraph_NSP
#define NSP_INC_NspAgraph_NSP

#include "agnode.h"
#include "agedge.h"
#include "agsym.h"

NspAgraph *nsp_agread(void *chan);
/* NspAgraph *nsp_agconcat(NspAgraph * g, void *chan, Agdisc_t * disc); */
int nsp_agwrite(NspAgraph * g, char *chan);
void nsp_agflatten(NspAgraph * g, int flag);
int nsp_agisflattened(NspAgraph * g);
int nsp_agisdirected(NspAgraph * g);
int nsp_agisundirected(NspAgraph * g);
int nsp_agisstrict(NspAgraph * g);

/* edges */

NspAgedge *nsp_agidedge(NspAgnode * t, NspAgnode * h, unsigned long id,
			  int createflag);
NspAgedge *nsp_agsubedge(NspAgraph * g, NspAgedge * e, int createflag);
NspAgedge *nsp_agfstin(NspAgnode * n);
NspAgedge *nsp_agnxtin(NspAgedge * e);
NspAgedge *nsp_agfstout(NspAgnode * n);
NspAgedge *nsp_agnxtout(NspAgedge * e);
NspAgedge *nsp_agfstedge(NspAgnode * n);
NspAgedge *nsp_agnxtedge(NspAgedge * e, NspAgnode * n);

/* generic */

int nsp_agrelabel(void *obj, char *name);	/* scary */
int nsp_agrelabel_node(NspAgnode * n, char *newname);
int nsp_agdelete(NspAgraph * g, void *obj);
int nsp_agdelnode(NspAgnode * arg_n);
int nsp_agdeledge(NspAgedge * arg_e);
int nsp_agisarootobj(void *);
/* strings */
char *nsp_agstrdup(NspAgraph *, char *);
char *agstrdup_nsp_html(NspAgraph *, char *);
int nsp_aghtmlstr(char *);
char *nsp_agstrbind(NspAgraph * g, char *);
int nsp_agstrfree(NspAgraph *, char *);
char *nsp_agcanonstr(char *, char *);
char *nsp_agcanonStr(char*);
NspAgsym *nsp_agattr(NspAgraph * g, int kind, char *name,
		       char *value);
NspAgsym *nsp_agattrsym(void *obj, char *name);
NspAgsym *nsp_agnxtattr(NspAgraph * g, int kind, NspAgsym * attr);
void *nsp_agbindrec(void *obj, char *name, unsigned int size,  int move_to_front);
/* Agrec_t *nsp_aggetrec(void *obj, char *name, int move_to_front);*/
int nsp_agdelrec(void *obj, char *name);
void nsp_aginit(NspAgraph * g, int kind, char *rec_name,
		   int rec_size, int move_to_front);
void nsp_agclean(NspAgraph * g, int kind, char *rec_name);
char *nsp_agget(void *obj, char *name);
char *nsp_agxget(void *obj, NspAgsym * sym);
int nsp_agset(void *obj, char *name, char *value);
int nsp_agxset(void *obj, NspAgsym * sym, char *value);

/* set cardinality */
int nsp_agdegree(NspAgnode * n, int in, int out);

#endif 
