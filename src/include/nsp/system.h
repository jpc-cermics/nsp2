#ifndef NSP_INC_SYSTEM
#define NSP_INC_SYSTEM

extern double nsp_timer(void);
extern double nsp_cputime(void);
extern int nsp_realtime(double *t);
extern int nsp_realtime_init( double *t,  double *scale);
extern int nsp_stimer(void);
extern int nsp_tictoc(double *etime);

#endif 

