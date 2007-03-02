#ifndef SCI_BASIC_GEN
#define SCI_BASIC_GEN

#define NbGenInNsp 6
enum {MT, KISS, FSULTRA, WELL1024A, CLCG4, CLCG2};

typedef struct _NspRandomGen NspRandomGen;

struct _NspRandomGen
{
  int id_gen;
  unsigned long (*gen)();
  const char *name_gen;
  int dim_state;
  unsigned long max_int; 
  double factor;              /* 1.0/(rng_max_int+1) */
  void (*get_state)(double *);
  int (*set_state)(double *);
  int (*set_state_simple)(double);
} ;

extern NspRandomGen MersenneTwister;
extern NspRandomGen Kiss;
extern NspRandomGen Fsultra;
extern NspRandomGen Well1024a;
extern NspRandomGen Clcg4;
extern NspRandomGen Clcg2;

extern NspRandomGen *NspRNG[NbGenInNsp];

/* header for mt */
unsigned long randmt();
int set_state_mt_simple(double s);
int set_state_mt(double *seed_array);
void get_state_mt(double *state);

/* header for kiss */
unsigned long kiss();
int set_state_kiss(double g[]);
int set_state_kiss_simple(double g);
void get_state_kiss(double g[]);

/* header for clcg2 */
unsigned long clcg2();
int set_state_clcg2(double g[]);
int set_state_clcg2_simple(double g);
void get_state_clcg2(double g[]);

/* header for bcpl */
unsigned long randbcpl(unsigned long s);

/* header for fsultra */
unsigned long fsultra();
int set_state_fsultra(double g[]);
int set_state_fsultra_simple(double g);
void get_state_fsultra(double g[]);

/* header for the well1024a */
unsigned long well1024a();
int set_state_well1024a_simple(double s);
int set_state_well1024a(double seed_array[]);
void get_state_well1024a(double state[]);

/* header for the clcg4 */
#define Maxgen  100
typedef  enum {InitialSeed, LastSeed, NewSeed}  SeedType;
unsigned long clcg4();
int set_state_clcg4(double s[]);
int set_state_clcg4_simple(double s0);
void get_state_clcg4(double s[]);
int get_current_clcg4(void);
int set_current_clcg4(int new_clcg4_gen);
void init_generator_clcg4(SeedType Where);
void advance_state_clcg4(int k);
int set_initial_seed_clcg4(double s[]);

#endif /** SCI_BASIC_GEN   **/




