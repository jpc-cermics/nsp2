#ifndef SCI_BASIC_GEN
#define SCI_BASIC_GEN

#define NbGenInNsp 6
enum {MT, KISS, FSULTRA, WELL1024A, CLCG4, CLCG2};

typedef struct _NspRandomGen NspRandomGen;

struct _NspRandomGen
{
  int id_gen;
  guint32 (*gen)();
  const char *name_gen;
  int dim_state;
  guint32 max_int; 
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
guint32 randmt();

/* header for bcpl */
guint32 randbcpl(guint32 s);

/* header for the clcg4 */
#define Maxgen  100
typedef  enum {InitialSeed, LastSeed, NewSeed}  SeedType;
extern int set_current_clcg4(int new_clcg4_gen);
extern int get_current_clcg4(void);
extern void init_generator_clcg4(SeedType Where);
extern int set_initial_seed_clcg4(double s[]);
extern void advance_state_clcg4(int k);


#endif /* SCI_BASIC_GEN  */




