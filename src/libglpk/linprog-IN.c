/* Nsp
 * Copyright (C) 2012-2019 Bruno Pinçon Esial/Iecn
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 */

#include <glpk.h>
#include <setjmp.h>
#include <nsp/interf.h>
#include <nsp/matrix.h>
#include <nsp/smatrix.h>
#include <nsp/hash.h>
#include <nsp/spcolmatrix.h>
#include <nsp/sciio.h>
#include <nsp/system.h>
/* 
 * nsp interface to glpk
 *
 * POSSIBLE IMPROVMENTS:
 *     1/ add the possibility to write a mps or cplex file in linprog
 *        (somebody may want to solve a LP problem build in nsp with
 *        another LP solver...
 *
 *     2/ add postprocessing features of glpk
 *
 *     3/ verify, when provided, that the heuristic solution is feasible 
 *        (but it will be possible that glpk do that in the future)
 *
 *  Another possibility would be to define glpk "LP object" in nsp and interface 
 *  all glpk routines.
 */


/* solvers flags */
#define SMPLX 0
#define IPT 1
#define MIP 2

/* a structure to store solver options given by the optional named argument solver_options=hashtable */
typedef struct
{
  Boolean presolve; /* use presolver GLP_OFF|GLP_ON  (common to smplx and mip) */
  int meth;         /* simplex algo GLP_PRIMAL=1 | GLP_DUALP=2 | GLP_DUAL=3 */ 
  int pricing;      /* 0 <-> GLP_PT_STD (=0x11) | 1 <-> GLP_PT_PSE (=0x22) */ 
  int r_test;       /* 0 <-> GLP_RT_STD (=0x11) | 1 <-> GLP_RT_HAR (=0x22) */ 
  int ord_alg;      /* reordering for ipt solver */
  Boolean gmi_cuts; /* GLP_OFF=0|GLP_ON=1 */
  Boolean mir_cuts; /* GLP_OFF=0|GLP_ON=1 */
  Boolean cov_cuts; /* GLP_OFF=0|GLP_ON=1 */
  Boolean clq_cuts; /* GLP_OFF=0|GLP_ON=1 */
  Boolean fp_heur;  /* GLP_OFF=0|GLP_ON=1 */
  Boolean binarize; /* GLP_OFF=0|GLP_ON=1 */
  int br_tech;      /* GLP_BR_FFV=1|GLP_BR_LFV=2|GLP_BR_MFV=3|GLP_BR_DTH=4|GLP_BR_PCH=5 */
  int bt_tech;      /* GLP_BT_DFS=1|GLP_BT_BFS=2|GLP_BT_BLB=3|GLP_BT_BPH=4 */
  double mip_gap;   /* should be >= 0 */ 
  int scale;        /* an int in [0,13] */ 
  NspHash *basis_info;
} Solvers_params;


#define CheckFinite(pos,obj,k) for ( k = 0 ; k < obj->mn ; k++ ) \
    { if ( ! finite(obj->R[k]) ) { Scierror("Error: component %d of argument %d is not finite\n",k+1,pos); \
	return RET_BUG;}  }

#define CheckNotNan(argname,obj,k) for ( k = 0 ; k < obj->mn ; k++ ) \
    { if ( isnan(obj->R[k]) ) { Scierror("Error: component %d of opt named argument %s is a Nan\n",k+1,argname); \
	return RET_BUG;}  }

/* return codes at nsp level */
#define LP_OPT 0	        /* solution is optimal (or integer optimal for a mip) */
#define LP_UNBND 1	        /* solution is unbounded */
#define LP_NO_FEAS 2	        /* LP is unfeasible  */ 
#define LP_NO_DUAL_FEAS 3       /* dual LP is unfeasible */
#define LP_TIME_OUT_FEAS 4      /* time limit reached with a feasible solution */
#define LP_TIME_OUT_DUAL_FEAS 5 /* time limit reached with a dual feasible solution */
#define LP_ITER_OUT_FEAS 6      /* iteration limit reached with a feasible solution */
#define LP_ITER_OUT_DUAL_FEAS 7 /* iteration limit reached with a dual feasible solution */
#define LP_MIPGAP_REACHED 8     /* mip gap reached (for mip LP) */

/* 
 * static int get_status_flag(int ret_code, int sol_status, int dual_sol_status)
 * 
 * build and return our own return code from the return code 'ret_code' 
 * of the used glpk solver (SMPLX, IPT, MIP) and the solution status 'sol_status'
 * (and also from dual_sol_status for simplex solver, for the other solver we set
 *  dual_sol_status to UNDEF so that it has no effect)
 *
 * values for our return codes: see just before 
 *                              or -ret_code (used after to set an error with the corresponding message)
 */
static int get_status_flag(int ret_code, int sol_status, int dual_sol_status)
{
  if ( ret_code == 0 )
    {
      if ( sol_status == GLP_OPT )
	return LP_OPT;
      else if ( sol_status == GLP_UNBND )
	return LP_UNBND;
      else if ( sol_status == GLP_NOFEAS )
	return LP_NO_FEAS;
      else if ( sol_status == GLP_INFEAS )   /* may be got with dual simplex */
	return LP_NO_DUAL_FEAS;       
      else  /* should never pass here ? */
	return -9999;
    }
  else if ( ret_code == GLP_ENOPFS ) 
    return LP_NO_FEAS;
  else if ( ret_code == GLP_ENODFS ) /*about this flag see: http://www.mail-archive.com/help-glpk@gnu.org/msg05652.html */
    return LP_NO_DUAL_FEAS;           
  else if ( ret_code == GLP_ETMLIM && sol_status == GLP_FEAS )
    return LP_TIME_OUT_FEAS;
  else if ( ret_code == GLP_ETMLIM && dual_sol_status == GLP_FEAS )
    return LP_TIME_OUT_DUAL_FEAS;
  else if ( ret_code == GLP_EITLIM && sol_status == GLP_FEAS )
    return LP_ITER_OUT_FEAS;
  else if ( ret_code == GLP_EITLIM && dual_sol_status == GLP_FEAS )
    return LP_ITER_OUT_DUAL_FEAS;
  else if ( ret_code == GLP_EMIPGAP )
    return LP_MIPGAP_REACHED;
  else
    return -ret_code;
}

static void display_error_message(int ret_code)
{
  char *mes;
  switch ( ret_code )
    {
    case GLP_EBADB:
      mes = "specified initial basis is invalid"; break;
    case GLP_ESING:
      mes = "initial basis matrix singular within working precision"; break;
    case GLP_ECOND:
      mes = "initial basis matrix ill-conditioned"; break;
    case GLP_EFAIL:
      mes = "solver failure"; break;
    case GLP_EOBJLL:
      mes = "objective lower limit reached (dual simplex)"; break;
    case GLP_EOBJUL:
      mes = "objective upper limit reached (dual simplex)"; break;
    case GLP_EITLIM:
      mes = "iteration limit exceeded without got a feasible solution"; break;
    case GLP_ETMLIM:
      mes = "time limit exceeded without got a feasible solution"; break;
    case GLP_ENOCVG:
      mes = "very slow convergence or divergence (ipt solver)"; break;
    case GLP_EINSTAB:
      mes = "numerical instability on solving newtonian system (ipt solver)"; break;
    default:
      Scierror("Error: error code (=%d) not managed (should not be got...)\n", ret_code);
      return;
    }
  Scierror("Error: %s\n", mes);
}


/*  static int get_glpk_scale_option(int scale)
 *
 *  transform scale option given at nsp level to scale option for glpk
 *
 *  note that we could scale too for ipt solver see
 *  http://lists.gnu.org/archive/html/help-glpk/2009-03/msg00057.html
 *
 *  glpk scale setting
 *     GLP_SF_GM       0x01 =  1 perform geometric mean scaling
 *     GLP_SF_EQ       0x10 = 16 perform equilibration scaling
 *     GLP_SF_2N       0x20 = 32 round scale factors to power of two
 *     GLP_SF_SKIP     0x40 = 64 skip if problem is well scaled
 *     GLP_SF_AUTO     0x80 =128 choose scaling options automatically
 *
 *  Note that some combinaisons are available for instance:
 *     GLP_SF_AUTO => GLP_SF_GM | GLP_SF_EQ | GLP_SF_SKIP
 *
 *  nsp scale option to glpk scale option:
 *     scale=0: no scaling
 *     scale=1: automatic scaling
 *     scale=2: geometric mean scaling
 *     scale=3: geometric mean scaling + round scale factors to power of two
 *     scale=4: geometric mean scaling but skip if problem is well scaled
 *     scale=5: geometric mean scaling + round scale factors to power of two but skip if problem is well scaled
 *     scale=6: equilibration scaling
 *     scale=7: equilibration scaling + round scale factors to power of two
 *     scale=8: equilibration scaling but skip if problem is well scaled
 *     scale=9: equilibration scaling + round scale factors to power of two but skip if problem is well scaled
 *     scale=10: geom mean + equil scaling
 *     scale=11: geom mean + equil scaling  + round scale factors to power of two
 *     scale=12: geom mean + equil scaling but skip if problem is well scaled (this is automatic scaling)
 *     scale=13:  geom mean + equil scaling +  round scale factors to power of two but skip if problem is well scaled
 */
static int get_glpk_scale_option(int scale)
{
  /* scale shoud be in [1, 13] */
  switch ( scale )
    {
    case  1: return GLP_SF_AUTO;
    case  2: return GLP_SF_GM; 
    case  3: return GLP_SF_GM | GLP_SF_2N;
    case  4: return GLP_SF_GM | GLP_SF_SKIP;
    case  5: return GLP_SF_GM | GLP_SF_2N | GLP_SF_SKIP;
    case  6: return GLP_SF_EQ;
    case  7: return GLP_SF_EQ | GLP_SF_2N;
    case  8: return GLP_SF_EQ | GLP_SF_SKIP;
    case  9: return GLP_SF_EQ | GLP_SF_2N | GLP_SF_SKIP;
    case 10: return GLP_SF_GM | GLP_SF_EQ;
    case 11: return GLP_SF_GM | GLP_SF_EQ | GLP_SF_2N;
    case 12: return GLP_SF_GM | GLP_SF_EQ | GLP_SF_SKIP;
    case 13: return GLP_SF_GM | GLP_SF_EQ | GLP_SF_2N | GLP_SF_SKIP;
    default: return GLP_SF_AUTO;
    }
} 

static int output_to_nsp_term(void *info, const char *s)
{
  Sciprintf("%s", s);
  return 1; 
}

jmp_buf intglpk_env;

#ifdef HAVE_GLPK_ERROR_HOOK
static void exit_from_glpk_redirected_to_nsp(void *info)
{
  longjmp(intglpk_env,-1);
}
#endif 

static int set_ijval_from_mat_or_spmat(int *Count, int row_stride, NspObject *A, 
				       int *iA, int *jA, double *valA, int *ii, int *jj)
{
  int i, j, k, count = *Count;

  if ( IsMat(A) )
    {
      NspMatrix *AA = (NspMatrix *) A;
      for ( j = 0, k = 0 ; j < AA->n ; j++ )
	for ( i = 0 ; i < AA->m ; i++, k++)
	  {
	    count++;
	    iA[count] = i+1+row_stride; jA[count] = j+1; valA[count] = AA->R[k];
	    if ( ! finite(valA[count]) ) { *ii = iA[count]; *jj = jA[count]; return FAIL; }
	  }
    }
  else  /* A is sparse */
    {
      NspSpColMatrix *AA = (NspSpColMatrix *) A;
      for ( j = 0, k = 0 ; j < AA->n ; j++ )
	{
	  SpCol *Col = AA->D[j];
	  for ( k = 0 ; k < Col->size ; k++)
	    {
	      count++;
	      iA[count] = Col->J[k]+1+row_stride; jA[count] = j+1; valA[count] = Col->R[k];
	      if ( ! finite(valA[count]) ) { *ii = iA[count]; *jj = jA[count]; return FAIL; }
	    }
	}
    }
  *Count = count;
  return OK;
}

static int get_solver_options(Stack stack, NspHash *solver_options, Solvers_params *SolPar, Boolean ismip)
{
  int presolve=-1, presolve_default = ismip ? GLP_ON : GLP_OFF;
  int meth=0, meth_default = GLP_PRIMAL;         /* simplex algo GLP_PRIMAL=1 | GLP_DUALP=2 | GLP_DUAL=3 */ 
  int pricing=-1, pricing_default=GLP_PT_PSE;    /* 0 <-> GLP_PT_STD (=0x11) | 1 <-> GLP_PT_PSE (=0x22) */ 
  int r_test=-1, r_test_default=GLP_RT_HAR;      /* 0 <-> GLP_RT_STD (=0x11) | 1 <-> GLP_RT_HAR (=0x22) */ 
  int ord_alg=-1, ord_alg_default = GLP_ORD_AMD; /* Ordering alg GLP_ORD_NONE=0|GLP_ORD_QMD=1|GLP_ORD_AMD=2|GLP_ORD_SYMAMD=3 */
  int gmi_cuts=-1, gmi_cuts_default = GLP_OFF;
  int mir_cuts=-1, mir_cuts_default = GLP_ON;
  int cov_cuts=-1, cov_cuts_default = GLP_OFF;
  int clq_cuts=-1, clq_cuts_default = GLP_OFF;
  int fp_heur=-1, fp_heur_default = GLP_OFF;
  int br_tech=0, br_tech_default = GLP_BR_DTH; /* br_tech \in [1,5] */
  int bt_tech=0, bt_tech_default = GLP_BT_BLB; /* bt_tech \in [1,4] */
  double mip_gap=-1.0, mip_gap_default = 0.0;  /* should be a real >= 0 */
  int binarize=-1, binarize_default = GLP_OFF;
  int scale=-1, scale_default = 1;  /* automatic scaling (if presolve is not set) */
  NspHash *basis_info=NULLHASH; 
  nsp_option opts[] ={
    { "presolve",s_bool,NULLOBJ,-1},
    { "meth"   , s_int ,NULLOBJ,-1},
    { "pricing", s_int ,NULLOBJ,-1},
    { "r_test" , s_int ,NULLOBJ,-1},
    { "ord_alg", s_int, NULLOBJ,-1},
    { "gmi_cuts",s_bool,NULLOBJ,-1},
    { "mir_cuts",s_bool,NULLOBJ,-1},
    { "cov_cuts",s_bool,NULLOBJ,-1},
    { "clq_cuts",s_bool,NULLOBJ,-1},
    { "fp_heur", s_bool,NULLOBJ,-1},
    { "br_tech", s_int, NULLOBJ,-1},
    { "bt_tech", s_int, NULLOBJ,-1},
    { "mip_gap", s_double, NULLOBJ,-1},
    { "binarize",s_bool, NULLOBJ,-1},
    { "scale"  , s_int, NULLOBJ,-1},
    { "basis_info", hash, NULLOBJ,-1},
    { NULL,      t_end ,NULLOBJ,-1}
  };

  if ( solver_options != NULL) 
    {
      if ( get_optional_args_from_hash(stack,solver_options,opts, &presolve, 
				       &meth, &pricing, &r_test, &ord_alg, &gmi_cuts, 
				       &mir_cuts, &cov_cuts, &clq_cuts, &fp_heur, 
				       &br_tech, &bt_tech, &mip_gap, &binarize, 
				       &scale, &basis_info) == FAIL )
	return FAIL;
    }

  SolPar->presolve = presolve < 0 ? presolve_default : presolve;
  SolPar->meth = meth < 1  || meth > 3 ? meth_default : meth;
  SolPar->pricing = pricing < 0  || pricing > 1 ? pricing_default : (pricing=0 ? GLP_PT_STD : GLP_PT_PSE);
  SolPar->r_test = r_test < 0  || r_test > 1 ? r_test_default : (r_test=0 ? GLP_PT_STD : GLP_PT_PSE);
  SolPar->ord_alg = ord_alg < 0  || ord_alg > 3 ? ord_alg_default : ord_alg;
  SolPar->gmi_cuts = gmi_cuts < 0 ? gmi_cuts_default : gmi_cuts;
  SolPar->mir_cuts = mir_cuts < 0 ? mir_cuts_default : mir_cuts;
  SolPar->cov_cuts = cov_cuts < 0 ? cov_cuts_default : cov_cuts;
  SolPar->clq_cuts = clq_cuts < 0 ? clq_cuts_default : clq_cuts;
  SolPar->fp_heur = fp_heur < 0 ? fp_heur_default : fp_heur;
  SolPar->br_tech = br_tech < 1 || br_tech > 5 ? br_tech_default : br_tech;
  SolPar->bt_tech = bt_tech < 1 || bt_tech > 4 ? bt_tech_default : bt_tech;
  SolPar->mip_gap = mip_gap < 0 || ! finite(mip_gap) ? mip_gap_default : mip_gap;
  SolPar->binarize = binarize < 0 ? binarize_default : binarize;
  SolPar->scale = scale < 0 || scale > 13 ? scale_default : scale;
  SolPar->basis_info = basis_info;
  return OK;
}

static void set_solver_options(void *gen_params, Solvers_params *SolPar, int solver, 
			       int tm_lim, int out_frq, int verb)
{
  switch (solver)
    {
    case(MIP):
      {
	glp_iocp *params = (glp_iocp *) gen_params;
	glp_init_iocp(params);
	params->tm_lim = tm_lim;
	params->msg_lev =  verb;
	params->out_frq = out_frq;
	params->presolve = SolPar->presolve;
	params->gmi_cuts = SolPar->gmi_cuts;
	params->mir_cuts = SolPar->mir_cuts;
	params->cov_cuts = SolPar->cov_cuts;
	params->clq_cuts = SolPar->clq_cuts;
	params->fp_heur = SolPar->fp_heur;
	params->br_tech = SolPar->br_tech;
	params->bt_tech = SolPar->bt_tech;
	params->mip_gap = SolPar->mip_gap;
	params->binarize = SolPar->binarize;
	break;
      }
    case (SMPLX):
      {
	glp_smcp *params = (glp_smcp *) gen_params;
	glp_init_smcp(params);
	params->msg_lev =  verb;
	params->tm_lim = tm_lim;
	params->out_frq = out_frq;
	params->presolve = SolPar->presolve;
	params->meth = SolPar->meth;
	params->pricing = SolPar->pricing;
	params->r_test = SolPar->r_test;
	break;
      }
    case (IPT):
      {
	glp_iptcp *params = (glp_iptcp *) gen_params; 
	glp_init_iptcp(params);
	params->msg_lev =  verb;
	params->ord_alg =  SolPar->ord_alg;
	break;
      }
    }
}

static void provide_heuristic_sol(glp_tree *tree, void *info)
{
  double *x = (double *)info - 1;
  if ( glp_ios_reason(tree) == GLP_IHEUR && glp_ios_curr_node(tree) == 1 )
    glp_ios_heur_sol(tree, x);   /* affichage valeur de retour ? */
  return;
}

static int int_glpk(Stack stack, int sense, NspMatrix *c, int nnzA, int *iA, int *jA, double *valA, 
		    NspMatrix *b, NspMatrix *be, NspSMatrix *var_type, NspMatrix *lb, NspMatrix *ub,
		    int solver, Solvers_params *SolPar, Boolean binprog, Boolean intprog, 
		    Boolean mipprog, int verb, int tm_lim, int out_frq, double *fopt, NspMatrix *xopt, int *Status_flag,
		    NspMatrix *redcosts, NspMatrix *lambda, NspMatrix *basis_info_str, NspMatrix *basis_info_aux,
		    NspMatrix *heuristic_sol)
{
  glp_prob *LP=NULL;
  int k, nb_var = c->mn, nb_ineq = b->mn, nb_eq = be->mn, ret_code, sol_status, dual_sol_status, status_flag;
  void *info=NULL;
  double PlusInf = 2.0*DBL_MAX, MinusInf = - PlusInf, Nan = 0.0/0.0;
 
  /* install function to redirect exit */
#ifdef HAVE_GLPK_ERROR_HOOK
  glp_error_hook(exit_from_glpk_redirected_to_nsp, info);
#endif 

  /* install function to redirect output terminal messages to nsp */
  glp_term_hook(output_to_nsp_term, info);
  
  /* describe LP problem for glpk */
  LP = glp_create_prob();

  /* set min or max */
  glp_set_obj_dir(LP, sense);

  /* set matrix cstr */
  glp_add_rows(LP, nb_ineq + nb_eq);
  glp_add_cols(LP, nb_var);
  glp_load_matrix(LP, nnzA , iA, jA, valA);
  
  /* set cost function coef */
  for ( k = 0 ; k < nb_var ; k++ )
    glp_set_obj_coef(LP, k+1, c->R[k]);
  
  /* set rows bounds */
  for ( k = 0 ; k < nb_ineq ; k++ )
    glp_set_row_bnds(LP, k+1, GLP_UP, 0.0, b->R[k]);
  for ( k = 0 ; k < nb_eq ; k++ )
    glp_set_row_bnds(LP, k+1+nb_ineq, GLP_FX, be->R[k], 0);
   
  /* set columns types */
  if ( binprog )
    for ( k = 0 ; k < nb_var ; k++ )
      glp_set_col_kind(LP, k+1, GLP_BV);
  else if ( intprog )
    for ( k = 0 ; k < nb_var ; k++ )
      glp_set_col_kind(LP, k+1, GLP_IV);
  else if ( mipprog )
    for ( k = 0 ; k < nb_var ; k++ )
      glp_set_col_kind(LP, k+1, strcmp(var_type->S[k],"I") == 0 ? GLP_IV : GLP_CV);

  
  /* set columns bounds */
  if ( ! binprog )
    {
      double binf, bsup;
      for ( k = 0 ; k < nb_var ; k++ )
	{
	  binf = lb != NULLMAT ? lb->R[k] : 0.0;
	  bsup = ub != NULLMAT ? ub->R[k] : PlusInf;
	  if ( intprog || (mipprog && strcmp(var_type->S[k],"I") == 0) )
	    {
	      binf = ceil(binf); bsup = floor(bsup);
	    }
	  if ( bsup < binf || (binf == bsup && isinf(binf)) )
	    { Scierror("Error: incompatible lower and upper bounds #%d\n", k+1); goto err; }
	  else if ( binf == bsup )
	    glp_set_col_bnds(LP, k+1, GLP_FX, binf, bsup);
	  else if ( MinusInf < binf && bsup < PlusInf )
	    glp_set_col_bnds(LP, k+1, GLP_DB, binf, bsup);
	  else if ( binf == MinusInf && bsup == PlusInf )
	    glp_set_col_bnds(LP, k+1, GLP_FR, binf, bsup);
	  else if ( MinusInf == binf && bsup < PlusInf )
	    glp_set_col_bnds(LP, k+1, GLP_UP, binf, bsup);
	  else /* ( MinusInf < binf && bsup == PlusInf ) */
	    glp_set_col_bnds(LP, k+1, GLP_LO, binf, bsup);
	}
    }
     
  if ( SolPar->basis_info != NULLHASH  &&  SolPar->presolve == 0  && solver != IPT )
    {
      NspMatrix *basis_str=NULLMAT, *basis_aux=NULLMAT;
      if (   (nsp_hash_find(SolPar->basis_info, "str", (NspObject **) &basis_str) == FAIL)
          || (nsp_hash_find(SolPar->basis_info, "aux", (NspObject **) &basis_aux) == FAIL))
	{
	  Scierror("Error: basis_info is invalid\n"); goto err;
	}
      if ( basis_str->mn != nb_var || basis_aux->mn != nb_eq+nb_ineq )
	{
	  Scierror("Error: basis_info is invalid\n"); goto err;
	}
      for ( k = 0 ; k < nb_var ; k++ )
	glp_set_col_stat(LP, k+1, (int) basis_str->R[k]);
      for ( k = 0 ; k < nb_eq+nb_ineq ; k++ )
	glp_set_row_stat(LP, k+1, (int) basis_aux->R[k]);
    }

  /* scale pb if asked */
  if ( SolPar->scale > 0  &&  SolPar->presolve == 0 )
    {
      if ( verb <= 1 ) 
	glp_term_out(GLP_OFF);
      glp_scale_prob(LP, get_glpk_scale_option(SolPar->scale));
      glp_term_out(GLP_ON);    
    }

  /* solve problem */
  if ( binprog || intprog || mipprog )
    {
      glp_iocp params;
      set_solver_options((void *) &params, SolPar, MIP, tm_lim, out_frq, verb);
      if ( heuristic_sol != NULLMAT )
	{
	  params.cb_func = provide_heuristic_sol;
	  params.cb_info = (void *) heuristic_sol->R;
	}
      if ( ! params.presolve )  /* we should provide first the solution of the */
	{                       /* relaxed LP problem with simplex method */
	  glp_smcp sparams; 
	  glp_init_smcp(&sparams);
	  sparams.msg_lev =  verb;  /* other parameters could be set but for "usual" mip */
                                    /* the relaxed LP is generally easy */
	  ret_code = glp_simplex(LP, &sparams);
	  sol_status = glp_get_status(LP);
	  status_flag = get_status_flag(ret_code, sol_status, GLP_UNDEF);
	  if ( status_flag != 0 )
	    {
	      if ( status_flag == 1 ) /* relaxed LP is unbounded, mip is unbounded too if at least a feasible */
		*fopt = Nan;          /* integer solution exists but we could not decide here so put fopt=Nan */
	      goto end;
	    }
	}
      ret_code = glp_intopt(LP, &params);
      sol_status = glp_mip_status(LP);
      status_flag = get_status_flag(ret_code, sol_status, GLP_UNDEF);
      if ( status_flag == 0 || status_flag == 4 || status_flag == 5 || status_flag == 8 )
	{      
	  *fopt = glp_mip_obj_val(LP);
	  for ( k = 0 ; k < nb_var ; k++ )
	    xopt->R[k] = glp_mip_col_val(LP, k+1); 
	}
    }
  else if ( solver == SMPLX )
    { 
      glp_smcp params; 
      set_solver_options((void *) &params, SolPar, SMPLX, tm_lim, out_frq, verb);
      ret_code = glp_simplex(LP, &params);
      sol_status = glp_get_status(LP);
      dual_sol_status = glp_get_dual_stat(LP);
      status_flag = get_status_flag(ret_code, sol_status, dual_sol_status);
      if ( status_flag == 0 || (4 <= status_flag && status_flag <= 7) )
	{
	  *fopt = glp_get_obj_val(LP);
	  for ( k = 0 ; k < nb_var ; k++ )
	    xopt->R[k] = glp_get_col_prim(LP, k+1);
	  if ( redcosts != NULLMAT )
	    {
	      for ( k = 0 ; k < nb_var ; k++ )
		{
		  redcosts->R[k] = glp_get_col_dual(LP, k+1);
		  basis_info_str->R[k] = glp_get_col_stat(LP, k+1);
		}
	      for ( k = 0 ; k < nb_eq + nb_ineq ; k++ )
		{
		  basis_info_aux->R[k] = glp_get_row_stat(LP, k+1);
		  lambda->R[k] =  glp_get_row_dual(LP, k+1);
		}
	    }
	}
      else if ( status_flag == 1 )  /* unbounded */
	*fopt = sense == GLP_MIN ? MinusInf : PlusInf;
    }
  else /* solver == IPT */
    {
      glp_iptcp params;
      set_solver_options((void *) &params, SolPar, IPT, tm_lim, out_frq, verb);
      ret_code = glp_interior(LP, &params);
      sol_status = glp_ipt_status(LP);
      status_flag = get_status_flag(ret_code, sol_status, GLP_UNDEF);
      if ( status_flag == 0 )
	{
	  *fopt = glp_ipt_obj_val(LP);
	  for ( k = 0 ; k < nb_var ; k++ )
	    xopt->R[k] = glp_ipt_col_prim(LP, k+1);
	  if ( redcosts != NULLMAT )
	    {
	      for ( k = 0 ; k < nb_var ; k++ )
		redcosts->R[k] = glp_ipt_col_dual(LP, k+1);
	      for ( k = 0 ; k < nb_eq + nb_ineq ; k++ )
		lambda->R[k] =  glp_ipt_row_dual(LP, k+1);
	    }
	}
    }
  
 end:
  if ( status_flag == 1 || status_flag  == 2 || status_flag == 3 )
    {
      nsp_matrix_resize(xopt, 0, 0);
      if ( redcosts != NULLMAT )
	{
	  nsp_matrix_resize(redcosts, 0, 0);
	  nsp_matrix_resize(lambda, 0, 0);
	  if ( solver == SMPLX )
	    {
	      nsp_matrix_resize(basis_info_str, 0, 0);
	      nsp_matrix_resize(basis_info_aux, 0, 0);
	    }
	}
    }

  if ( status_flag == 2 )      /* unfeasible */
    *fopt = sense == GLP_MIN ? PlusInf : MinusInf;
  else if ( status_flag == 3 ) /* dual unfeasible (primal is either unbounded or unfeasible) */
    *fopt = Nan;

  glp_term_hook(NULL, NULL);   /* uninstall term_hook  */
  glp_delete_prob(LP);
  
  if ( status_flag < 0 )
    {
      display_error_message(-status_flag);
      return FAIL;
    }
  else
    {
      *Status_flag = status_flag;
      return OK;
    }

 err:
  glp_delete_prob(LP);
  glp_term_hook(NULL, NULL);   /* uninstall term_hook  */
  return FAIL;
}

/*
 *    [xopt, fopt, exitflag, extras] = linprog(c, A, b, Ae, be, lb=, ub=, sense="min"|"max", 
 *                                             solver="smplx"|"ipt", var_type=
 *                                             binprog=%f|%t, intprog=%f|%t, verb=0|1|2|3,
 *                                             tm_lim= , out_frq= , heur_sol= , solver_options=hash)
 *
 *    with A and/or Ae full or sparse matrices.
 *
 *    when binprog is specified and true, lb, ub, var_type should not be given
 *    (binprog=%t implies lb[k] = 0, ub[k]=1, var_type[k]="I" for all variable k
 *
 *    when intprog is specified and true, var_type should not be given (implies
 *    var_type[k]="I" for all variable k)
 *
 */
int int_linprog(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *c=NULLMAT, *b=NULLMAT, *be=NULLMAT, *lb=NULLMAT, *ub=NULLMAT, *xopt=NULLMAT, *fopt=NULLMAT,
    *redcosts=NULLMAT, *lambda=NULLMAT, *basis_info_str=NULLMAT, *basis_info_aux=NULLMAT, *heur_sol=NULLMAT;
  NspObject *A, *Ae;
  NspHash *solver_options = NULLHASH, *extra = NULLHASH, *basis_info = NULLHASH;
  Solvers_params SolPar={0};
  char *sense_str = NULL, *solver_str = NULL;
  NspSMatrix *var_type = NULLSMAT;
  Boolean binprog=FALSE, intprog=FALSE, mipprog=FALSE;
  int k, ii, jj, nb_var, nb_ineq=0, nb_eq=0, count, status_flag;
  Boolean have_inequality_cstr=FALSE, have_equality_cstr=FALSE;
  int_types T[] = {realmat, obj, realmat, obj, realmat, new_opts, t_end} ;
  nsp_option opts[] ={{"lb",realmat,NULLOBJ,-1},
		      {"ub",realmat,NULLOBJ,-1},
		      {"sense",string,NULLOBJ,-1},
		      {"solver",string,NULLOBJ,-1},
		      {"var_type",smat,NULLOBJ,-1},
		      {"binprog",s_bool,NULLOBJ,-1},
		      {"intprog",s_bool,NULLOBJ,-1},
		      {"verb",s_int,NULLOBJ,-1},   /* verbosity level : GLP_MSG_OFF=0|GLP_MSG_ERR=1|GLP_MSG_ON=2|GLP_MSG_ALL=3 */
		      {"tm_lim",  s_double ,NULLOBJ,-1},
		      {"out_frq", s_int ,NULLOBJ,-1},
		      {"heur_sol",realmat,NULLOBJ,-1},
		      {"solver_options", hash, NULLOBJ,-1},
 		      { NULL,t_end,NULLOBJ,-1}};
  int *iA=NULL, *jA=NULL, nnzA=0, nnzAe=0;
  double *valA=NULL;
  int sense=GLP_MIN, solver = SMPLX, verb = -1, verb_default = GLP_MSG_ERR, tm_lim, tm_lim_default=300000; /* 5 mn */
  int out_frq=0, out_frq_default = 5000;  /* 5 sec */
  double tm_lim_d = -1;

  CheckLhs(1,4);

  if ( GetArgs(stack,rhs,opt,T,&c,&A,&b,&Ae,&be,
	       &opts,&lb,&ub,&sense_str,&solver_str,&var_type,&binprog,&intprog, 
	       &verb, &tm_lim_d, &out_frq, &heur_sol, &solver_options) == FAIL) return RET_BUG;

  /* check first arg c */
  CheckVector(NspFname(stack), 1, c);
  CheckFinite(1, c, k);
  nb_var = c->mn;

  /* check second arg A */
  if ( IsMat(A) )
    {
      NspMatrix *temp = (NspMatrix *) A;
      if ( temp->m != 0 )
	{
	  have_inequality_cstr = TRUE; nb_ineq = temp->m; 
	  CheckDimProp(NspFname(stack),1,2,temp->n!=nb_var);
	  CheckReal(NspFname(stack),2,temp);
	  nnzA = temp->mn;
	}
    }
  else if ( IsSpColMat(A) )
    {
      NspSpColMatrix *temp = (NspSpColMatrix *) A;
      if ( temp->m != 0 )
	{
	  have_inequality_cstr = TRUE; nb_ineq = temp->m; 
	  CheckDimProp(NspFname(stack),1,2,temp->n!=nb_var);
	  CheckReal(NspFname(stack),2,temp);
	  nnzA = nsp_spcolmatrix_nnz(temp);
	}
    }
  else
    {
      Scierror("Error:  2d arg of function %s should be a real full or sparse matrix\n",NspFname(stack));
      return RET_BUG;
    }

  /* check third arg b */
  if ( have_inequality_cstr )
    {
      CheckVector(NspFname(stack), 3, b);
    }
  CheckDimProp(NspFname(stack),2,3,b->mn!=nb_ineq);
  CheckFinite(3, b, k);

  /* check 4 th arg Ae */
  if ( IsMat(Ae) )
    {
      NspMatrix *temp = (NspMatrix *) Ae;
      if ( temp->m != 0 )
	{
	  have_equality_cstr = TRUE; nb_eq = temp->m; 
	  CheckDimProp(NspFname(stack),1,4,temp->n!=nb_var);
	  CheckReal(NspFname(stack),4,temp);	  
	  nnzAe = temp->mn;
	}
    }
  else if ( IsSpColMat(Ae) )
    {
      NspSpColMatrix *temp = (NspSpColMatrix *) Ae; 
      if ( temp->m != 0 )
	{
	  have_equality_cstr = TRUE; nb_eq = temp->m; 
	  CheckDimProp(NspFname(stack),1,4,temp->n!=nb_var);
	  CheckReal(NspFname(stack),4,temp);
	  nnzAe = nsp_spcolmatrix_nnz(temp);
	}
    }
  else
    {
      Scierror("Error: 4th arg of function %s should be a real full or sparse matrix\n",NspFname(stack));
      return RET_BUG;
    }

  /* check 5th arg be */
  if ( have_equality_cstr )
    {
      CheckVector(NspFname(stack), 5, be);
    }
  CheckDimProp(NspFname(stack),4,5,be->mn!=nb_eq);
  CheckFinite(5, be, k);

  /* check optional args */
  if ( binprog )
    {
      if ( lb != NULLMAT || ub != NULLMAT || var_type != NULLSMAT )
	{
	  Scierror("Error: with binprog=\%t you cannot not specify lb= nor ub= or var_type=\n");
	  return RET_BUG;
	}
    }
  else if ( intprog )
    {
      if ( var_type != NULLSMAT )
	{
	  Scierror("Error: with intprog=\%t you cannot not specify var_type=\n");
	  return RET_BUG;
	}
    }

  if ( lb != NULLMAT )
    {
      if ( (lb->m != 1 && lb->n != 1) || lb->mn != nb_var )
	{
	  Scierror("Error: incompatible dimensions for lower bound argument\n");
	  return RET_BUG;
	}
      CheckNotNan("lb",lb,k);
    }

  if ( ub != NULLMAT )
    {
      if ( (ub->m != 1 && ub->n != 1) || ub->mn != nb_var )
	{
	  Scierror("Error: incompatible dimensions for upper bound argument\n");
	  return RET_BUG;
	}
      CheckNotNan("ub",ub,k);
    }

  if ( sense_str != NULL )
    {
      if ( strcmp(sense_str,"min") == 0 )
	sense = GLP_MIN;
      else if ( strcmp(sense_str,"max") == 0 )
	sense = GLP_MAX;
      else
	{
	  Scierror("Error: sense should be 'min' or 'max'\n");
	  return RET_BUG;
	}
    }

  if ( var_type != NULLSMAT )
    {
      if ( (var_type->m != 1 && var_type->n != 1) || var_type->mn != nb_var )
	{
	  Scierror("Error: incompatible dimensions for var_type argument\n");
	  return RET_BUG;
	}
      for ( k = 0 ; k < nb_var ; k++ )
	{
	  char *t = var_type->S[k];
	  if ( strcmp(t, "I") == 0 )
	    mipprog = TRUE;
	  else if ( strcmp(t, "C") != 0 )
	    {
	      Scierror("Error: var_type components should be 'I' or 'C' (and var_type(%d) = %s)\n",k+1,t);
	      return RET_BUG;
	    }
	}
    }

  if ( lhs > 3 && ( mipprog || intprog || binprog ) )
    {
      Scierror("Error: 3 output arguments max for a binary, integer or mixed integer LP\n");
      return RET_BUG;
    }

  if ( solver_str != NULL )
    {
      if ( binprog || intprog || mipprog )
	Sciprintf("Warning: solver choice ignored (problem is an integer or mixed integer LP\n");
      else
	{
	  if ( strcmp(solver_str,"smplx") == 0 )
	    solver = SMPLX;
	  else if ( strcmp(solver_str,"ipt") == 0 )
	    solver = IPT;
	  else
	    {
	      Scierror("Error: solver should be 'smplx' or 'ipt'\n");
	      return RET_BUG;
	    }
	}
    }

  if ( heur_sol != NULLMAT )
    {
      if ( ! ( mipprog || intprog || binprog ) )
	Sciprintf("Warning: heuristic solution ignored\n");
      if ( heur_sol->mn != nb_var )
	{
	  Scierror("Error: incompatible dimensions for heur_sol optional argument\n");
	  return RET_BUG;
	}
      /* il faut verifier qu'elle est bien realisable... */
      /*    Ax <= b + tol*|b| ? ;  || Ae x - b ||/(1 + ||b||) <= tol  */
      /*    lb <= x <= ub et OK avec var_type            */
    }

  verb = verb < 0 || verb > 3 ? verb_default : verb;

  out_frq *= 1000; /* time is given in second for nsp interface and in msec for glpk */
  out_frq = out_frq <= 0 ? out_frq_default : out_frq;

  tm_lim_d *= 1000.0; /* time is given in second for nsp interface and in msec for glpk */
  if ( tm_lim_d < 0 )
    tm_lim = tm_lim_default;
  else
    tm_lim = tm_lim_d >= INT_MAX ? INT_MAX : (int) tm_lim_d;  /* INT_MAX => no time limit for glpk */
 
  if ( get_solver_options(stack, solver_options, &SolPar, binprog || intprog || mipprog) == FAIL )
    return RET_BUG;

  /* triplet representation iA, jA, valA*/
  iA = nsp_alloc_work_int(1+nnzA+nnzAe);
  jA = nsp_alloc_work_int(1+nnzA+nnzAe);
  valA = nsp_alloc_work_doubles(1+nnzA+nnzAe);
  if ( iA == NULL || jA == NULL || valA == NULL )
    goto err;
  count = 0;
  if ( have_inequality_cstr )
    if ( set_ijval_from_mat_or_spmat(&count, 0, A, iA, jA, valA, &ii, &jj) == FAIL ) /* some entries of A not finite */ 
      {
	Scierror("Error: coef (%d,%d) of argument 2 not finite\n", ii, jj);
	goto err;
      }

  if ( have_equality_cstr )
    if ( set_ijval_from_mat_or_spmat(&count, nb_ineq, Ae, iA, jA, valA, &ii, &jj) == FAIL ) /* some entries of Ae not finite */ 
      {
	Scierror("Error: coef (%d,%d) of argument 4 not finite\n", ii, jj);
	goto err;
      }
  
  if ( (xopt = nsp_matrix_create (NVOID, 'r', nb_var, 1)) == NULLMAT )
    goto err;

  if ( (fopt = nsp_matrix_create (NVOID, 'r', 1, 1)) == NULLMAT )
    goto err;

  if ( lhs == 4 )
    {
      if ( (extra = nsp_hash_create (NVOID, 3)) == NULLHASH )
	goto err;
      if ( (redcosts = nsp_matrix_create ("redcosts", 'r', nb_var, 1)) == NULLMAT )
	goto err;
      if ( nsp_hash_enter(extra, (NspObject *) redcosts) == FAIL )
	goto err;
      if ( (lambda = nsp_matrix_create ("lambda", 'r', nb_eq + nb_ineq, 1)) == NULLMAT )
	goto err;
      if ( nsp_hash_enter(extra, (NspObject *) lambda) == FAIL )
	goto err;
      if ( solver == SMPLX )
	{
	  if ( (basis_info = nsp_hash_create ("basis_info", 2)) == NULLHASH )
	    goto err;
	  if ( (basis_info_str = nsp_matrix_create ("str", 'r', nb_var, 1)) == NULLMAT )
	    goto err;
	  if ( nsp_hash_enter(basis_info, (NspObject *) basis_info_str) == FAIL )
	    goto err;
	  if ( (basis_info_aux = nsp_matrix_create ("aux", 'r', nb_eq + nb_ineq, 1)) == NULLMAT )
	    goto err;
	  if ( nsp_hash_enter(basis_info, (NspObject *) basis_info_aux) == FAIL )
	    goto err;
	  if ( nsp_hash_enter(extra, (NspObject *) basis_info) == FAIL )
	    goto err;
	}
    }

  if ( setjmp(intglpk_env) == 0 )
    {
      if (int_glpk(stack, sense, c, count, iA, jA, valA, b, be, var_type, lb, ub,
		   solver, &SolPar, binprog, intprog, mipprog, verb, tm_lim, out_frq, fopt->R, xopt,
		   &status_flag, redcosts, lambda, basis_info_str, basis_info_aux, heur_sol) == FAIL )
	goto err;
    }
  else
    {
      glp_free_env(); /* free memory allocated by glpk routines */
      Scierror("%s: internal error in glpk\n", NspFname(stack));
      goto err;
    }

  FREE(iA); FREE(jA); FREE(valA);

  MoveObj (stack, 1, (NspObject *) xopt);
  if ( lhs >= 2 )
    {
      MoveObj (stack, 2, (NspObject *) fopt);
      if ( lhs >= 3 )
	{
	  if ( nsp_move_double(stack, 3, status_flag) == FAIL )
	    goto err;
	  if ( lhs >= 4 )
	    MoveObj (stack, 4, (NspObject *) extra);
	}
    }
  
  return lhs;

 err:
  nsp_matrix_destroy(xopt);
  nsp_matrix_destroy(fopt);
  nsp_matrix_destroy(redcosts);
  nsp_matrix_destroy(lambda);
  nsp_matrix_destroy(basis_info_str);
  nsp_matrix_destroy(basis_info_aux);
  nsp_hdestroy(extra);
  nsp_hdestroy(basis_info);
  FREE(iA); FREE(jA); FREE(valA);
  return RET_BUG;
}

/*
 *    [c,A,b,Ae,be,sense,lb,ub,binprog,intprog,var_type] = readlp(filename,type="mps"|"free_mps"|"lp", verb=0|1|2)
 *
 */
int int_readlp(Stack stack, int rhs, int opt, int lhs)
{
  char filename_expanded[FSIZE+1];
  NspMatrix *c=NULLMAT, *b=NULLMAT, *be=NULLMAT, *lb=NULLMAT, *ub=NULLMAT;
  NspSpColMatrix *A=NULLSPCOLMAT, *Ae=NULLSPCOLMAT;
  NspSMatrix *var_type = NULLSMAT;
  char *filename=NULL, *type_str=NULL, *sense=NULL;
  int i, j, nb_ineq, nb_eq, nb_free, nb_var_int, nb_var_bin, ib, ibe, 
    nb_var, nb_cstr, *rownum=NULL, *ind=NULL, verb=-1, verb_default=1;
  Boolean binprog, intprog, mipprog;
  glp_prob *LP=NULL;
  int_types T[] = {string, new_opts, t_end} ;
  nsp_option opts[] ={{"type",string,NULLOBJ,-1},
		      {"verb",s_int,NULLOBJ,-1},
 		      { NULL,t_end,NULLOBJ,-1}};
  void *info=NULL;
  double *val=NULL;
  
  CheckLhs(1,11);

  if ( GetArgs(stack,rhs,opt,T,&filename, &opts, &type_str, &verb) == FAIL) 
    return RET_BUG;
  nsp_expand_file_with_exec_dir(&stack,filename,filename_expanded);
  
  verb = verb < 0 ? verb_default : verb;

  /* install function to redirect output terminal messages to nsp */
  glp_term_hook(output_to_nsp_term, info);

  if ( verb < 1 ) 
    glp_term_out(GLP_OFF);
  else
    glp_term_out(GLP_ON);

  LP = glp_create_prob();

  if ( type_str == NULL  ||  strcmp(type_str , "mps") == 0 ) 
    {
      if ( glp_read_mps(LP, GLP_MPS_DECK, NULL, filename_expanded) != 0 )
	{
	  Scierror("Error: glp_read_mps FAILS \n");
	  return RET_BUG;
	}
    }

  else if ( strcmp(type_str , "free_mps") == 0 ) 
    {
      if ( glp_read_mps(LP, GLP_MPS_FILE, NULL, filename_expanded) != 0 )
	{
	  Scierror("Error: glp_read_mps FAILS \n");
	  return RET_BUG;
	}
    }

  else if ( strcmp(type_str , "lp") == 0 ) 
    {
      if ( glp_read_lp(LP, NULL, filename_expanded) != 0 )
	{
	  Scierror("Error: glp_read_lp FAILS \n");
	  return RET_BUG;
	}
    }
  else
    {
      Scierror("Error: optional arg type=  in function %s should be 'mps' 'free_mps' or 'lp'\n",NspFname(stack));
      return RET_BUG;
    }

  /* get various elements */
  sense = glp_get_obj_dir(LP) == GLP_MIN ? "min" : "max";

  /* number of constraints */
  nb_cstr = glp_get_num_rows(LP);

  /* number of variables */
  nb_var = glp_get_num_cols(LP);

  /* in case of double bounded constraints we add a supplementary constraint 
     (as we record a LP using only A x <= b and Ae x = be) */
  nb_ineq = 0; nb_eq = 0; nb_free = 0;
  for ( i = 1 ; i <= nb_cstr ; i++ )
    switch ( glp_get_row_type(LP, i) )
      {
      case GLP_LO: case GLP_UP:
	nb_ineq++; break;
      case GLP_DB:
	nb_ineq+=2; break;
      case GLP_FX:
	nb_eq++; break;
      case GLP_FR:
	nb_free++; break;
      }

  /*  set binprog and intprog */
  nb_var_int = glp_get_num_int(LP);
  nb_var_bin = glp_get_num_bin(LP);
  binprog = nb_var_bin == nb_var;
  intprog = nb_var_int ==  nb_var;

  /* if ( verb >= 2 ) */
  /*   { */
  /*     Sciprintf("nb_cstr = %d\n",nb_cstr); */
  /*     Sciprintf("nnz = %d\n", glp_get_num_nz(LP)); */
  /*     Sciprintf("nb_var = %d\n",nb_var); */
  /*     Sciprintf("     dont nb_var_bin = %d\n",nb_var_bin); */
  /*     Sciprintf("dont nb_var_int = %d\n",nb_var_int); */
  /*     if ( nb_free > 0 ) */
  /* 	Sciprintf(" skipped %d free constraints\n",nb_free); */
  /*   } */

  if ( intprog )     /* here mipprog means a LP with both continuous and int variables */
    mipprog = FALSE; /* so that we should define var_type for each variables */
  else
    mipprog = nb_var_int > 0;

  /* allocate nsp objects */
  A = nsp_spcolmatrix_create(NVOID, 'r', nb_ineq, nb_var);
  b = nsp_matrix_create(NVOID, 'r', nb_ineq, 1);
  Ae = nsp_spcolmatrix_create(NVOID, 'r', nb_eq, nb_var);
  be = nsp_matrix_create(NVOID, 'r', nb_eq, 1);
  c = nsp_matrix_create(NVOID, 'r', nb_var, 1);
  lb = nsp_matrix_create(NVOID, 'r', nb_var, 1);
  ub = nsp_matrix_create(NVOID, 'r', nb_var, 1);
  if ( mipprog )
    var_type = nsp_smatrix_create(NVOID, nb_var, 1, "C", 1);
  else
    var_type = nsp_smatrix_create(NVOID, nb_var, 0, "", 0);
  rownum = nsp_alloc_work_int(nb_cstr+1);
  ind = nsp_alloc_work_int(nb_cstr+1);
  val = nsp_alloc_work_doubles(nb_cstr+1);

  if ( ! (A && b && Ae && be && c && lb && ub && var_type && rownum && ind && val) )
    goto err;

  /* fill c, lb, ub */
  for ( j = 0 ; j < nb_var ; j++ )
    {
      double PlusInf = 2.0*DBL_MAX, MinusInf = - PlusInf;
      
      c->R[j] = glp_get_obj_coef(LP, j+1);
      switch (glp_get_col_type(LP, j+1))
	{
	case GLP_FR:
	  lb->R[j] = MinusInf; ub->R[j] = PlusInf; break;
	case GLP_LO:
	  lb->R[j] = glp_get_col_lb(LP, j+1);  ub->R[j] = PlusInf; break;
	case GLP_UP:
	  lb->R[j] = MinusInf;  ub->R[j] = glp_get_col_ub(LP, j+1); break;
	case GLP_DB: case GLP_FX:
	  lb->R[j] = glp_get_col_lb(LP, j+1);  ub->R[j] = glp_get_col_ub(LP, j+1); break;
	}
    }

  /* fill var_type in case of mipprog */
  if ( mipprog )
    for ( j = 0 ; j < nb_var ; j++ )
      if (  glp_get_col_kind(LP, j+1) != GLP_CV )  /* initialised with 'C' */
	var_type->S[j][0] = 'I';
  
  /* fill b and be and array rownum (FIXME: more explanation for rownum) */
  ib = 0; ibe = 0;
  for ( i = 1 ; i <= nb_cstr ; i++ )
    switch ( glp_get_row_type(LP, i) )
      {
      case GLP_LO:
	b->R[ib] = -glp_get_row_lb(LP, i); 
	rownum[i] = ib; ib++; break;
      case GLP_UP:
	b->R[ib] =  glp_get_row_ub(LP, i); 
	rownum[i] = ib; ib++; break;
      case GLP_DB:
	b->R[ib] =  glp_get_row_ub(LP, i); 
	rownum[i] = ib; ib++;
	b->R[ib] = -glp_get_row_lb(LP, i); ib++; break;
      case GLP_FX:
	be->R[ibe] = glp_get_row_lb(LP, i); 
	rownum[i] = ibe; ibe++; break;
      }
  
  /* fill A and Ae */
  for ( j = 0 ; j < nb_var ; j++ )
    {
      int k, kA, kAe, nnz, nnzA=0, nnzAe=0;
      nnz = glp_get_mat_col(LP, j+1, ind, val);
      /* count number of non zero element in column j of A and Ae */
      for ( k = 1 ; k <= nnz ; k++ )
	switch ( glp_get_row_type(LP, ind[k]) )
	  {
	  case GLP_LO: case GLP_UP:
	    nnzA++; break;
	  case GLP_DB:
	    nnzA+=2; break;
	  case GLP_FX:
	    nnzAe++; break;
	  }
      /* now allocate column j for A and Ae */
      if ( nsp_spcolmatrix_resize_col(A, j, nnzA) == FAIL )
	goto err;
      if ( nsp_spcolmatrix_resize_col(Ae, j, nnzAe) == FAIL )
	goto err;
      /* fill column j of A and Ae */
      kA = 0; kAe = 0;
      for ( k = 1 ; k <= nnz ; k++ )
	switch ( glp_get_row_type(LP, ind[k]) )
	  {
	  case GLP_LO:
	    A->D[j]->R[kA] = -val[k]; A->D[j]->J[kA] = rownum[ind[k]]; kA++; break;
	  case GLP_UP:
	    A->D[j]->R[kA] = val[k]; A->D[j]->J[kA] = rownum[ind[k]]; kA++; break;
	  case GLP_DB:
	    A->D[j]->R[kA] = val[k]; A->D[j]->J[kA] = rownum[ind[k]]; kA++; 
	    A->D[j]->R[kA] = -val[k]; A->D[j]->J[kA] = rownum[ind[k]]+1; kA++; break;
	  case GLP_FX:
	    Ae->D[j]->R[kAe] = val[k]; Ae->D[j]->J[kAe] = rownum[ind[k]]; kAe++; break;
	  }
    }

  FREE(rownum); FREE(ind); FREE(val);
  glp_delete_prob(LP);
  glp_term_hook(NULL, NULL);   /* uninstall term_hook  */
  
  if ( lhs <= 1 ) 
    {
      /* return a hash table [c,A,b,Ae,be,sense,lb,ub,binprog,intprog,var_type] */
      NspObject *O;
      NspHash *D;
      if ( ( D = nsp_hash_create(NVOID, 20) ) == NULLHASH) goto err;
      if (nsp_object_set_name(NSP_OBJECT(c),"c") == FAIL) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(c))== FAIL) goto err;
      if (nsp_object_set_name(NSP_OBJECT(A),"A") == FAIL) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(A))== FAIL) goto err;
      if (nsp_object_set_name(NSP_OBJECT(b),"b") == FAIL) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(b))== FAIL) goto err;
      if (nsp_object_set_name(NSP_OBJECT(Ae),"Ae") == FAIL) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(Ae))== FAIL) goto err;
      if (nsp_object_set_name(NSP_OBJECT(be),"be") == FAIL) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(be))== FAIL) goto err;
      if (( O = nsp_new_string_obj("sense",sense, 3)) == NULLOBJ ) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(O))== FAIL) goto err;
      if (nsp_object_set_name(NSP_OBJECT(lb),"lb") == FAIL) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(lb))== FAIL) goto err;
      if (nsp_object_set_name(NSP_OBJECT(ub),"ub") == FAIL) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(ub))== FAIL) goto err;
      if ((O = (binprog != FALSE) ? nsp_create_true_object("binprog") : nsp_create_false_object("binprog") )==NULLOBJ) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(O))== FAIL) goto err;
      if ((O = (intprog != FALSE) ? nsp_create_true_object("intprog") : nsp_create_false_object("intprog") )==NULLOBJ) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(O))== FAIL) goto err;
      if (nsp_object_set_name(NSP_OBJECT(var_type),"var_type") == FAIL) goto err;
      if (nsp_hash_enter(D,NSP_OBJECT(var_type))== FAIL) goto err;
      MoveObj (stack, 1, NSP_OBJECT(D));
      return Max(lhs,1);
    }
  
  MoveObj (stack, 1, (NspObject *) c);
  if ( lhs >= 2 ) 
    MoveObj (stack, 2, (NspObject *) A);
  else
    nsp_spcolmatrix_destroy(A);
  if ( lhs >= 3 ) 
    MoveObj (stack, 3, (NspObject *) b);
  else
    nsp_matrix_destroy(b);
  if ( lhs >= 4 ) 
    MoveObj (stack, 4, (NspObject *) Ae);
  else
    nsp_spcolmatrix_destroy(Ae);
  if ( lhs >= 5 ) 
    MoveObj (stack, 5, (NspObject *) be);
  else
    nsp_matrix_destroy(be);
  if ( lhs >= 6 ) 
    {
      if ( nsp_move_string(stack, 6, sense, 3) == FAIL ) goto err;
    }
  if ( lhs >= 7 ) 
    MoveObj (stack, 7, (NspObject *) lb);
  else
    nsp_matrix_destroy(lb);
  if ( lhs >= 8 ) 
    MoveObj (stack, 8, (NspObject *) ub);
  else
    nsp_matrix_destroy(ub);
  if ( lhs >= 9 ) 
    if ( nsp_move_boolean(stack, 9, binprog) == FAIL ) goto err;
  if ( lhs >= 10 ) 
    if ( nsp_move_boolean(stack, 10, intprog) == FAIL ) goto err;
  if ( lhs >= 11 ) 
    MoveObj (stack, 11, (NspObject *) var_type);
  else
    nsp_smatrix_destroy(var_type);
  
  return lhs;
  
 err:
  nsp_spcolmatrix_destroy(A);
  nsp_spcolmatrix_destroy(Ae);
  nsp_matrix_destroy(b);
  nsp_matrix_destroy(be);
  nsp_matrix_destroy(c);
  nsp_matrix_destroy(lb);
  nsp_matrix_destroy(ub);
  nsp_smatrix_destroy(var_type);
  FREE(rownum); FREE(ind); FREE(val);
  glp_delete_prob(LP);
  glp_term_hook(NULL, NULL);   /* uninstall term_hook  */
  return RET_BUG;
}






static OpTab liblinprog_func[] = {
  {"linprog", int_linprog},
  {"linprog_glpk", int_linprog},
  {"readlp", int_readlp},
  {(char *) 0, NULL},
};

int liblinprog_Interf (int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(liblinprog_func[i].fonc)) (stack, rhs, opt, lhs);
}

void liblinprog_Interf_Info (int i, char **fname, function (**f))
{
 *fname = liblinprog_func[i].name;
 *f = liblinprog_func[i].fonc;
}


