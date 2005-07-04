#ifndef __SCICOS_BLOCKS__ 
#define __SCICOS_BLOCKS__ 

/*
 * Copyright INRIA
 * table of functions associated to predefined blocks 
 */

extern int scicos_affich (scicos_args_F0);
extern int scicos_bound (scicos_args_F0);
extern int scicos_constraint(scicos_args_Fi);
extern int scicos_csslti (scicos_args_F0);
extern int scicos_cstblk(scicos_args_F);
extern int scicos_delay (scicos_args_F0);
extern int scicos_delayv(scicos_args_F);
extern int scicos_diffblk(scicos_args_Fi);
extern int scicos_dlradp (scicos_args_F0);
extern int scicos_dollar (scicos_args_F0);
extern int scicos_dsslti (scicos_args_F0);
extern int scicos_eselect (scicos_args_Fm1);
extern int scicos_evscpe (scicos_args_F0);
extern int scicos_evtdly (scicos_args_F0);
extern int scicos_expblk (scicos_args_F0);
extern int scicos_forblk (scicos_args_F0);
extern int scicos_fscope(scicos_args_F);
extern int scicos_fsv (scicos_args_F0);
extern int scicos_gensin (scicos_args_F0);
extern int scicos_gensin (scicos_args_F0);
extern int scicos_gensin_test (scicos_args_F0);
extern int scicos_gensqr (scicos_args_F0);
extern int scicos_hltblk (scicos_args_F0);
extern int scicos_ifthel(scicos_args_Fm1);
extern int scicos_integr (scicos_args_F0);
extern int scicos_intplt (scicos_args_F0);
extern int scicos_intpol (scicos_args_F0);
extern int scicos_intrp2(scicos_args_F);
extern int scicos_intrpl (scicos_args_F0);
extern int scicos_invblk (scicos_args_F0);
extern int scicos_iocopy (scicos_args_F0);
extern int scicos_logblk (scicos_args_F0);
extern int scicos_lookup (scicos_args_F0);
extern int scicos_lsplit (scicos_args_F0);
extern int scicos_lsplit (scicos_args_F0);
extern int scicos_lusat (scicos_args_F);
extern int scicos_maxblk (scicos_args_F0);
extern int scicos_memo (scicos_args_F0);
extern int scicos_mfclck (scicos_args_F0);
extern int scicos_minblk (scicos_args_F0);
extern int scicos_mscope (scicos_args_F0);
extern int scicos_mux(scicos_args_F);
extern int scicos_demux(scicos_args_F);
extern int scicos_pload ( scicos_args_F0);
extern int scicos_powblk (scicos_args_F0);
extern int scicos_qzcel (scicos_args_F0);
extern int scicos_qzflr (scicos_args_F0);
extern int scicos_qzrnd (scicos_args_F0);
extern int scicos_qztrn (scicos_args_F0);
extern int scicos_rndblk (scicos_args_F0);
extern int scicos_samphold (scicos_args_F0);
extern int scicos_sawtth (scicos_args_F0);
extern int scicos_scope(scicos_args_F);
extern int scicos_scopxy (scicos_args_F0);
extern int scicos_scoxy (scicos_args_F0);
extern int scicos_selblk (scicos_args_F0);
extern int scicos_sinblk (scicos_args_F0);
extern int scicos_sqrblk (scicos_args_F0);
extern int scicos_sum2(scicos_args_F);
extern int scicos_sum3(scicos_args_F);
extern int scicos_tanblk (scicos_args_F0);
extern int scicos_tcslti(scicos_args_F);
extern int scicos_tcsltj (scicos_args_F0);
extern int scicos_timblk (scicos_args_F0);
extern int scicos_trash (scicos_args_F0);
extern int scicos_zcross (scicos_args_F0);
extern int scicos_integr (scicos_args_F0);
extern int scicos_intplt (scicos_args_F0);
extern void blocks_absblk(scicos_args_F0);
extern void blocks_andlog(scicos_args_F0);
extern void blocks_bidon(scicos_args_F0);
extern void blocks_cdummy(scicos_args_F0);
extern void blocks_cosblk(scicos_args_F0);
extern void blocks_dband(scicos_args_F0);
extern void blocks_gain(scicos_args_F0);
extern void plusblk(scicos_args_F2);
extern void prod(scicos_args_F2);
extern void readau(scicos_args_F2);
extern void readc(scicos_args_F2);
extern void relay(scicos_args_F2);
extern void scicos_plusblk(scicos_args_F2);
extern void selector(scicos_args_F2);
extern void slider(scicos_args_F0);
extern void sum(scicos_args_F2);
extern void switchn(scicos_args_F2);
extern void writeau(scicos_args_F2);
extern void writec(scicos_args_F2);
extern void zcross2(scicos_args_F0);

extern void absolute_value(scicos_block *block,int flag); 
extern void acos_blk(scicos_block *block,int flag);
extern void acosh_blk(scicos_block *block,int flag);
extern void asin_blk(scicos_block *block,int flag);
extern void asinh_blk(scicos_block *block,int flag);
extern void atan_blk(scicos_block *block,int flag);
extern void atanh_blk(scicos_block *block,int flag);
extern void tanh_blk(scicos_block *block,int flag);
extern void tan_blk(scicos_block *block,int flag);
extern void sin_blk(scicos_block *block,int flag);
extern void sinh_blk(scicos_block *block,int flag);
extern void backlash(scicos_block *block,int flag);
extern void cos_blk(scicos_block *block,int flag);
extern void cosh_blk(scicos_block *block,int flag);
extern void deadband(scicos_block *block,int flag);
extern void deriv(scicos_block *block,int flag);
extern void extractor(scicos_block *block,int flag);
extern void gainblk(scicos_block *block,int flag);
extern void time_delay(scicos_block *block,int flag);
extern void variable_delay(scicos_block *block,int flag);
extern void step_func(scicos_block *block,int flag);
extern void signum(scicos_block *block,int flag);
extern void summation(scicos_block *block,int flag);
extern void switch2(scicos_block *block,int flag);
extern void satur(scicos_block *block,int flag);
extern void logicalop(scicos_block *block,int flag);
extern void multiplex(scicos_block *block,int flag);
extern void hystheresis(scicos_block *block,int flag);
extern void ramp(scicos_block *block,int flag);
extern void minmax(scicos_block *block,int flag);
extern void modulo_count(scicos_block *block,int flag);
extern void mswitch(scicos_block *block,int flag);
extern void product(scicos_block *block,int flag);
extern void ratelimiter(scicos_block *block,int flag);
extern void integral_func(scicos_block *block,int flag);
extern void evtvardly(scicos_block *block,int flag);
extern void relationalop(scicos_block *block,int flag);
extern void bounce_ball(scicos_block *block,int flag);
extern void bouncexy(scicos_block *block,int flag);
extern void cscope(scicos_block *block,int flag);
extern void cmscope(scicos_block *block,int flag);
extern void scalar2vector(scicos_block *block,int flag);
extern void evaluate_expr(scicos_block *block,int flag);


 
scicos_block_table  tabsim[] ={
  {"absblk",(ScicosF) blocks_absblk},
  {"absolute_value",(ScicosF) absolute_value},
  {"acos_blk",(ScicosF) acos_blk},
  {"acosh_blk",(ScicosF) acosh_blk},
  {"affich",(ScicosF) scicos_affich},
  {"andlog",(ScicosF) blocks_andlog},
  {"asin_blk",(ScicosF) asin_blk},
  {"asinh_blk",(ScicosF) asinh_blk},
  {"atan_blk",(ScicosF) atan_blk},
  {"atanh_blk",(ScicosF) atanh_blk},
  {"backlash",(ScicosF) backlash},
  {"bidon",(ScicosF) blocks_bidon},
  {"bounce_ball",(ScicosF) bounce_ball},
  {"bouncexy",(ScicosF) bouncexy},
  {"cdummy",(ScicosF) blocks_cdummy},
  {"cmscope",(ScicosF) cmscope},
  {"constraint",(ScicosF) scicos_constraint},
  {"cos_blk",(ScicosF) cos_blk},
  {"cosblk",(ScicosF) blocks_cosblk},
  {"cosh_blk",(ScicosF) cosh_blk},
  {"cscope",(ScicosF) cscope},
  {"csslti",(ScicosF) scicos_csslti},
  {"cstblk",(ScicosF) scicos_cstblk},
  {"dband",(ScicosF) blocks_dband},
  {"deadband",(ScicosF) deadband},
  {"delay",(ScicosF) scicos_delay},
  {"delayv",(ScicosF) scicos_delayv},
  {"demux",(ScicosF) scicos_demux},
  {"deriv",(ScicosF) deriv},
  {"diffblk",(ScicosF) scicos_diffblk},
  {"dlradp",(ScicosF) scicos_dlradp},
  {"dollar",(ScicosF) scicos_dollar},
  {"dsslti",(ScicosF) scicos_dsslti},
  {"eselect",(ScicosF) scicos_eselect},
  {"evaluate_expr",(ScicosF) evaluate_expr},
  {"evscpe",(ScicosF) scicos_evscpe},
  {"evtdly",(ScicosF) scicos_evtdly},
  {"evtvardly",(ScicosF) evtvardly},
  {"expblk",(ScicosF) scicos_expblk},
  {"extractor",(ScicosF) extractor},
  {"forblk",(ScicosF) scicos_forblk},
  {"fscope",(ScicosF) scicos_fscope},
  {"fsv",(ScicosF) scicos_fsv},
  {"gainblk",(ScicosF) gainblk},
  {"gain",(ScicosF) blocks_gain},
  {"gensin",(ScicosF) scicos_gensin},
  {"gensqr",(ScicosF) scicos_gensqr},
  {"hltblk",(ScicosF) scicos_hltblk},
  {"hystheresis",(ScicosF) hystheresis},
  {"ifthel",(ScicosF) scicos_ifthel},
  {"integral_func",(ScicosF) integral_func},
  {"integr",(ScicosF) scicos_integr},
  {"intplt",(ScicosF) scicos_intplt},
  {"intpol",(ScicosF) scicos_intpol},
  {"intrp2",(ScicosF) scicos_intrp2},
  {"intrpl",(ScicosF) scicos_intrpl},
  {"invblk",(ScicosF) scicos_invblk},
  {"iocopy",(ScicosF) scicos_iocopy},
  {"logblk",(ScicosF) scicos_logblk},
  {"logicalop",(ScicosF) logicalop},
  {"lookup",(ScicosF) scicos_lookup},
  {"lsplit",(ScicosF) scicos_lsplit},
  {"lusat",(ScicosF) scicos_lusat},
  {"maxblk",(ScicosF) scicos_maxblk},
  {"memo",(ScicosF) scicos_memo},
  {"mfclck",(ScicosF) scicos_mfclck},
  {"minblk",(ScicosF) scicos_minblk},
  {"minmax",(ScicosF) minmax},
  {"modulo_count",(ScicosF) modulo_count},
  {"mscope",(ScicosF) scicos_mscope},
  {"mswitch",(ScicosF) mswitch},
  {"multiplex",(ScicosF) multiplex},
  {"mux",(ScicosF) scicos_mux},
  {"pload",(ScicosF) scicos_pload},
  {"plusblk",(ScicosF) plusblk},
  {"powblk",(ScicosF) scicos_powblk},
  {"prod",(ScicosF) prod},
  {"product",(ScicosF) product},
  {"qzcel",(ScicosF) scicos_qzcel},
  {"qzflr",(ScicosF) scicos_qzflr},
  {"qzrnd",(ScicosF) scicos_qzrnd},
  {"qztrn",(ScicosF) scicos_qztrn},
  {"ramp",(ScicosF) ramp},
  {"ratelimiter",(ScicosF) ratelimiter},
  {"readau",(ScicosF) readau},
  {"readc",(ScicosF) readc},
  /* {"readf",(ScicosF) scicos_readf}, */
  {"relationalop",(ScicosF) relationalop},
  {"relay",(ScicosF) relay},
  {"rndblk",(ScicosF) scicos_rndblk},
  {"samphold",(ScicosF) scicos_samphold},
  {"satur",(ScicosF) satur},
  {"sawtth",(ScicosF) scicos_sawtth},
  {"scalar2vector",(ScicosF) scalar2vector},
  {"scope",(ScicosF) scicos_scope},
  {"scopxy",(ScicosF) scicos_scopxy},
  {"scoxy",(ScicosF) scicos_scoxy},
  {"selblk",(ScicosF) scicos_selblk},
  {"selector",(ScicosF) selector},
  {"signum",(ScicosF) signum},
  {"sinblk",(ScicosF) scicos_sinblk},
  {"sin_blk",(ScicosF) sin_blk},
  {"sinh_blk",(ScicosF) sinh_blk},
  {"slider",(ScicosF) slider},
  {"sqrblk",(ScicosF) scicos_sqrblk},
  {"step_func",(ScicosF) step_func},
  {"sum2",(ScicosF) scicos_sum2},
  {"sum3",(ScicosF) scicos_sum3},
  {"summation",(ScicosF) summation},
  {"sum",(ScicosF) sum},
  {"switch2",(ScicosF) switch2},
  {"switchn",(ScicosF) switchn},
  {"tanblk",(ScicosF) scicos_tanblk},
  {"tan_blk",(ScicosF) tan_blk},
  {"tanh_blk",(ScicosF) tanh_blk},
  {"tcslti",(ScicosF) scicos_tcslti},
  {"tcsltj",(ScicosF) scicos_tcsltj},
  {"timblk",(ScicosF) scicos_timblk},
  {"time_delay",(ScicosF) time_delay},
  {"trash",(ScicosF) scicos_trash},
  {"variable_delay",(ScicosF) variable_delay},
  {"writeau",(ScicosF) writeau},
  {"writec",(ScicosF) writec},
  /* {"writef",(ScicosF) scicos_writef}, */
  {"zcross2",(ScicosF) zcross2},
  {"zcross",(ScicosF) scicos_zcross} ,
  {(char *) 0, (ScicosF) 0}
};


int ntabsim= 126;


#endif 

