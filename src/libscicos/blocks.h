#ifndef __SCICOS_BLOCKS__ 
#define __SCICOS_BLOCKS__ 

/*
 * Copyright INRIA
 * table of functions associated to predefined blocks 
 */

extern void scicos_evtdly(ARGS_scicos);
extern void scicos_cstblk(ARGS_scicos);
extern void scicos_lusat(ARGS_scicos);
extern void scicos_pload(ARGS_scicos);
extern void scicos_qzcel(ARGS_scicos);
extern void scicos_qzflr(ARGS_scicos);
extern void scicos_qzrnd(ARGS_scicos);
extern void scicos_qztrn(ARGS_scicos);
extern void scicos_scope(ARGS_scicos);
extern void scicos_lsplit(ARGS_scicos);
extern void scicos_csslti(ARGS_scicos);
extern void scicos_dsslti(ARGS_scicos);
extern void scicos_trash(ARGS_scicos);
extern void scicos_zcross(ARGS_scicos);
extern void scicos_expblk(ARGS_scicos);
extern void scicos_logblk(ARGS_scicos);
extern void scicos_sinblk(ARGS_scicos);
extern void scicos_tanblk(ARGS_scicos);
extern void scicos_powblk(ARGS_scicos);
extern void scicos_sqrblk(ARGS_scicos);
extern void scicos_delay(ARGS_scicos);
extern void scicos_selblk(ARGS_scicos);
extern void scicos_forblk(ARGS_scicos);
extern void scicos_writef(ARGS_scicos);
extern void scicos_invblk(ARGS_scicos);
extern void scicos_hltblk(ARGS_scicos);
extern void scicos_gensin(ARGS_scicos);
extern void scicos_rndblk(ARGS_scicos);
extern void scicos_lookup(ARGS_scicos);
extern void scicos_timblk(ARGS_scicos);
extern void scicos_gensqr(ARGS_scicos);
extern void scicos_mfclck(ARGS_scicos);
extern void scicos_sawtth(ARGS_scicos);
extern void scicos_tcslti(ARGS_scicos);
extern void scicos_tcsltj(ARGS_scicos);
extern void scicos_scopxy(ARGS_scicos);
extern void scicos_evscpe(ARGS_scicos);
extern void scicos_integr(ARGS_scicos);
extern void scicos_readf(ARGS_scicos);
extern void scicos_affich(ARGS_scicos);
extern void scicos_intpol(ARGS_scicos);
extern void scicos_intplt(ARGS_scicos);
extern void scicos_minblk(ARGS_scicos);
extern void scicos_maxblk(ARGS_scicos);
extern void scicos_dlradp(ARGS_scicos);
extern void scicos_iocopy(ARGS_scicos);
extern void scicos_sum2(ARGS_scicos);
extern void scicos_sum3(ARGS_scicos);
extern void scicos_delayv(ARGS_scicos);
extern void scicos_mux(ARGS_scicos);
extern void scicos_demux(ARGS_scicos);
extern void scicos_samphold(ARGS_scicos);
extern void scicos_dollar(ARGS_scicos);
extern void scicos_mscope(ARGS_scicos);
extern void scicos_intrp2(ARGS_scicos);
extern void scicos_intrpl(ARGS_scicos);
extern void scicos_fsv(ARGS_scicos);
extern void scicos_memo(ARGS_scicos);
extern void scicos_fscope(ARGS_scicos);
extern void scicos_scoxy(ARGS_scicos);
extern void scicos_diffblk(ARGS_scicos);
extern void scicos_constraint(ARGS_scicos);
extern void blocks_absblk(ARGS_scicos);
extern void blocks_andlog(ARGS_scicos);
extern void blocks_bidon(ARGS_scicos);
extern void blocks_gain(ARGS_scicos);
extern void blocks_cdummy(ARGS_scicos);
extern void blocks_dband(ARGS_scicos);
extern void blocks_cosblk(ARGS_scicos);
extern void scicos_ifthel(ARGS_scicos);
extern void scicos_eselect(ARGS_scicos);
extern void selector (ARGS_scicos);
extern void sum (ARGS_scicos);
extern void prod (ARGS_scicos);
extern void switchn (ARGS_scicos);
extern void relay (ARGS_scicos);
extern void readc (ARGS_scicos);
extern void writec (ARGS_scicos);
extern void writeau (ARGS_scicos);
extern void readau (ARGS_scicos);
extern void plusblk (ARGS_scicos);
extern void slider (ARGS_scicos);
extern void zcross2 (ARGS_scicos);
extern void mswitch (ARGS_scicos);
extern void logicalop (ARGS_scicos);
extern void switch2 (ARGS_scicos);
extern void variable_delay (ARGS_scicos);
extern void time_delay (ARGS_scicos);
extern void cscope (ARGS_scicos);
extern void cmscope (ARGS_scicos);
extern void satur (ARGS_scicos);
extern void step_func (ARGS_scicos);
extern void integral_func (ARGS_scicos);
extern void absolute_value (ARGS_scicos);
extern void bounce_ball (ARGS_scicos);
extern void bouncexy (ARGS_scicos);
extern void extractor (ARGS_scicos);
extern void scalar2vector (ARGS_scicos);
extern void minmax (ARGS_scicos);
extern void signum (ARGS_scicos);
extern void product (ARGS_scicos);
extern void summation (ARGS_scicos);
extern void multiplex (ARGS_scicos);
extern void gainblk (ARGS_scicos);
extern void relationalop (ARGS_scicos);
extern void modulo_count (ARGS_scicos);
extern void hystheresis (ARGS_scicos);
extern void ratelimiter (ARGS_scicos);
extern void backlash (ARGS_scicos);
extern void deadband (ARGS_scicos);
extern void ramp (ARGS_scicos);
extern void evaluate_expr (ARGS_scicos);
extern void deriv (ARGS_scicos);
extern void sin_blk (ARGS_scicos);
extern void cos_blk (ARGS_scicos);
extern void tan_blk (ARGS_scicos);
extern void asin_blk (ARGS_scicos);
extern void acos_blk (ARGS_scicos);
extern void atan_blk (ARGS_scicos);
extern void sinh_blk (ARGS_scicos);
extern void cosh_blk (ARGS_scicos);
extern void tanh_blk (ARGS_scicos);
extern void asinh_blk (ARGS_scicos);
extern void acosh_blk (ARGS_scicos);
extern void atanh_blk (ARGS_scicos);
extern void evtvardly (ARGS_scicos);
 
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


int ntabsim= 126 ;


#endif 

