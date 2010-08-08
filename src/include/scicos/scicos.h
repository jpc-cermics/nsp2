#ifndef __SCICOS_H 
#define __SCICOS_H 

#include "simul.h"

extern scicos_run *Scicos;

/* maximum value for sum of number of inputs 
 * and outputs ports of a given 
 * block of type 2 
 */

#define SZ_SIZE 60

/* maximum value for sum of number of inputs 
 * and outputs of a given block 
 * of type 0 */

#define TB_SIZE 500

typedef void (*voidf)();

#include "scicos-proto.h"

#define do_cold_restart             scicos_do_cold_restart
#define get_phase_simulation        scicos_get_phase_simulation
#define get_scicos_time             scicos_get_scicos_time
#define scicos_get_scicos           scicos_get_scicos_time
#define get_final_time              scicos_get_final_time
#define get_block_number            scicos_get_block_number
#define set_block_number            scicos_set_block_number
#define get_block_error             scicos_get_block_error
#define set_block_error             scicos_set_block_error
#define end_scicos_sim              scicos_end_scicos_sim
#define what_is_hot                 scicos_what_is_hot
#define scicos_getlabel             scicos_get_label

#endif 
