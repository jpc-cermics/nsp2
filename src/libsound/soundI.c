#include <string.h>
#ifdef WIN32
#include <windows.h>
#pragma comment(lib, "winmm.lib")
#endif

#include "nsp/machine.h"
#include "nsp/interf.h"
#include "sox.h" 
#include "../system/files.h" /* FSIZE+1 */

static int int_savewave(Stack stack, int rhs, int opt, int lhs)
{
  int err=0;
  char buf[FSIZE+1];
  int rate =22050; 
  char *fname;
  NspMatrix *A;
  nsp_option opts[] ={{ "rate",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int_types T[] = {string,realmat,new_opts, t_end} ;

  if ( GetArgs(stack,rhs,opt,T,&fname,&A,&opts,&rate) == FAIL) return RET_BUG;
  nsp_expand_with_exec_dir(&stack,fname,buf);
  /* nsp_path_expand(fname,buf,FSIZE); */
  C2F(savewave)(buf,A->R,&rate,&A->mn,&err);
  if (err >  0) {
    Scierror("%s: internal error \n",NspFname(stack));
    return RET_BUG;
  };
  return 0;
}

static int int_loadwave(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M,*N;
  char *fname;
  char buf[FSIZE+1];
  int n2,err=0;
  WavInfo Wi;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((fname = GetString(stack,1)) == NULLSTRING ) return RET_BUG;
  nsp_expand_with_exec_dir(&stack,fname,buf);
  /* nsp_path_expand(fname,buf,FSIZE); */
  C2F(loadwave)(buf,(double *) 0,&n2,0,&Wi,&err);
  if (err >  0) {
    Scierror("%s: internal error \n",NspFname(stack));
    return RET_BUG;
  };
  if ((M=nsp_matrix_create(NVOID,'r',1,n2))==NULLMAT) return RET_BUG;

  /* FIXME: should be better here to return a hash table 
   * from Wi
   */

  if ((N=nsp_matrix_create(NVOID,'r',1,8))==NULLMAT) return RET_BUG;

  N->R[0] = Wi.wFormatTag;	/* data format */
  N->R[1] = Wi.wChannels;	/* number of channels */
  N->R[2] = Wi.wSamplesPerSecond; /* samples per second per channel */
  N->R[3] = Wi.wAvgBytesPerSec; /* estimate of bytes per second needed */
  N->R[4] = Wi.wBlockAlign;	/* byte alignment of a basic sample block */
  N->R[5] = Wi.wBitsPerSample; /* bits per sample */
  N->R[6] = Wi.data_length;	/* length of sound data in bytes */
  N->R[7] = Wi.bytespersample; /* bytes per sample (per channel) */
  
  C2F(loadwave)(buf,M->R,&M->mn,1,&Wi,&err);
  if (err >  0) {
    Scierror("%s: internal error \n",NspFname(stack));
    return RET_BUG;
  };
  MoveObj(stack,1,NSP_OBJECT(M));
  NSP_OBJECT(M)->ret_pos=1;
  if ( lhs >= 2 ) 
    {
      MoveObj(stack,2,NSP_OBJECT(N));
      NSP_OBJECT(N)->ret_pos=2;
    }
  if ( lhs >= 3)
    {
      if ( nsp_move_string(stack,3,Wi.wav_format,-1)== FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}


static int playsound(char *filename)
{
#ifdef WIN32
  /* Play Sound for windows Allan CORNET 18/01/2004 */
  /* Stop Playing */
  PlaySound(NULL,NULL,SND_PURGE);	
  /* Play Wav file	*/
  PlaySound(filename,NULL,SND_ASYNC|SND_FILENAME);
  return 0;
#else 
  /* FIXME: to be improved with g_spawn 
   *        using play from sox 
   */
  char system_cmd[FILENAME_MAX+10];
  int rep ;
  sprintf(system_cmd,"play  %s",filename);
  rep = system(system_cmd);
  return rep;
#endif 
}

static int int_playsound(Stack stack, int rhs, int opt, int lhs)
{
  char *fname;
  char buf[FSIZE+1];
  int rep;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((fname = GetString(stack,1)) == NULLSTRING ) return RET_BUG;
  nsp_expand_with_exec_dir(&stack,fname,buf);
  /* nsp_path_expand(fname,buf,FSIZE); */
  rep = playsound(buf);
  if ( lhs == 1 ) 
    {
      if ( nsp_move_double(stack,1,(double) rep)==FAIL) return RET_BUG;
      return 1;
    }
  if ( rep == -1 ) 
    {
      Scierror("%s: internal error\n",NspFname(stack));
      return RET_BUG;
    }
  return 0;
}


static OpTab Sound_func[]={
  {"loadwav",int_loadwave},
  {"savewav",int_savewave},
  {"play",int_playsound},
  {(char *) 0, NULL}
};

int Sound_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Sound_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Sound_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Sound_func[i].name;
  *f = Sound_func[i].fonc;
}

