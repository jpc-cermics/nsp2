/*------------------------------------------------------------------------
 *    Copyright (C) 2001-2003 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *    
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>

#include "nsp/object.h"
#include "command.h"
#include "All-extern.h"
#include "../system/Sun.h"

/*---------------------------------------------------------------------
 *  This function is used to store Scilab command in a queue 
 *  (implemented with a NspList XXXX)
 *  The queue is filled by dynamic buttons and menus handlers. 
 *  The default behaviour is the following, the interpreter 
 *  checks for available commands when it is idle or during 
 *  the course of expression evaluation. 
 *
 *  One can also set a specific handler to change this default 
 *  evaluation behaviour. 
 *
 *  set_scig_command_handler(Scig_command_handler f) : 
 *           set a specific handler for command handling 
 *  reset_scig_command_handler() : reset to default state 
 *           (the use of the previous function is recommended )
 *  int enqueue_nsp_command( char *command)
 *  NspObject *dequeue_nsp_command() 
 *---------------------------------------------------------------------*/

int scig_command_handler_none (char *command) {return 0;}

static Scig_command_handler scig_command_handler = scig_command_handler_none;

Scig_command_handler set_scig_command_handler(Scig_command_handler f)
{
  Scig_command_handler old = scig_command_handler;
  scig_command_handler = f;
  return old;
}

void reset_scig_command_handler() 
{
  scig_command_handler = scig_command_handler_none;
}

/*---------------------------------------------------------------
 * try to execute a command or add it to the end of command queue 
 * XXXX global variable 
 *----------------------------------------------------------------*/

static int locked = FALSE;
static int initialized = FALSE;
static NspList *L = NULLLIST;

int enqueue_nsp_command(char *command)
{
  NspObject *O;
  /** first check if we have a special handler set for commands **/
  if ( scig_command_handler(command) == 1) return 0;
  /*
   *  if scilab is at the prompt level 
   *  using write_scilab will entre command at the prompt level 
   *  as if user has entered it 
   */
  /* if (get_is_reading()) 
    { 
      write_scilab(command);
      return 0;
    }
  */
  /* store the command in a queue 
   * command will be executed by scilab 
   */
  if ( initialized == FALSE ) 
    {
      if ((L = EListCreate(NVOID,NULLSTRING))==NULLLIST) return FAIL;
      initialized = TRUE;
    }
  if (( O = ObjStr(command))==NULLOBJ ) return FAIL;
  if ( EndInsert(L,O) == FAIL ) return FAIL;
  return OK;
}

/*---------------------------------------------------------------------------
 * check queue 
 *---------------------------------------------------------------------------*/

int checkqueue_nsp_command() 
{
  if ( locked) return FALSE;
  if ( L == NULLLIST)  return FALSE;
  if ( ListLength(L) < 1 )  return FALSE;
  return ( NthElement(L,1) != NULLOBJ);
}

/*---------------------------------------------------------------------------
 * lock the queue (checkqueue_nsp_command() will return FALSE)
 *---------------------------------------------------------------------------*/

int lockqueue_nsp_command() 
{
  locked = TRUE;
}

int unlockqueue_nsp_command() 
{
  locked = FALSE;
}

/*---------------------------------------------------------------------------
 * returns a command or NULLOBJ if queue is empty 
 *---------------------------------------------------------------------------*/

static NspObject *dequeue_nsp_command_obj() 
{
  NspObject *O;
  if ( L == NULLLIST) return NULLOBJ;
  if ( ListLength(L) < 1 )  return FALSE;
  O = NthElement(L,1);
  if ( O != NULLOBJ)  DeleteNthCellOnly(L,1);
  return O;
}

/*---------------------------------------------------------------------------
 * returns a command or NULLOBJ if queue is empty 
 *---------------------------------------------------------------------------*/

int dequeue_nsp_command(char *buf,int buf_len)
{
  NspObject *O = dequeue_nsp_command_obj() ;
  if ( O == NULLOBJ) return FAIL;
  strncpy(buf,((NspSMatrix *) O)->S[0],buf_len);
  ObjDestroy(&O);
  return OK;
}


