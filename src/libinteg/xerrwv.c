/* Nsp
 * Copyright (C) 2005-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * Translated from odepack to C (jpc 2005)
 *
 */

#include <math.h>
#include <stdio.h>
#include <string.h> 
#include "nsp/interf.h"

struct {
    int mesflg, lunit;
} C2F(eh0001);

extern struct {
  int iero;
} C2F(ierode);

/*----------------------------------------------------------------------- 
 * %purpose 
 * subroutines xerrwv, xsetf, and xsetun, as given here, constitute 
 * a simplified version of the slatec error handling package. 
 * written by a. c. hindmarsh at llnl.  version of august 13, 1981. 
 * this version is in double precision. 
 *
 * %calling sequence 
 * all arguments are input arguments. 
 *
 * msg    = the message (character string). 
 * nmes   = the length of msg (not used). 
 * nerr   = the error number (not used). 
 * iert   = the error type.. 
 *          1 means recoverable (control returns to caller). 
 *          2 means fatal (run is aborted--see note below). 
 * ni     = number of ints (0, 1, or 2) to be printed with message. 
 * i1,i2  = ints to be printed, depending on ni. 
 * nr     = number of reals (0, 1, or 2) to be printed with message. 
 * r1,r2  = reals to be printed, depending on nr. 
 *
 * %note.. 
 *    this routine is machine-dependent and specialized for use 
 * in limited context, in the following ways.. 
 * 2. the value of nmes is assumed to be at most 80. 
 *    (multi-line messages are generated by repeated calls.) 
 * 3. if iert = 2, control passes to the statement   stop 
 *    to abort the run.  this statement may be machine-dependent. 
 * 4. r1 and r2 are assumed to be in double precision and are printed 
 *    in d21.13 format. 
 * 5. the common block /eh0001/ below is data-loaded (a machine- 
 *    dependent feature) with default values. 
 *    this block is needed for proper retention of parameters used by 
 *    this routine which the user can reset by calling xsetf or xsetun. 
 *    the variables in this block are as follows.. 
 *       mesflg = print control flag.. 
 *                1 means print all messages (the default). 
 *                0 means no printing. 
 *       lunit  = logical unit number for messages. 
 *                the default is 6 (machine-dependent). 
 *********************************************************************/

int C2F(xerrwv)(char *msg, int *nmes, int *nerr, int *iert, int *ni, int *i1, int *i2, 
		int *nr, double *r1, double *r2, unsigned int  msg_len)
{
  if (C2F(eh0001).mesflg !=  0) 
    {
      int i;
      for (i=0; i < msg_len ; i++) Scierror("%c",msg[i]);
      Scierror("\n");
      if ( *ni == 1) 
	{
	  Scierror("\twhere i1 is :%d\n",*i1);
	}
      else if ( *ni ==  2) 
	{
	  Scierror("\twhere i1 is :%d  and i2 :%d \n",*i1,*i2);
	}
      if ( *nr == 1 ) 
	{
	  Scierror("\twhere r1 is :%g\n",*r1);
	}
      else if ( *nr ==  2) 
	{
	  Scierror("\twhere r1 is :%g  and r2 :%g \n",*r1,*r2);
	}
    }
  /* abort the run if iert = 2.*/
  if ( *iert == 2 ) C2F(ierode).iero = 1;
  return 0;
}


/* 
 *  in this version msg is only the routine name and the message is created from
 *  the message number (which is cumbersome but allows a better presentation) 
 */
int C2F(xerrwvb)(char *msg, int *nmes, int *nerr, int *iert, int *ni, int *i1, int *i2, 
		int *nr, double *r1, double *r2, unsigned int  msg_len)
{
  if (C2F(eh0001).mesflg !=  0) 
    {
      int i;
      for (i=0; i < msg_len ; i++) Sciprintf("%c",msg[i]);

      switch ( *nerr )
	{
	case 1101:
	  Sciprintf("\t t (=%g) and h (=%g) are such that t + h = t\n", *r1, *r2);
	  Sciprintf("\t at next step ; integration stops\n");
	  break;
	case 101:
	  Sciprintf("\t caution t (=%g) and h (=%g) are such that t + h = t\n", *r1, *r2);
	  Sciprintf("\t at next step ; integration continues\n");
	  break;
	case 102:
	  Sciprintf("\t previous message precedent given %d time will no more be repeated\n", *i1);
	  break;
	case 201:
	  Sciprintf("\t at t (=%g), mxstep (=%d) steps needed before reaching tout\n", *r1, *i1);
	  break;
	case 202:
	  Sciprintf("\t at t (=%g), ewt(%d) = %g is <= 0\n", *r1, *i1, *r2);
	  break;
	case 203:
	  Sciprintf("\t at t (=%g), too much precision required w.r.t. machine precision tolsf (=%g)\n", *r1, *r2);
	  break;
	case 205:
	  Sciprintf("\t at t (=%g) for step h (=%g) corrector does not", *r1, *r2);
	  Sciprintf("\t converge with |h| = hmin\n");
	  break;
	}
    }
  
  /* abort the run if iert = 2.*/
  if ( *iert == 2 ) C2F(ierode).iero = 1;
  return 0;
}


int C2F(xsetun)(int *lun)
{
  /* unused */
  return 0;
}

int C2F(xsetf)(int *mflag)
{
  /* this routine resets the print control flag mflag */
  if ( *mflag == 0 || *mflag == 1) C2F(eh0001).mesflg = *mflag;
  return 0;
}
