/****************************************************************
Copyright 1990, 1993, 1994 by AT&T, Lucent Technologies and Bellcore.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the names of AT&T, Bell Laboratories,
Lucent or Bellcore or any of their entities not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

AT&T, Lucent and Bellcore disclaim all warranties with regard to
this software, including all implied warranties of
merchantability and fitness.  In no event shall AT&T, Lucent or
Bellcore be liable for any special, indirect or consequential
damages or any damages whatsoever resulting from loss of use,
data or profits, whether in an action of contract, negligence or
other tortious action, arising out of or in connection with the
use or performance of this software.
****************************************************************/

#include "defs.h"

void
warni(char *s, int t)
{
  char buf[100];
  sprintf(buf,s,t);
  warn(buf);
}

void
warn1(char *s, char *t)
{
  char buff[100];
  sprintf(buff, s, t);
  warn(buff);
}

void
warn(char *s)
{
  if(nowarnflag)
    return;
  if (infname && *infname)
    fprintf(diagfile, "Warning on line %ld of %s: %s\n",
	    lineno, infname, s);
  else
    fprintf(diagfile, "Warning on line %ld: %s\n", lineno, s);
  fflush(diagfile);
  ++nwarn;
}

void
errstr(char *s, char *t)
{
  char buff[100];
  sprintf(buff, s, t);
  err(buff);
}


void
erri(char *s, int t)
{
  char buff[100];
  sprintf(buff, s, t);
  err(buff);
}

void
errl(char *s, long t)
{
  char buff[100];
  sprintf(buff, s, t);
  err(buff);
}

char *err_proc = 0;

void
err(char *s)
{
  if (err_proc)
    fprintf(diagfile,
	    "Error processing %s before line %ld",
	    err_proc, lineno);
  else
    fprintf(diagfile, "Error on line %ld", lineno);
  if (infname && *infname)
    fprintf(diagfile, " of %s", infname);
  fprintf(diagfile, ": %s\n", s);
  fflush(diagfile);
  ++nerr;
}

void
yyerror(char *s)
{
  err(s);
}


void
dclerr(char *s, Namep v)
{
  char buff[100];

  if(v)
    {
      sprintf(buff, "Declaration error for %s: %s", v->fvarname, s);
      err(buff);
    }
  else
    errstr("Declaration error %s", s);
}


void
execerr(char *s, char *n)
{
  char buf1[100], buf2[100];

  sprintf(buf1, "Execution error %s", s);
  sprintf(buf2, buf1, n);
  err(buf2);
}


void
Fatal(char *t)
{
  fprintf(diagfile, "Compiler error line %ld", lineno);
  if (infname)
    fprintf(diagfile, " of %s", infname);
  fprintf(diagfile, ": %s\n", t);
  done(3);
}



void
fatalstr(char *t, char *s)
{
  char buff[100];
  sprintf(buff, t, s);
  Fatal(buff);
}


void
fatali(char *t, int d)
{
  char buff[100];
  sprintf(buff, t, d);
  Fatal(buff);
}


void
badthing(char *thing, char *r, int t)
{
  char buff[50];
  sprintf(buff, "Impossible %s %d in routine %s", thing, t, r);
  Fatal(buff);
}


void
badop(char *r, int t)
{
  badthing("opcode", r, t);
}


void
badtag(char *r, int t)
{
  badthing("tag", r, t);
}




void
badstg(char *r, int t)
{
  badthing("storage class", r, t);
}



void
badtype(char *r, int t)
{
  badthing("type", r, t);
}

void
many(char *s, char c, int n)
{
  char buff[250];

  sprintf(buff,
	  "Too many %s.\nTable limit now %d.\nTry rerunning with the -N%c%d option.\n",
	  s, n, c, 2*n);
  Fatal(buff);
}

void
err66(char *s)
{
  errstr("Fortran 77 feature used: %s", s);
  --nerr;
}


void
errext(char *s)
{
  errstr("f2c extension used: %s", s);
  --nerr;
}
