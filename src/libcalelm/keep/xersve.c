/* xersve.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

#if 0
/* Table of constant values */

static int c__1 = 1;

/*DECK XERSVE 
 */
int
nsp_calpack_xersve (char *librar, char *subrou, char *messg, int *kflag,
		    int *nerr, int *level, int *icount, long int librar_len,
		    long int subrou_len, long int messg_len)
{
  /* Initialized data */

  static int kountx = 0;
  static int nmsg = 0;

  /* Format strings */
  static char fmt_9010[] = "(1x,a,3x,a,3x,a,3i10)";
  static char fmt_9020[] =
    "(\0020OTHER ERRORS NOT INDIVIDUALLY TABULATED = \002,i10)";

  /* System generated locals */
  int i__1;
  icilist ici__1;

  /* Builtin functions */
  int s_wsfi (icilist *), do_fio (int *, char *, long int), e_wsfi (void);
  int s_copy (char *, char *, long int, long int);
  int s_cmp (char *, char *, long int, long int);

  /* Local variables */
  int i__;
  char cbuff[148];
  static int kount[10];
  int io;
  static char libtab[8 * 10], mestab[20 * 10];
  static int nertab[10], levtab[10];
  static char subtab[8 * 10];
  char lib[8], mes[20], sub[8];
  double wte;

  /****BEGIN PROLOGUE  XERSVE 
   ****SUBSIDIARY 
   ****PURPOSE  Record that an error has occurred. 
   ****LIBRARY   SLATEC (XERROR) 
   ****CATEGORY  R3 
   ****TYPE      ALL (XERSVE-A) 
   ****KEYWORDS  ERROR, XERROR 
   ****AUTHOR  Jones, R. E., (SNLA) 
   ****DESCRIPTION 
   * 
   **Usage: 
   * 
   *       INT  KFLAG, NERR, LEVEL, ICOUNT 
   *       CHARACTER * (len) LIBRAR, SUBROU, MESSG 
   * 
   *       CALL XERSVE (LIBRAR, SUBROU, MESSG, KFLAG, NERR, LEVEL, ICOUNT) 
   * 
   **Arguments: 
   * 
   *       LIBRAR :IN    is the library that the message is from. 
   *       SUBROU :IN    is the subroutine that the message is from. 
   *       MESSG  :IN    is the message to be saved. 
   *       KFLAG  :IN    indicates the action to be performed. 
   *                     when KFLAG > 0, the message in MESSG is saved. 
   *                     when KFLAG=0 the tables will be dumped and 
   *                     cleared. 
   *                     when KFLAG < 0, the tables will be dumped and 
   *                     not cleared. 
   *       NERR   :IN    is the error number. 
   *       LEVEL  :IN    is the error severity. 
   *       ICOUNT :OUT   the number of times this message has been seen, 
   *                     or zero if the table has overflowed and does not 
   *                     contain this message specifically.  When KFLAG=0, 
   *                     ICOUNT will not be altered. 
   * 
   **Description: 
   * 
   *  Record that this error occurred and possibly dump and clear the 
   *  tables. 
   * 
   ****REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC 
   *                Error-handling Package, SAND82-0800, Sandia 
   *                Laboratories, 1982. 
   ****ROUTINES CALLED  I1MACH, XGETUA 
   ****REVISION HISTORY  (YYMMDD) 
   *  800319  DATE WRITTEN 
   *  861211  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900413  Routine modified to remove reference to KFLAG.  (WRB) 
   *  900510  Changed to add LIBRARY NAME and SUBROUTINE to calling 
   *          sequence, use IF-THEN-ELSE, make number of saved entries 
   *          easily changeable, changed routine name from XERSAV to 
   *          XERSVE.  (RWC) 
   *  910626  Added LIBTAB and SUBTAB to SAVE statement.  (BKS) 
   *  920501  Reformatted the REFERENCES section.  (WRB) 
   ****END PROLOGUE  XERSVE 
   */
  /****FIRST EXECUTABLE STATEMENT  XERSVE 
   * 
   */
  if (*kflag <= 0)
    {
      /* 
       *       Dump the table. 
       * 
       */
      if (nmsg == 0)
	{
	  return 0;
	}
      /* 
       *       Print to each unit. 
       * 
       */
      nsp_calpack_basout (&io, &wte, "0          ERROR MESSAGE SUMMARY", 32L);
      nsp_calpack_basout (&io, &wte,
			  " LIBRARY    SUBROUTINE MESSAGE START             NERR     LEVEL     COUNT",
			  73L);
      i__1 = nmsg;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  ici__1.icierr = 0;
	  ici__1.icirnum = 1;
	  ici__1.icirlen = 148;
	  ici__1.iciunit = cbuff;
	  ici__1.icifmt = fmt_9010;
	  s_wsfi (&ici__1);
	  do_fio (&c__1, libtab + (i__ - 1 << 3), 8L);
	  do_fio (&c__1, subtab + (i__ - 1 << 3), 8L);
	  do_fio (&c__1, mestab + (i__ - 1) * 20, 20L);
	  do_fio (&c__1, (char *) &nertab[i__ - 1], (long int) sizeof (int));
	  do_fio (&c__1, (char *) &levtab[i__ - 1], (long int) sizeof (int));
	  do_fio (&c__1, (char *) &kount[i__ - 1], (long int) sizeof (int));
	  e_wsfi ();
	  nsp_calpack_basout (&io, &wte, cbuff, 148L);
	  /* L10: */
	}
      if (kountx != 0)
	{
	  ici__1.icierr = 0;
	  ici__1.icirnum = 1;
	  ici__1.icirlen = 148;
	  ici__1.iciunit = cbuff;
	  ici__1.icifmt = fmt_9020;
	  s_wsfi (&ici__1);
	  do_fio (&c__1, (char *) &kountx, (long int) sizeof (int));
	  e_wsfi ();
	  nsp_calpack_basout (&io, &wte, cbuff, 148L);
	}
      nsp_calpack_basout (&io, &wte, " ", 1L);
      /*STD         CALL XGETUA (LUN, NUNIT) 
       *STD         DO 20 KUNIT = 1,NUNIT 
       *STD            IUNIT = LUN(KUNIT) 
       *STD            IF (IUNIT.EQ.0) IUNIT = I1MACH(4) 
       *STDC 
       *STDC           Print the table header. 
       *STDC 
       *STD            WRITE (IUNIT,9000) 
       *STDC 
       *STDC           Print body of table. 
       *STDC 
       *STD            DO 10 I = 1,NMSG 
       *STD               WRITE (IUNIT,9010) LIBTAB(I), SUBTAB(I), MESTAB(I), 
       *STD     *            NERTAB(I),LEVTAB(I),KOUNT(I) 
       *STD   10       CONTINUE 
       *STDC 
       *STDC           Print number of other errors. 
       *STDC 
       *STD            IF (KOUNTX.NE.0) WRITE (IUNIT,9020) KOUNTX 
       *STD            WRITE (IUNIT,9030) 
       *STD   20    CONTINUE 
       * 
       *       Clear the error tables. 
       * 
       */
      if (*kflag == 0)
	{
	  nmsg = 0;
	  kountx = 0;
	}
    }
  else
    {
      /* 
       *       PROCESS A MESSAGE... 
       *       SEARCH FOR THIS MESSG, OR ELSE AN EMPTY SLOT FOR THIS MESSG, 
       *       OR ELSE DETERMINE THAT THE ERROR TABLE IS FULL. 
       * 
       */
      s_copy (lib, librar, 8L, librar_len);
      s_copy (sub, subrou, 8L, subrou_len);
      s_copy (mes, messg, 20L, messg_len);
      i__1 = nmsg;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (s_cmp (lib, libtab + (i__ - 1 << 3), 8L, 8L) == 0
	      && s_cmp (sub, subtab + (i__ - 1 << 3), 8L, 8L) == 0
	      && s_cmp (mes, mestab + (i__ - 1) * 20, 20L, 20L) == 0
	      && *nerr == nertab[i__ - 1] && *level == levtab[i__ - 1])
	    {
	      ++kount[i__ - 1];
	      *icount = kount[i__ - 1];
	      return 0;
	    }
	  /* L30: */
	}
      /* 
       */
      if (nmsg < 10)
	{
	  /* 
	   *          Empty slot found for new message. 
	   * 
	   */
	  ++nmsg;
	  s_copy (libtab + (i__ - 1 << 3), lib, 8L, 8L);
	  s_copy (subtab + (i__ - 1 << 3), sub, 8L, 8L);
	  s_copy (mestab + (i__ - 1) * 20, mes, 20L, 20L);
	  nertab[i__ - 1] = *nerr;
	  levtab[i__ - 1] = *level;
	  kount[i__ - 1] = 1;
	  *icount = 1;
	}
      else
	{
	  /* 
	   *          Table is full. 
	   * 
	   */
	  ++kountx;
	  *icount = 0;
	}
    }
  return 0;
  /* 
   *    Formats. 
   * 
   */
  /* L9000: */
  /* L9030: */
}				/* xersve_ */
#endif
