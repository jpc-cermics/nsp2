
/* $Id$ */

/*
 *         PVM version 3.4:  Parallel Virtual Machine System
 *               University of Tennessee, Knoxville TN.
 *           Oak Ridge National Laboratory, Oak Ridge TN.
 *                   Emory University, Atlanta GA.
 *      Authors:  J. J. Dongarra, G. E. Fagg, M. Fischer
 *          G. A. Geist, J. A. Kohl, R. J. Manchek, P. Mucci,
 *         P. M. Papadopoulos, S. L. Scott, and V. S. Sunderam
 *                   (C) 1997 All Rights Reserved
 *
 *                              NOTICE
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted
 * provided that the above copyright notice appear in all copies and
 * that both the copyright notice and this permission notice appear in
 * supporting documentation.
 *
 * Neither the Institutions (Emory University, Oak Ridge National
 * Laboratory, and University of Tennessee) nor the Authors make any
 * representations about the suitability of this software for any
 * purpose.  This software is provided ``as is'' without express or
 * implied warranty.
 *
 * PVM version 3 was funded in part by the U.S. Department of Energy,
 * the National Science Foundation and the State of Tennessee.
 */

/*
 *	lpvmmisc.h
 *
 *	Misc library defines
 *
 * $Log$
 * Revision 1.1.1.1  2003/11/14 13:02:08  stochopt
 * Imported files
 *
 * Revision 1.1.1.1  2003/02/18 09:26:05  stochopt
 * initial
 *
 * Revision 1.1.1.1  2003/02/17 11:25:32  stochopt
 * initial
 *
 * Revision 1.1.1.1  2003/02/03 08:42:11  stochopt
 * imported sources
 *
 * Revision 1.1.1.1  2003/01/06 09:04:12  stochopt
 * Imported sources
 *
 * Revision 1.1.1.1  2002/12/19 17:37:37  stochopt
 * start
 *
 * Revision 1.2  2002/10/14 14:37:48  chanceli
 * update
 *
 * Revision 1.4  1999/07/08 19:00:21  kohl
 * Fixed "Log" keyword placement.
 * 	- indent with " * " for new CVS.
 *
 * Revision 1.3  1997/06/25  22:08:52  pvmsrc
 * Markus adds his frigging name to the author list of
 * 	every file he ever looked at...
 *
 * Revision 1.2  1997/01/28  19:27:59  pvmsrc
 * New Copyright Notice & Authors.
 *
 * Revision 1.1  1996/09/23  23:43:20  pvmsrc
 * Initial revision
 *
 */

/*
*	keep a self - notify request
*/

struct notreq {
	struct notreq *n_link;		/* chain or 0 */
	struct notreq *n_rlink;
	int n_ctx;
	int n_tag;
	int n_count;
};

