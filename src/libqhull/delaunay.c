/* Nsp 
 * Copyright (C) 2000-2012 Kai Habel
 * This file is adapted from the octave version 
 * Copyright (C) 2000-2017 Kai Habel, 
 *               adapted to nsp: 2017 Jean-Philippe Chancelier
 *  
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
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
 */

/* 
 * 16. July 2000 - Kai Habel: first release
 * 25. September 2002 - Changes by Rafael Laboissiere <rafael@laboissiere.net>
 * Added Qbb option to normalize the input and avoid crashes in Octave.
 * delaunayn accepts now a second (optional) argument that must be a string
 *   containing extra options to the qhull command.
 * Fixed doc string.  The dimension of the result matrix is [m, dim+1], and
 * not [n, dim-1].
 * 6. June 2006: Changes by Alexander Barth <abarth@marine.usf.edu>
 * triangulate non-simplicial facets
 * allow options to be specified as cell array of strings
 * change the default options (for compatibility with matlab)
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <nsp/interf.h> 
#include <nsp/objects.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/matint.h> 
#include <nsp/cells.h> 
#include <nsp/hobj.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 
#include <nsp/qhull.h>

int int_delaunay(Stack stack, int rhs, int opt, int lhs)
{
  FILE *outfile = stdout;
  FILE *errfile = stderr;
  int verbose = FALSE;
  int outfile_created=FALSE;
#ifdef WIN32
  const char *devnul = "NUL";
#else
  const char *devnul = "/dev/null";
#endif
  int ret = RET_BUG;
  char *options;
  NspMatrix *M;
  CheckStdRhs(1,2);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( M->n <= 3)
    options = "Qt Qbb Qc Qz";
  else
    options = "Qt Qbb Qc Qx";
  if (rhs - opt > 1 )
    {
      if ((options = GetString(stack,2)) == ((char *) 0) ) return RET_BUG;
    }

  if (  M->m == M->n + 1 )
    {
      /* one should check if nx points span a simplex I will look at this later.*/
      int i;
      NspMatrix *vec;
      if (( vec = nsp_matrix_create(NVOID,'r',1,M->m)) == NULLMAT)
	return RET_BUG;
      for (i = 0; i < M->m; i++)
        vec->R[i] = i + 1.0;
      MoveObj(stack,1,NSP_OBJECT(vec));
      return 1;
    }
  else if ( M->m > M->n + 1)
    {
      NspMatrix *Mt;
      int exitcode;
      int str_len = strlen("qhull d ")+ strlen(options)+1;
      NspSMatrix *flags;
      // p = p.transpose ();
      int ismalloc = FALSE;
      if ( ( flags =nsp_smatrix_create_with_length(NVOID,1,1,str_len) ) == NULLSMAT)
	return RET_BUG;
      sprintf(flags->S[0],"qhull d %s",options);

      if ((Mt = nsp_matrix_transpose (M)) == NULLMAT)
	{
	  nsp_smatrix_destroy(flags);
	  return RET_BUG;
	}
      
      if (verbose == FALSE )
	{
	  if ((outfile = fopen(devnul,"w"))==NULL)
	    outfile = stdout;
	  else
	    outfile_created= TRUE;
	}

      exitcode = qh_new_qhull (M->n, M->m, Mt->R, 
			       ismalloc,flags->S[0], outfile, errfile);
      if ( outfile_created) fclose(outfile);
      nsp_smatrix_destroy(flags);
      nsp_matrix_destroy(Mt);
      
      if (! exitcode)
        {
          /* triangulate non-simplicial facets */
          qh_triangulate ();

          facetT *facet;
          vertexT *vertex, **vertexp;
          int nf = 0, i = 0;

          FORALLfacets
            {
              if (! facet->upperdelaunay) nf++;
              /* Double check.  Non-simplicial facets will cause segfault below */
              if (! facet->simplicial)
                {
                  Scierror("delaunay: Qhull returned non-simplicial facets -- try delaunayn with different options\n");
                  exitcode = 1;
                  break;
                }
            }

          if (! exitcode)
            {
              NspMatrix *simpl;
	      if (( simpl = nsp_matrix_create(NVOID,'r',nf,M->n+1)) == NULLMAT)
		return RET_BUG;
	      FORALLfacets
                {
                  if (! facet->upperdelaunay)
                    {
                      int j = 0;
                      FOREACHvertex_ (facet->vertices)
                        {
                          simpl->R[i + simpl->m*j] = 1 + qh_pointid(vertex->point);
			  j++;
                        }
                      i++;
                    }
                }
              MoveObj(stack,1,NSP_OBJECT(simpl));
	      ret = 1;
            }
	  
        }
      else
	{
	  Scierror("delaunayn: qhull failed\n");
	}
      
      // Free memory from Qhull
      qh_freeqhull (! qh_ALL);

      int curlong, totlong;
      qh_memfreeshort (&curlong, &totlong);

      if (curlong || totlong)
	{
	  Sciprintf("delaunay: did not free %d bytes of long memory (%d pieces)",
		    totlong, curlong);
	}
    }
  else
    {
      Scierror("Error: delaunay argument has wrong size (m=%d,n=%d) expecting m >= n\n",
	       M->m,M->n);
      return RET_BUG;
    }
  
  return ret;
}
