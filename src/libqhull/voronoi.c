/* Nsp 
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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <nsp/nsp.h>
#include <nsp/objects.h> 
#include <nsp/interf.h>
#include <nsp/cells.h> 
#include <nsp/qhull.h>
/*
 * 
 */

int int_voronoi(Stack stack, int rhs, int opt, int lhs)
{
  char *options=NULL;
  int verbose = FALSE;
  int outfile_created=FALSE;
#ifdef WIN32
  const char *devnul = "NUL";
#else
  const char *devnul = "/dev/null";
#endif
  int rep = TRUE;
  int i;
  NspMatrix *M;
  int num_voronoi_regions,num_voronoi_vertices;
  FILE *outfile = stdout;
  FILE *errfile = stderr;
  int exitcode;
  char cmd1[]= "qhull v Qbb";
  char cmd2[]= "qhull v Qbb Qx";
  boolT ismalloc = False;
  int dim, num_points;
  CheckStdRhs (2,3);
  CheckLhs (1,3);
  /* we ignore the first argument */
  if ((M = GetRealMat (stack, 2)) == NULLMAT)
    return RET_BUG;
  dim = M->m  ; 
  num_points = M->n;
  options = (dim <= 4 ) ? cmd1: cmd2;
  if (rhs - opt > 2 )
    {
      if ((options = GetString(stack,3)) == ((char *) 0) ) return RET_BUG;
    }
  
  if (num_points <= dim)
    {
      Scierror ("%s: number of points must be greater than their dimension\n",
		NspFname(stack));
      return RET_BUG;
    }
  
  if (verbose == FALSE )
    {
      if ((outfile = fopen(devnul,"w"))==NULL)
	outfile = stdout;
      else
	outfile_created= TRUE;
    }
  
  exitcode = qh_new_qhull (dim, num_points, M->R, ismalloc, options, outfile, errfile);
  
  if ( outfile_created) fclose(outfile);
  
  if (! exitcode) 
    {
      // Calling findgood_all provides the number of Voronoi vertices
      // (sets qh num_good).
      qh_findgood_all (qh facet_list);
      num_voronoi_regions = qh num_vertices - qh_setsize (qh del_vertices);
      num_voronoi_vertices = qh num_good;
      // Find the voronoi centers for all facets.
      qh_setvoronoi_all ();
      facetT *facet;
      vertexT *vertex;
      int k;
      
      // Find the number of Voronoi vertices for each Voronoi cell and
      // store them in NI so we can use them later to set the dimensions
      // of the RowVector objects used to collect them.

      FORALLfacets
        {
          facet->seen = False;
        }
      
      NspMatrix *Ni = nsp_matrix_create(NVOID,'r',num_voronoi_regions,1);
      int *ni = Ni->I;
      for ( i = 0; i < num_voronoi_regions; i++)  ni[i] = 0;

      k = 0;

      FORALLvertices
        {
          if (qh hull_dim == 3)
            qh_order_vertexneighbors (vertex);
          
          int infinity_seen = False;

          facetT *neighbor, **neighborp;

          FOREACHneighbor_ (vertex)
            {
              if (neighbor->upperdelaunay)
                {
                  if (! infinity_seen)
                    {
                      infinity_seen = True;
                      ni[k]++;
                    }
                }
              else
                {
                  neighbor->seen = True;
                  ni[k]++;
                }
            }

          k++;
        }

      // If Qhull finds fewer regions than points, we will pad the end
      // of the at_inf and C arrays so that they always contain at least
      // as many elements as the given points array.

      // FIXME -- is it possible (or does it make sense) for
      // num_voronoi_regions to ever be larger than num_points?

      int nr = (num_points > num_voronoi_regions
		? num_points : num_voronoi_regions);
      
      NspBMatrix *At_inf = nsp_bmatrix_create(NVOID, nr ,1);
      int *at_inf = At_inf->B;
      int d;
      for ( d = 0; d < At_inf->mn ;  d++) At_inf->B[d]=FALSE;
      
      // The list of Voronoi vertices.  The first element is always
      // Inf.
      NspMatrix *F = nsp_matrix_create(NVOID,'r',num_voronoi_vertices+1, dim);
      double zero=0.0;
      for ( d = 0; d < dim; d++)
        F->R[0+F->m*d] = 1/zero;
      
      // The cell array of vectors of indices into F that represent the
      // vertices of the Voronoi regions (cells).
      
      NspCells *C = nsp_cells_create(NVOID,nr, 1);

      // Now loop through the list of vertices again and store the
      // coordinates of the Voronoi vertices and the lists of indices
      // for the cells.

      FORALLfacets
        {
          facet->seen = False;
        }

      i = 0;
      k = 0;

      FORALLvertices
        {
          if (qh hull_dim == 3) qh_order_vertexneighbors (vertex);
          int infinity_seen = False;
          int idx = qh_pointid (vertex->point);
          int num_vertices = ni[k++];

          // Qhull seems to sometimes produces regions with a single
          // vertex.  Is that a bug?  How can a region have just one
          // vertex?  Let's skip it.

          if (num_vertices == 1) continue;

	  NspMatrix *Facet_list= nsp_matrix_create("ce",'r',1,num_vertices);
          int m = 0;
          facetT *neighbor, **neighborp;

          FOREACHneighbor_(vertex)
            {
              if (neighbor->upperdelaunay)
                {
                  if (! infinity_seen)
                    {
                      infinity_seen = True;
                      Facet_list->R[m++] = 1;
                      at_inf[idx] = TRUE;
                    }
                }
              else
                {
                  if (! neighbor->seen)
                    {
                      i++;
                      for ( d = 0; d < dim; d++)
                        F->R[i+F->m*d] = neighbor->center[d];

                      neighbor->seen = True;
                      neighbor->visitid = i;
                    }

                  Facet_list->R[m++] = neighbor->visitid + 1;
                }
            }

          C->objs[idx] = (NspObject *) Facet_list;
        }
      MoveObj(stack,1,NSP_OBJECT(F));
      if ( lhs >= 2 ) 
	MoveObj(stack,2,NSP_OBJECT(C));
      if ( lhs >= 3) 	
	MoveObj(stack,3,NSP_OBJECT(At_inf));
    }
  else
    {
      rep = FALSE;
      Scierror("Error: qhull failed\n");
    }

  // Free memory from Qhull
  qh_freeqhull (! qh_ALL);
  int curlong, totlong;
  qh_memfreeshort (&curlong, &totlong);

  if (curlong || totlong)
    {
      Sciprintf("Warning: qhull did not free %d bytes of long memory (%d pieces)",
	       totlong, curlong);
    }
  if ( rep == FALSE ) return RET_BUG;
  return Max(lhs,1);
}
