/* Nsp 
 * This file is adapted from the octave version 
 * Copyright (C) 2000-2017 Kai Habel
 * 
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
 * 29. July 2000 - Kai Habel: first release
 * 2002-04-22 Paul Kienzle
 *   Use warning(...) function rather than writing to cerr
 * 2006-05-01 Tom Holroyd
 * add support for consistent winding in all dimensions; output is 
 * guaranteed to be simplicial.
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <nsp/nsp.h>
#include <nsp/objects.h> 
#include <nsp/interf.h>
#include <nsp/file.h> 
#include <nsp/type.h> 
#include <nsp/qhull.h>

/* convhulln interface */

int int_convhulln(Stack stack, int rhs, int opt, int lhs)
{
  int use_2d = FALSE;
  int outfile_created=FALSE;
#ifdef WIN32
  const char *devnul = "NUL";
#else
  const char *devnul = "/dev/null";
#endif
  int verbose = FALSE;
  int ismalloc = FALSE;
  FILE *outfile = stdout;
  FILE *errfile = stderr;
  char *options;
  NspMatrix *M,*Mt;
  int str_len, dim, num_points;
  NspSMatrix *flags;
  nsp_option opts[] ={{"use_2d",s_bool,NULLOBJ,-1},
		      {"verbose",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  
  CheckStdRhs(1,2);
  CheckLhs(0,2);
  if ((M = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;

  dim = M->n;
  num_points= M->m;
  
  if (rhs - opt > 1 )
    {
      if ((options = GetString(stack,2)) == ((char *) 0) ) return RET_BUG;
    }
  else
    {
      if (dim <= 4)
	options = " Qt";
      else
	options = " Qt Qx";
    }
  if ( get_optional_args(stack, rhs, opt, opts, &use_2d,&verbose) == FAIL )
    return RET_BUG;
  str_len = strlen("qhull ")+ strlen(options)+1;
  if ( ( flags =nsp_smatrix_create_with_length(NVOID,1,1,str_len) ) == NULLSMAT)
    return RET_BUG;
  sprintf(flags->S[0],"qhull %s",options);
  
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
  
  int exitcode = qh_new_qhull (dim, num_points, Mt->R, ismalloc, flags->S[0], outfile, errfile);

  if ( outfile_created) fclose(outfile);
    
  if (! exitcode)
    {
      int nonsimp_seen = FALSE;
      int nf = qh num_facets;
      facetT *facet;
      int i = 0;
      NspMatrix *idx;
      if (( idx = nsp_matrix_create(NVOID,'r',nf,dim+1)) == NULLMAT)
	return RET_BUG;
      FORALLfacets
        {
          int  j = 0;
          if (! nonsimp_seen && ! facet->simplicial)
            {
              nonsimp_seen = TRUE;
	      /* 
              if ( cmd.find ("QJ") != std::string::npos)
                {
		  // Should never happen with QJ.
                  error ("convhulln: qhull failed: option 'QJ' returned non-simplicial facet");
                  return retval;
                }
	      */
            }
#define idx(i,j) idx->R[i+idx->m*j]
          if (dim == 3)
            {
              setT *vertices = qh_facet3vertex (facet);
              vertexT *vertex, **vertexp;
              FOREACHvertex_ (vertices)
                idx(i, j++) = 1 + qh_pointid(vertex->point);
              qh_settempfree (&vertices);
            }
          else
            {
              if (facet->toporient ^ qh_ORIENTclock)
                {
                  vertexT *vertex, **vertexp;
                  FOREACHvertex_ (facet->vertices)
                    idx(i, j++) = 1 + qh_pointid(vertex->point);
                }
              else
                {
                  vertexT *vertex, **vertexp;
                  FOREACHvertexreverse12_ (facet->vertices)
                    idx(i, j++) = 1 + qh_pointid(vertex->point);
                }
            }
#undef idx
          if (j < dim)
            Sciprintf("convhulln: facet %d only has %d vertices", i, j);
          i++;
        }
            
      /* Remove extra dimension if all facets were simplicial. */
      if (! nonsimp_seen )
	{
	  if (nsp_matrix_resize (idx, nf , dim ) != OK)
	    return RET_BUG;
	}

      /* if use_2d is true we just return a vector giving the 
       * path of 2d convex-hull in order to return values 
       * compatible with convhull 
       */
      if ( use_2d && dim == 2 && idx->n == 2 )
	{
	  int current;
	  NspMatrix *Temp;
	  if (( Temp = nsp_matrix_create(NVOID,'r',nf+1,1)) == NULLMAT)
	    return RET_BUG;
	  Temp->R[0]= idx->R[0];
	  current = idx->R[idx->m];
	  for ( i = 1 ; i < nf ; i++)
	    {
	      int j;
	      Temp->R[i]= current;
	      for ( j = 0 ; j < nf ; j++)
		if ( idx->R[j]== current) { current = idx->R[j+idx->m];break;}
	    }
	  Temp->R[nf]= Temp->R[0];
	  nsp_matrix_destroy(idx);
	  idx=Temp;
	}
      MoveObj(stack,1,NSP_OBJECT(idx));
      if ( lhs == 2 )
        {
          /* Calculate volume of convex hull, taken from qhull src/geom2.c. */
	  realT area;
          realT dist;
          FORALLfacets
            {
              if (! facet->normal)
                continue;
              if (facet->upperdelaunay && qh ATinfinity)
                continue;
              facet->f.area = area = qh_facetarea (facet);
              facet->isarea = True;
              if (qh DELAUNAY)
                {
                  if (facet->upperdelaunay == qh UPPERdelaunay)
                    qh totarea += area;
                }
              else
                {
                  qh totarea += area;
                  qh_distplane (qh interior_point, facet, &dist);
                  qh totvol += -dist * area/ qh hull_dim;
                }
            }
	  nsp_move_double(stack,2,qh totvol);
        }
    }
  else
    {
      Scierror("convhulln: qhull failed\n");
      return RET_BUG;
    }
  
  qh_freeqhull (! qh_ALL);
  int curlong, totlong;
  qh_memfreeshort (&curlong, &totlong);
  if (curlong || totlong)
    Sciprintf("convhulln: did not free %d bytes of long memory (%d pieces)\n",
	      totlong, curlong);
  return Max(lhs,1);
}

/*
 cube = [0 0 0;1 0 0;1 1 0;0 1 0;0 0 1;1 0 1;1 1 1;0 1 1];
 [h, v] = convhull(cube, "Qt");
 if ~size(h).equal[[12 3]] then pause;end 
 h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
 if ~h.equal[[1 2 4; 1 2 6; 1 4 8; 1 5 6; 1 5 8; 2 3 4; 2 3 7; 2 6 7; 3 4 7; 4 7 8; 5 6 7; 5 7 8]] then pause;end
 if abs(v-1) >= 10*%eps then pause;end 

 [h2, v2] = convhull (cube); // Test defaut option = "Qt"
 if ~size(h2).equal[[12 3]] then pause;end 
 h2 = sort(sort(h2, type='c', dir='i'), type='lr', dir='i');
 if ~h2.equal[h] then pause;end 
 if abs(v2-v1) >= 10*eps then pause;end 

 [h, v] = convhull (cube, "QJ");
 if ~size(h).equal[[12 3]] then pause;end 
 h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
 if ~h.equal[[1 2 4; 1 2 5; 1 4 5; 2 3 4; 2 3 6; 2 5 6; 3 4 8; 3 6 7; 3 7 8; 4 5 8; 5 6 8; 6 7 8]] then pause;end
 if abs(v-1.0) >= 1e6*%eps then pause;end 

 tetrahedron = [1 1 1;-1 -1 1;-1 1 -1;1 -1 -1];
 [h, v] = convhull (tetrahedron);
 h = sort(sort(h, type='c', dir='i'), type='lr', dir='i');
 if ~h.equal[[1 2 3;1 2 4; 1 3 4; 2 3 4]] then pause;end 
 if abs(v- 8/3) >=  10*%eps then pause;end 

*/
