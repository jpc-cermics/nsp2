/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 * test for link 
 *********************************************************************/

  int ilib =0, iflag=1;
  CheckRhs(1,3);
  CheckLhs(0,1);
  if ( IsMatObj(stack,1 ) == OK ) 
    {
      if (GetScalarInt(stack,1,&ilib) == FAIL) return RET_BUG;
    }
  else
    {
      if ((Files = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
      iflag = 0;
      files = Files->S;
    }      
  if ( rhs > 1 ) 
    {
      if ((Enames = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
      enames = Enames->S;
    }
  if ( rhs > 2 ) 
    {
      if ((Str = GetString(stack,3)) == (char*)0) return RET_BUG;
    }
  else
    {
      Str = "f";
    }
  SciDynLoad(files,enames,Str,&ilib,iflag,&rhs);
  if ( ilib < 0) 
    {
      link_bug(ilib); 
      return RET_BUG;
    }
  x1 =ilib;
  if ( (OHMat = ObjDouble(VOID,&x1)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,OHMat);
  return 1;
}

static void  link_bug(i)
     int i;
{
  switch (i)
    {
    case -1 :Scierror("Error in link: the shared archive was not loaded\n");break;
    case -2 :Scierror("You cannot link more functions, maxentry reached\n");break;
    case -3 :Scierror("link: First argument  cannot be a number\n");break;
    case -4 :Scierror("link: Only one entry point allowed\n\ton this operating system\n");break;
    case -5 :Scierror("link: problem with one of the entry point\n");break;
    case -6: Scierror("link: problem with one of the entry point\n");break;
    default: Scierror("Error in function link\n");break;
    }
}


/***********************************************************
 * interface for ulink function 
 *********************************************************/

int int_ulink(stack,rhs,opt,lhs)
     Stack stack;int rhs,opt,lhs;
{
  int ilib;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if (GetScalarInt(stack,1,&ilib) == FAIL) return RET_BUG;
  C2F(isciulink)(&ilib);
  return 0;
}

/***********************************************************
 * interface for c_link function 
      [%t|%false,number]=c_link(name [,ilib]) 
 *********************************************************/

int int_c_link(stack,rhs,opt,lhs)
     Stack stack;int rhs,opt,lhs;
{
  char *Str;
  Obj *O1,*O2;
  int ilib=-1;
  CheckRhs(1,2);
  CheckLhs(0,2);
  if ((Str = GetString(stack,1)) == (char*)0) return RET_BUG;  
  if (rhs == 2 ) 
    if (GetScalarInt(stack,2,&ilib) == FAIL) return RET_BUG;
  C2F(iislink)(Str,&ilib);
  if ( ilib == -1 ) 
    { 
      if (( O1=ObjFalse(VOID)) == NULLOBJ) return RET_BUG;
    }
  else 
    {
      if (( O1=ObjTrue(VOID)) == NULLOBJ) return RET_BUG;
    }
  if ( lhs == 2 ) 
    {
      double d = ilib;
      if (( O2 = ObjDouble(VOID,&d)) == NULLOBJ) return RET_BUG;
    }
  MoveObj(stack,1,O1);
  if ( rhs == 2) 
    MoveObj(stack,2,O2);
  else
    NthObj(2) = O2;
  return lhs;
}

/***********************************************************
 * addinter function 
 *********************************************************/

int int_addinter(stack,rhs,opt,lhs)
     Stack stack;int rhs,opt,lhs;
{
  int err=0;
  char *Str;
  SMatrix *Files,*Enames;  
  CheckRhs(3,3);
  CheckLhs(0,1);
  if ((Files = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((Str = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ((Enames = GetSMat(stack,3)) == NULLSMAT) return RET_BUG;
  AddInter(Files->S,Str,Enames->S,&err);
  if ( err < 0 ) 
    {
      link_bug(err);
      return RET_BUG;
    }
  if ( err != 0) return RET_BUG;
  return 0;
}

/***********************************************************
 * call function 
 *   used to call from Scilab a dynamically linked function 
 * XXXX : we only implement here the long form 
 *        and we use an other name for the short form 
 *********************************************************/

#define MAXPAR 31 

int int_call(stack,rhs,opt,lhs)
     Stack stack;int rhs,opt,lhs;
{
  /** posi[i]=j if i-th argument of function fname 
    is then j-th argument on the stack **/
  int posi[MAXPAR]={0}; 
  /** outpos[i]=j if i-th returned argument is the one 
    which is at position j on the stack **/
  int outpos[MAXPAR];
  /** flag to check that returned arguments are returned only once **/
  int checkout[MAXPAR]={0};
  /** ref[i] stores pointer for relevant data for i-th fname argument **/
  void *ref[MAXPAR];
  int inpos,i,newout=1,outarg,ismat;
  char *Fname,*Type;
  char *Str;
  Matrix *M;
  Obj *O;
  void *Data;
  CheckRhs(1,1000);
  CheckLhs(0,1000);
  /** first argument is the function name **/
  if ((Fname = GetString(stack,1)) == NULL) return RET_BUG;
  /** checking input arguments arg,position,type **/
  /************************************************/
  i=2;
  while ( i <= rhs )
    {
      if ( IsSMatObj(stack,i) == OK ) 
	{
	  if ((Str = GetString(stack,i)) == NULL) return RET_BUG;
	  Data = (void *) Str;
	  ismat = FAIL;
	  if ( strcmp("out",Str)==0)
	    {
	      /** we have reached keywork out **/
	      i++; break ;
	    }
	}
      else 
	{
	  /** Argument is a Matrix **/
	  if ( (M=GetRealMatCopy(stack,i))== NULLMAT) return RET_BUG;	  
	  Data = (void *) M->R;
	  ismat = OK;
	}
      if ( i + 2 > rhs ) 
	{
	  Scierror("Error: Not enough arguments to describe input variable %d\n",
		   (i-2)/3 +1 );
	  return RET_BUG;
	}
      if (GetScalarInt(stack,i+1,&inpos) == FAIL) return RET_BUG;
      if ((Type = GetString(stack,i+2))== NULL) return RET_BUG;
      /** Change data **/
      if ( ismat == OK ) 
	switch (Type[0]) 
	  {
	  case 'd' : break;
	  case 'r' :
	  case 'f' : Mat2float(M);break;
	  case 'i' : Mat2int(M);break;
	  default :
	    Scierror("Error: bad type conversion %c for variable %d\n",Type[0],
		   (i-2)/3 +1 );
	    return RET_BUG;
	  }
      if ( inpos <= 0 || inpos > MAXPAR) 
	{
	  Scierror("Error: input position %d for variable %d\n",inpos,
		   (i-2)/3 +1 );
	  Scierror("\tis out of range ]0,%d[\n",MAXPAR);
	  return RET_BUG;
	}
      if ( posi[inpos] != 0) 
	{
	  Scierror("Error: we have more than one variable (%d and %d) \n",posi[inpos],i);
	  Scierror("\tfor input variable %d\n",inpos);
	  return RET_BUG;
	}
      ref[inpos] = Data;
      posi[inpos] = i;
      i += 3;
    }
  /**  Checking output arguments                 **/  
  /************************************************/
  outarg = 0; /** counts output arguments **/
  while ( i <= rhs )
    {
      int pos1;
      if ((M=GetRealMatCopy(stack,i))== NULLMAT) return RET_BUG;
      if ( M->mn == 1) 
	{
	  /** output argument is only specified by its position in the input list **/
	  if (GetScalarInt(stack,i,&pos1) == FAIL) return RET_BUG;
	  if ( pos1 <= 0 || pos1 > MAXPAR) 
	    {
	      Scierror("Error: Output variable %d is given by its input position (%d)\n",
		       outarg+1,pos1);
	      Scierror("\twhich is out of range ]0,%d[\n",MAXPAR);
	      return RET_BUG;
	    }
	  if (posi[pos1] == 0 )
	    {
	      Scierror("Error: Output variable %d is supposed to be input variable %d\n",
		       outarg+1,pos1);
	      Scierror("\t but input variable %d does not exists\n",pos1);
	      return RET_BUG;
	    }
	  if ( checkout[pos1] > 0) 
	    {
	      Scierror("Error: an input variable (here %d) can appear only once in output list\n",
		       pos1);
	      return RET_BUG;
	    }
	  outpos[outarg]= posi[pos1];
	  checkout[pos1]= 1;
	  i++;
	}
      else if ( M->mn != 2) 
	{
	  Scierror("Error: Incorrect dimensions (%dx%d) for output variable %d\n",
		   M->m,M->n,outarg+1);
	  return RET_BUG;
	}
      else 
	{
	  /** [m,n],pos,type **/
	  if ( i+2 > rhs ) 
	    {
	      Scierror("Error: Not enough arguments to describe output variable %d\n",
		       outarg+1);
	      return RET_BUG;
	    }
	  /** M gives output size **/
	  if (GetScalarInt(stack,i+1,&pos1) == FAIL) return RET_BUG;
	  if ((Type = GetString(stack,i+2))== NULL) return RET_BUG;
	  if ( pos1 <= 0 || pos1 > MAXPAR) 
	    {
	      Scierror("Error: Output variable %d is given by a position (%d)\n",
		       outarg+1,pos1);
	      Scierror("\twhich is out of range ]0,%d[\n",MAXPAR);
	      return RET_BUG;
	    }
	  if ( posi[pos1] != 0 ) 
	    {
	      /** output variable is an input variable **/
	      /** Check size compatibility XXXXXXXXXX **/
	      if ( checkout[pos1] > 0) 
		{
		  Scierror("Error: an input variable (here %d) can appear only once in output list\n",
			   pos1);
		  return RET_BUG;
		}
	      outpos[outarg] = posi[pos1];
	      checkout[pos1]= 1;
	    }
	  else 
	    {
	      /** Create new variable **/
	      SMatrix *S;
	      Matrix *Loc;
	      char *lstr;
	      switch (Type[0])
		{
		case 'c' :
		  /** A revoir pour faire plus court  XXXXX **/
		  if ((S= SMatCreateN(VOID,1,1,M->mn)) == NULLSMAT ) return RET_BUG;
		  if (( O = ObjSMat(S)) == NULLOBJ ) return RET_BUG;
		  NthObj(rhs+newout)= O;
		  if ((lstr = NewStringN(M->mn)) == (String *) 0) 
		    return RET_BUG;
		  StringDestroy(&(S->S[0] ));
		  Data = (void *) S->S[0];
		  break;
		default :
		  if ((Loc = MatCreate(VOID,'r',M->R[0],M->R[1]))==  NULLMAT) 
		    return RET_BUG;
		  if (( O = ObjMat(Loc)) == NULLOBJ ) return RET_BUG;
		  NthObj(rhs+newout)= O;
		  Data = (void *) Loc->R;
		  break;
		}
	      posi[pos1]=i;
	      ref[pos1] = Data;
	      outpos[outarg] = rhs+newout ;
	      checkout[pos1]= 1;
	      newout++;
	    }
	  i += 3;
	}
      outarg++;      
    }
  /** Calling the interfaced routine **/
  /** XXXXX a Changer **/
  foo(ref[1],ref[2],ref[3],ref[4]);
  /** put output arguments on the stack : all the outpos[i] are differents 
    and outpos is changed after the call to PutLhsObj **/
  PutLhsObj(stack,Min(lhs,outarg),outpos);
  /** Check if we need to change Data i->d or r->d **/
  for ( i=1 ; i <= Min(lhs,outarg) ; i++) 
    ObjConvert(NthObj(i));
  /** Check if we need to change output dimensions **/
  return Min(lhs,outarg);
}

static int foo(int *ix,float *fx,double *dx,char *S)
{
  int i;
  for ( i=0; i < 10 ; i++)
    {
      ix[i] *= 2; fx[i] *= 2.0 ; dx[i] *= 2.0;
    }
  S[0]='X';
  return 0;
}


/*************************************************************
 * The Interface for basic matrices operation 
 *************************************************************/

static OpTab Functions_func[]={
  #include "Functions-IN.nam"
  {(char *) 0, NULL}
};

int Functions_Interf(i,stack,rhs,opt,lhs)
     Stack stack;
     int i,rhs,opt,lhs;
{
  return (*(Functions_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Functions_Interf_Info(i,fname,f)
     int i;
     char **fname;
     function **f;
{
  *fname = Functions_func[i].name;
  *f = Functions_func[i].fonc;
}

