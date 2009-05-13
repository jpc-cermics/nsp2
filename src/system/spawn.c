#include <gtk/gtk.h>
#define  Spawn_Private 
#include "nsp/object.h"
#include "spawn.h"
#include "nsp/interf.h"

/* XXXXXXXX */
nsp_string send_maxima_string(NspSpawn *H,const char *str);
static int nsp_g_spawn_cmd(char **cmd,NspSpawn *H);

/* 
 * NspSpawn inherits from NspObject 
 */

int nsp_type_spawn_id=0;
NspTypeSpawn *nsp_type_spawn=NULL;

/*
 * Type object for Spawn 
 * all the instance of NspTypeSpawn share the same id. 
 * nsp_type_spawn: is an instance of NspTypeSpawn 
 *    used for objects of NspSpawn type (i.e built with new_spawn) 
 * other instances are used for derived classes 
 */
NspTypeSpawn *new_type_spawn(type_mode mode)
{
  NspTypeSpawn *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_spawn != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_spawn;
    }
  if ((type =  malloc(sizeof(NspTypeSpawn))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = spawn_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = spawn_get_methods; 
  type->new = (new_func *) new_spawn;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for spawn */ 

  top->pr = (print_func *) nsp_spawn_print;                  
  top->dealloc = (dealloc_func *) nsp_spawn_destroy;
  top->copy  =  (copy_func *) nsp_spawn_copy;                 
  top->size  = (size_func *) nsp_spawn_size;                
  top->s_type =  (s_type_func *) nsp_spawn_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_spawn_type_short_string;
  top->info = (info_func *) nsp_spawn_info ;                  
  /* top->is_true = (is_true_func  *) nsp_spawn_is_true; */
  /* top->loop =(loop_func *) nsp_spawn_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_spawn_object;
  top->eq  = (eq_func *) nsp_spawn_eq;
  top->neq  = (eq_func *) nsp_spawn_neq;
  /* not implemented  
  top->save  = (save_func *) nsp_spawn_xdr_save;
  top->load  = (load_func *) nsp_spawn_xdr_load;
  */
  top->create = (create_func*) int_spawn_create;
  
  /* specific methods for spawn */
      
  type->init = (init_func *) init_spawn;

/* 
 * Spawn interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_spawn_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeSpawn called nsp_type_spawn
       */
      type->id =  nsp_type_spawn_id = nsp_new_type_id();
      nsp_type_spawn = type;
      if ( nsp_register_type(nsp_type_spawn) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_spawn(mode);
    }
  else 
    {
       type->id = nsp_type_spawn_id;
       return type;
    }
}

/*
 * initialize Spawn instances 
 * locally and by calling initializer on parent class 
 */

static int init_spawn(NspSpawn *o,NspTypeSpawn *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Spawn 
 */

NspSpawn *new_spawn() 
{
  NspSpawn *loc; 
  /* type must exists */
  nsp_type_spawn = new_type_spawn(T_BASE);
  if ( (loc = malloc(sizeof(NspSpawn)))== NULLSPAWN) return loc;
  /* initialize object */
  if ( init_spawn(loc,nsp_type_spawn) == FAIL) return NULLSPAWN;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Spawn 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_spawn_size(NspSpawn *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char spawn_type_name[]="Spawn";
static char spawn_short_type_name[]="spawn";

static char *nsp_spawn_type_as_string(void)
{
  return(spawn_type_name);
}

static char *nsp_spawn_type_short_string(NspObject *v)
{
  return(spawn_short_type_name);
}

/*
 * A == B 
 */

static int nsp_spawn_eq(NspSpawn *A, NspObject *B)
{
  NspSpawn *loc = (NspSpawn *) B;
  if ( check_cast(B,nsp_type_spawn_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  return FALSE;
}

/*
 * A != B 
 */

static int nsp_spawn_neq(NspSpawn *A, NspObject *B)
{
  return ( nsp_spawn_eq(A,B) == TRUE ) ? FALSE : TRUE;
}


/*
 * delete 
 */

void nsp_spawn_destroy(NspSpawn *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     nsp_smatrix_destroy(H->obj->out);
     if ( H->obj->active == TRUE )
       {
	 /* fprintf(stderr,"Closing the spawned child"); */
	 g_spawn_close_pid(H->obj->pid);
       }
     FREE(H->obj->prog);
     FREE(H->obj->prompt_check);
     FREE(H->obj);
   }
  FREE(H);
}

/*
 * info 
 */

void nsp_spawn_info(NspSpawn *M, int indent)
{
  int i;
  if ( M == NULLSPAWN) 
    {
      Sciprintf("Null Pointer Spawn \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("Spawn %s {\n", NSP_OBJECT(M)->name);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");Sciprintf("}\n");
}

/*
 * print 
 */

int nsp_spawn_print(NspSpawn *M, int indent)
{
  int i;
  if ( M == NULLSPAWN) 
    {
      Sciprintf("Null Pointer Spawn \n");
      return TRUE;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("Spawn %s {\n", NSP_OBJECT(M)->name);
  Sciprintf("%s", M->obj->prog);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");Sciprintf("}\n");
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Spawn objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspSpawn   *nsp_spawn_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_spawn_id) == TRUE ) return ((NspSpawn *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_spawn));
  return NULL;
}

int IsSpawnObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_spawn_id);
}

int IsSpawn(NspObject *O)
{
  return nsp_object_type(O,nsp_type_spawn_id);
}

NspSpawn  *GetSpawnCopy(Stack stack, int i)
{
  if (  GetSpawn(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSpawn  *GetSpawn(Stack stack, int i)
{
  NspSpawn *M;
  if (( M = nsp_spawn_object(NthObj(i))) == NULLSPAWN)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspSpawn *spawn_create_void(char *name,NspTypeBase *type)
{
 NspSpawn *H  = (type == NULL) ? new_spawn() : type->new();
 if ( H ==  NULLSPAWN)
  {
   Sciprintf("No more memory\n");
   return NULLSPAWN;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSPAWN;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->obj = NULL;
 return H;
}

NspSpawn *spawn_create(char *name,NspTypeBase *type)
{
  NspSpawn *H  = spawn_create_void(name,type);
  if ( H ==  NULLSPAWN) return NULLSPAWN;
  if ((H->obj = malloc(sizeof(nsp_spawn))) == NULL) return NULL;
  H->obj->ref_count=1;
  H->obj->active = FALSE;
  H->obj->out_smat = TRUE;
  H->obj->prog = NULL;
  H->obj->prompt_check = NULL;
  H->obj->out = NULL;
  return H;
}

/*
 * copy for gobject derived class  
 */

NspSpawn *nsp_spawn_copy(NspSpawn *self)
{
  NspSpawn *H  =spawn_create_void(NVOID,(NspTypeBase *) nsp_type_spawn);
  if ( H ==  NULLSPAWN) return NULLSPAWN;
  H->obj = self->obj;
  self->obj->ref_count++;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the Spawn
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_spawn_create(Stack stack, int rhs, int opt, int lhs)
{
  char *prompt_check=NULL;
  NspSMatrix *S;
  NspSpawn *H;
  CheckStdRhs(1,2);
  if ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) 
    {
      Scierror("Error: first argument of %s should be of size > 0\n",NspFname(stack));
      return RET_BUG;
    }
  if (rhs==2) 
    {
      if ((prompt_check = GetString(stack,2)) == NULL)
	return RET_BUG;
    }
  /* want to be sure that type spawn is initialized */
  nsp_type_spawn = new_type_spawn(T_BASE);
  if(( H = spawn_create(NVOID,(NspTypeBase *) nsp_type_spawn)) == NULLSPAWN) 
    return RET_BUG;
  if ( prompt_check != NULL )
    {
      if ((H->obj->prompt_check = nsp_string_copy(prompt_check)) == (nsp_string) 0) 
	return RET_BUG;
    }
  if ( nsp_g_spawn_cmd(S->S,H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_spawn_send(NspSpawn *self,Stack stack,int rhs,int opt,int lhs)
{
  nsp_string str,str_res;
  int_types T[] = {string,t_end};
  NspObject *Res;
  CheckLhs(0,1);
  if ( self->obj->active == FALSE ) 
    {
      Scierror("Error: connection to spawned program is not active\n");
      return RET_BUG;
    }
  if ( GetArgs(stack,rhs,opt,T,&str) == FAIL) return RET_BUG;
  if ((str_res = send_maxima_string(self,str))== NULL) return RET_BUG;
  if ((Res = nsp_create_object_from_str(NVOID,str_res)) == NULL) 
    {
      nsp_string_destroy(&str_res);
      return RET_BUG;
    }
  MoveObj(stack,1,Res);
  return 1;
}

static int _wrap_spawn_close(NspSpawn *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckLhs(0,0);
  CheckRhs(0,0);
  g_spawn_close_pid(self->obj->pid);
  self->obj->active = FALSE;
  return 0;
}

static NspMethods spawn_methods[] = {
  {"send",(nsp_method *) _wrap_spawn_send},
  {"close",(nsp_method *) _wrap_spawn_close},
  { NULL, NULL}
};

static NspMethods *spawn_get_methods(void) { return spawn_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab spawn_attrs[] = {
  { NULL,NULL,NULL,NULL, NULL  },
};

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/* the call is in fact in System-IN.c XXXXX */

static OpTab spawn_func[]={
  { "spawn", int_spawn_create},
  { NULL, NULL}
};

/* call ith function in the spawn interface */

int spawn_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(spawn_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void spawn_Interf_Info(int i, char **fname, function (**f))
{
  *fname = spawn_func[i].name;
  *f = spawn_func[i].fonc;
}

/* spawning. 
 *
 */

/* launching a child with redirection of in and out. 
 */

static gboolean stdout_read( GIOChannel *source, GIOCondition condition, gpointer data );
static gboolean stderr_read( GIOChannel *source, GIOCondition condition, gpointer data );

/* initialize a spwan object. 
 */

static int nsp_g_spawn_cmd(char **cmd,NspSpawn *H)
{
  GIOChannel  *channel_out, *channel_err, *channel_in=NULL;
  int pid;
  nsp_string res,pr;
  int stdout_pipe=0, stderr_pipe=0,stdin_pipe=0;
  GError *error = NULL;
  guint stdout_tag, stderr_tag;
  gboolean rep;
  if ( H->obj->prompt_check == NULL) 
    {
      rep =  g_spawn_async_with_pipes( NULL,cmd,NULL,
					G_SPAWN_SEARCH_PATH |
					G_SPAWN_DO_NOT_REAP_CHILD |
					G_SPAWN_CHILD_INHERITS_STDIN,
					NULL, NULL,
					&pid,
					NULL,&stdout_pipe,&stderr_pipe,
					&error );
    }
  else 
    {
      rep =  g_spawn_async_with_pipes( NULL,cmd,NULL,
					G_SPAWN_SEARCH_PATH |
					G_SPAWN_DO_NOT_REAP_CHILD,
					NULL, NULL,
					&pid,
					&stdin_pipe,&stdout_pipe,&stderr_pipe,
					&error );
    }
  if ( !rep ) 
    {
      Scierror("Error: spawn command failed\n");
      Scierror("\t%s\n",error->message );
      g_error_free( error );
      return FAIL;
    } 
  else 
    {
      if ( stdin_pipe != 0)
	{
	  channel_in =  g_io_channel_unix_new( stdin_pipe );
	  g_io_channel_set_encoding( channel_in, NULL, NULL );
	  g_io_channel_set_flags(channel_in, G_IO_FLAG_NONBLOCK, NULL);
	  g_io_channel_flush( channel_in,NULL);
	  g_io_channel_set_buffered(channel_in, FALSE);
	}
      if ( stdout_pipe != 0 ) 
	{
	  channel_out = g_io_channel_unix_new( stdout_pipe );
	  g_io_channel_set_encoding( channel_out, NULL, NULL );
	  g_io_channel_set_flags(channel_out, G_IO_FLAG_NONBLOCK, NULL);
	  g_io_channel_flush( channel_out,NULL);
	  g_io_channel_set_buffered(channel_out, FALSE);
	  stdout_tag = g_io_add_watch( channel_out,
				       (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL),
				       stdout_read,
				       H);
	  g_io_channel_unref( channel_out );
	}
      if ( stderr_pipe != 0 ) 
	{
	  channel_err = g_io_channel_unix_new( stderr_pipe );
	  g_io_channel_set_encoding( channel_err, NULL, NULL );
	  g_io_channel_set_flags(channel_err, G_IO_FLAG_NONBLOCK, NULL);
	  g_io_channel_flush( channel_err,NULL);
	  g_io_channel_set_buffered(channel_err, FALSE);
	  stderr_tag = g_io_add_watch ( channel_err,
					(G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL),
					stderr_read,
					H );
	  g_io_channel_unref( channel_err );
	}
    } 
  H->obj->active = TRUE;
  H->obj->pid = pid;
  H->obj->channel_in = channel_in;
  if ((H->obj->prog = nsp_string_copy(cmd[0])) == (nsp_string) 0) return FAIL;
  /* wait for the first prompt of child */
  gtk_main();
  if ( H->obj->out_smat == FALSE) return OK ;
  if ( H->obj->out  == NULL ) return OK;
  res = nsp_smatrix_elts_concat(H->obj->out,NULL,0,NULL,0);
  nsp_smatrix_resize(H->obj->out,0,0);
  if ( res != NULL && ((pr=strstr(res,"(%i")) != NULL)) 
    {
      if ( pr > res && *(pr-1) == '\n') *(pr-1) = '\0';
      else *pr='\0';
    }
  if ( res != NULL) 
    fprintf(stderr,"%s\n",res);
  return OK;
}


/* activated when child output something. 
 * if prompt_check is non null 
 * we accumulate output up to prompt_check and store the accumulated 
 * output in a String Matrix.
 */

static gboolean stdout_read( GIOChannel *source, GIOCondition condition, gpointer data )
{
  NspSpawn *S=data;
  gsize bytes_read=0;
  char  buf[1024];
  GIOStatus status;
  if (condition & (G_IO_IN | G_IO_PRI))
    {
      /* we read data in the out channel of the child */
      status = g_io_channel_read_chars( source, buf,1023,&bytes_read, NULL);
      if ( status != G_IO_STATUS_NORMAL) goto err;
      if (bytes_read != 0) 
	{
	  if ( S->obj->out == NULL) 
	    S->obj->out = nsp_smatrix_create(NVOID,0,0,NULL,0);
	  buf[bytes_read]='\0';
	  /* Sciprintf("{%s}",buf); */
	  if ( S->obj->prompt_check == NULL) 
	    {
	      Sciprintf("%s",buf);
	      /* XXXXX  here we need to be able to flush nsp stdout */
	      fflush(stdout);
	    }
	  else 
	    {
	      /* append the bytes that we have read from child */
	      nsp_row_smatrix_append_string( S->obj->out,buf);
	      /* detect the child prompt to decide when to return from here */
	      if ( strstr(buf,S->obj->prompt_check) != NULL) 
		{
		  if ( gtk_main_level() != 0)  gtk_main_quit();
		}
	    }
	}
    }
  else if ( condition & (G_IO_ERR | G_IO_HUP | G_IO_NVAL))
    {
      goto err;
    }
  return TRUE;
 err:
  S->obj->active = FALSE;
  if ( gtk_main_level() != 0)  gtk_main_quit();
  return FALSE;
}

/* A finir, XXX 
 *
 */

static gboolean stderr_read( GIOChannel *source, GIOCondition condition, gpointer data )
{
  NspSpawn *S=data;
  char buf[1024];
  gsize bytes_read;
  GIOStatus status;
  status = g_io_channel_read_chars( source, buf,1023,&bytes_read, NULL);
  if ( status != G_IO_STATUS_NORMAL) goto err;
  buf[bytes_read]='\0';
  Sciprintf( "%s",buf );
  return TRUE;
 err:
  S->obj->active = FALSE;
  if ( gtk_main_level() != 0)  gtk_main_quit();
  return FALSE;
}


nsp_string send_maxima_string(NspSpawn *H,const char *str)
{
  gsize bytes_written;
  int status;
  nsp_string res,pr;
  status = g_io_channel_write_chars(H->obj->channel_in,str,strlen(str),&bytes_written, NULL);
  if ( status != G_IO_STATUS_NORMAL) 
    {
      fprintf(stderr,"something wrong when sending characters to child\n");
      return NULL;
    }
  /* 
     status = g_io_channel_flush( H->obj->channel_in,NULL);
     if ( status != G_IO_STATUS_NORMAL) 
     {
     fprintf(stderr,"something wrong when sending characters to child\n");
     return NULL;
     }
  */

  gtk_main();
  if ( H->obj->out_smat == FALSE) return NULL;
  res = nsp_smatrix_elts_concat(H->obj->out,NULL,0,NULL,0);
  nsp_smatrix_resize(H->obj->out,0,0);
  /* remove the input prompt of child */
  if ( res != NULL && H->obj->prompt_check != NULL 
       && ((pr=strstr(res,H->obj->prompt_check)) != NULL)) 
    {
      if ( pr > res && *(pr-1) == '\n') *(pr-1) = '\0';
      else *pr='\0';
    }
  return res;
}

/* interface to the g_spawn_sync function 
 *
 * accept working directory 
 * command=%t|%f 
 * 
 *
 */

int int_g_spawn_sync(Stack stack, int rhs, int opt, int lhs)
{
  GSpawnFlags flags = G_SPAWN_SEARCH_PATH 
    | G_SPAWN_CHILD_INHERITS_STDIN;
  char *env[]={"POO=45",NULL};
  gboolean rep;
  gchar *working_directory=NULL;
  gchar *standard_output;
  gchar *standard_error;
  int exit_status;
  GError *error=NULL;
  NspSMatrix *S;
  nsp_option opts[] ={{"wd",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  
  CheckStdRhs(1,2);
  CheckLhs(1,4);
  if ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) 
    {
      Scierror("Error: first argument of %s should be of size > 0\n",NspFname(stack));
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &working_directory) == FAIL )
    return RET_BUG;

  if ( lhs <= 1) 
    {
      flags = flags | G_SPAWN_STDOUT_TO_DEV_NULL 
	| G_SPAWN_STDERR_TO_DEV_NULL ;
    }
  rep = g_spawn_sync(working_directory,S->S,env,flags,
		     NULL,
		     NULL,
		     (lhs <= 1) ? NULL: &standard_output,
		     (lhs <= 1) ? NULL: &standard_error,
		     &exit_status,
		     &error);

  if ( nsp_move_boolean(stack,1,rep)  == FAIL ) return RET_BUG;
  if ( lhs >= 2)
    {
      char *st = (standard_output != NULL) ? standard_output : "";
      /* return the standard output */
      if ( nsp_move_string(stack,2, st,-1) ==FAIL) return RET_BUG;
      if (standard_output != NULL) g_free(standard_output);
    }
  if ( lhs >= 3 )
    {
      char *st = (standard_error != NULL) ? standard_error : "";
      /* return the standard error */
      if ( nsp_move_string(stack,3, st,-1) ==FAIL) return RET_BUG;
      if (standard_error != NULL) g_free(standard_error);
    }
  if ( lhs >= 4)
    {
      char *st = (error != NULL) ? error->message : "";
      if ( nsp_move_string(stack,4, st,-1) ==FAIL) return RET_BUG;
      if ( error != NULL) g_error_free (error);
    }
  return Max(lhs,1);
} 

