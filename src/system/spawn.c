#include <gtk/gtk.h>
#include <nsp/nsp.h>
#define  Spawn_Private 
#include <nsp/objects.h> 
#include <nsp/interf.h> 
#include <nsp/nsptcl.h>
#include "spawn.h"

#ifdef WIN32
#include <windows.h>
extern char *nsp_translate_file_name ( char *name, nsp_tcldstring *bufferPtr);
static int nsp_spawn_showfile(const char *fname);
static NspSMatrix *nsp_file2SMatrix(const char *fname);
static int nsp_win32_system(const char *working_dir, const char *command,BOOL WaitInput, char *std_o,char *std_e, int *ret);
static int int_g_spawn_sync_win32(Stack stack, int rhs, int opt, int lhs);
#endif

static nsp_string send_string_to_child(NspSpawn *H,const char *str);
static int nsp_g_spawn_cmd(char **cmd,NspSpawn *H);
static int nsp_spawn_move(Stack stack,const char *str,const char *split_str, int pos);
static void nsp_spawn_showstr(char *str,const char *msg) ;

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
	 H->obj->active = FALSE;
	 if ( H->obj->channel_in != NULL) 
	   {
	     g_io_channel_unref(H->obj->channel_in);
	     H->obj->channel_in=NULL;
	   } 
	 if ( H->obj->channel_err != NULL) 
	   {
	     g_source_remove(H->obj->err_id);
	     g_io_channel_unref(H->obj->channel_err);
	     H->obj->channel_err=NULL;
	   }
	 if ( H->obj->channel_out != NULL) 
	   {
	     g_source_remove(H->obj->out_id);
	     g_io_channel_unref(H->obj->channel_out);
	     H->obj->channel_out=NULL;
	   }
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
  H->obj->channel_in = NULL;
  H->obj->channel_out = NULL;
  H->obj->channel_err = NULL;
  H->obj->out_id = 0;
  H->obj->err_id = 0;
  H->obj->err = FALSE;
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

/* method send:
 * send a string to a spawned process
 */

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
  if ((str_res = send_string_to_child(self,str))== NULL) return RET_BUG;
  if ((Res = nsp_create_object_from_str(NVOID,str_res)) == NULL) 
    {
      nsp_string_destroy(&str_res);
      return RET_BUG;
    }
  MoveObj(stack,1,Res);
  return 1;
}

/* method close: close a spawned process. 
 */

static int _wrap_spawn_close(NspSpawn *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckLhs(0,0);
  CheckRhs(0,0);
  self->obj->active = FALSE;
  if ( self->obj->channel_in != NULL) 
    {
      g_io_channel_unref(self->obj->channel_in);
      self->obj->channel_in=NULL;
    } 
  if ( self->obj->channel_err != NULL) 
    {
      g_source_remove(self->obj->err_id);
      g_io_channel_unref(self->obj->channel_err);
      self->obj->channel_err=NULL;
    }
  if ( self->obj->channel_out != NULL) 
    {
      g_source_remove(self->obj->out_id);
      g_io_channel_unref(self->obj->channel_out);
      self->obj->channel_out=NULL;
    }
  g_spawn_close_pid(self->obj->pid);
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
  {"spawn", int_spawn_create},
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
  GIOChannel  *channel_out=NULL, *channel_err=NULL, *channel_in=NULL;
  GPid pid;
  nsp_string res,pr;
  int stdout_pipe=0, stderr_pipe=0,stdin_pipe=0;
  GError *error = NULL;
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
	  g_io_channel_set_close_on_unref (channel_in,TRUE);
	}
      if ( stdout_pipe != 0 ) 
	{
	  channel_out = g_io_channel_unix_new( stdout_pipe );
	  g_io_channel_set_encoding( channel_out, NULL, NULL );
	  g_io_channel_set_flags(channel_out, G_IO_FLAG_NONBLOCK, NULL);
	  g_io_channel_flush( channel_out,NULL);
	  g_io_channel_set_buffered(channel_out, FALSE);
	  H->obj->out_id = g_io_add_watch( channel_out,
					   (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL),
					   stdout_read,
					   H);
	  g_io_channel_unref( channel_out );
	  g_io_channel_set_close_on_unref (channel_out,TRUE);

	}
      if ( stderr_pipe != 0 ) 
	{
	  channel_err = g_io_channel_unix_new( stderr_pipe );
	  g_io_channel_set_encoding( channel_err, NULL, NULL );
	  g_io_channel_set_flags(channel_err, G_IO_FLAG_NONBLOCK, NULL);
	  g_io_channel_flush( channel_err,NULL);
	  g_io_channel_set_buffered(channel_err, FALSE);
	  H->obj->err_id = g_io_add_watch ( channel_err,
					    (G_IO_IN|G_IO_PRI|G_IO_ERR|G_IO_HUP|G_IO_NVAL),
					    stderr_read,
					    H );
	  g_io_channel_unref( channel_err );
	  g_io_channel_set_close_on_unref (channel_err,TRUE);
	}
    } 

  H->obj->active = TRUE;
  H->obj->pid = pid;
  H->obj->channel_in = channel_in;
  H->obj->channel_out = channel_out;
  H->obj->channel_err = channel_err;
  if ((H->obj->prog = nsp_string_copy(cmd[0])) == (nsp_string) 0) return FAIL;
  /* wait for the first prompt of child */
  gtk_main();
  if ( H->obj->err == TRUE) 
    {
      g_source_remove(H->obj->err_id);
      H->obj->channel_err=NULL;
      g_source_remove(H->obj->out_id); 
      H->obj->channel_out=NULL;
      H->obj->active = FALSE;
      if ( H->obj->channel_in !=NULL) 
	g_io_channel_unref( channel_in );
      H->obj->channel_in=NULL;
      g_spawn_close_pid(H->obj->pid);
    }
  
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
    {
      fprintf(stderr,"%s\n",res);
    }
  /* 
  if ( H->obj->active == TRUE ) 
    {
      H->obj->active = FALSE;
      g_spawn_close_pid(H->obj->pid);
    }
  */
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
  S->obj->err = TRUE;
  if ( gtk_main_level() != 0)  gtk_main_quit();
  return FALSE;
}

/* to be finished 
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
  S->obj->err = TRUE;
  if ( gtk_main_level() != 0)  gtk_main_quit();
  return FALSE;
}

static nsp_string send_string_to_child(NspSpawn *H,const char *str)
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

/**
 * int_g_spawn_sync:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface to g_spawn_sync(). Note that we do not get 
 * control until the spawned process terminate. Thus it 
 * could be usefull to make an other function to at least 
 * check for gtk_events while the spawned process is running.
 *
 * On windows the functions have the following problems 
 * - a console will pop up for non gui programs 
 * - in some situation the standard output or standard error 
 *   are not collected. This happens for example when 
 *   spawning nmake. 
 * 
 * Returns: 
 **/

int int_g_spawn_sync(Stack stack, int rhs, int opt, int lhs)
{
  int ret =  RET_BUG, w32mode= TRUE ;
  GSpawnFlags flags = G_SPAWN_SEARCH_PATH 
    | G_SPAWN_CHILD_INHERITS_STDIN;
  gboolean rep;
  gchar *working_directory=NULL;
  gchar *standard_output=NULL;
  gchar *standard_error=NULL; 
  int exit_status;
  GError *error=NULL;
  NspSMatrix *S;

  nsp_option opts[] ={{"wd",string,NULLOBJ,-1},
		      {"w32mode", s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,2);
  CheckLhs(0,4);
  if ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) 
    {
      Scierror("Error: first argument of %s should be of size > 0\n",NspFname(stack));
      return RET_BUG;
    }
  
  if ( get_optional_args(stack, rhs, opt, opts, &working_directory, &w32mode) == FAIL )
    return RET_BUG;

#ifdef WIN32 
  if ( w32mode == TRUE ) 
    return  int_g_spawn_sync_win32(stack,rhs,opt,lhs);
#endif

  rep = g_spawn_sync(working_directory,S->S,NULL,flags,
		     NULL,
		     NULL,
		     &standard_output,
		     &standard_error,
		     &exit_status,
		     &error);

  if ( lhs <= 0 && rep == FALSE ) 
    {
      if (error != NULL) Scierror("Error: %s\n",error->message);
      return RET_BUG;
    }

  if ( nsp_move_boolean(stack,1,rep)  == FAIL ) return RET_BUG;

  if ( lhs <= 1 )
    {
      nsp_spawn_showstr(standard_output, "cannot convert standard output to utf8\n" );
      nsp_spawn_showstr(standard_error, "cannot convert standard error to utf8\n" );
      if (error != NULL) 
	nsp_spawn_showstr(error->message, "cannot convert error message to utf8\n" );
    }

  if ( lhs >= 2)
    {
      /* return standard_output as second argument */
      char *st = (standard_output != NULL) ? standard_output : "";
      if ( nsp_spawn_move(stack,st,"\n",2) == FAIL) goto end;
    }
  if ( lhs >= 3 )
    {
      /* return the standard error as third argument */
      char *st = (standard_error != NULL) ? standard_error : "";
      if ( nsp_spawn_move(stack,st,"\n",3) == FAIL) goto end;
    }
  if ( lhs >= 4)
    {
      /* return the error message as third argument */
      char *st = (error != NULL) ? error->message : "";
      if ( nsp_spawn_move(stack,st,"xxx",4) == FAIL) goto end;
    }
  ret = Max(lhs,1);
 end:
  if (standard_output != NULL) g_free(standard_output);
  if (standard_error != NULL) g_free(standard_error);
  if (error != NULL) g_error_free (error);
  return ret;
} 

/* convert a string to a utf8 string matrix 
 * then move the matrix on the stack 
 */

static int nsp_spawn_move(Stack stack,const char *str,const char *split_str, int pos)
{
  NspSMatrix *R;
  if ((R=nsp_smatrix_split_string(str,split_str,0))==NULL) 
    return FAIL;
  R->m=R->n;R->n=1;
  if ( nsp_smatrix_to_utf8(R) == FAIL ) return FAIL;
  MoveObj(stack,pos,NSP_OBJECT(R));
  return OK;
}

static void nsp_spawn_showstr(char *str,const char *msg) 
{
  gchar *str_utf8;
  if (str == NULL || strlen(str) == 0 ) return;
  str_utf8= nsp_string_to_utf8(str);
  Sciprintf("%s",(str_utf8 != NULL) ? str_utf8 : msg);
  if ( str_utf8 != str )  nsp_string_destroy(&str_utf8);
}


/**
 * int_g_spawn_sync_win32:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * used for win32 to avoid the console 
 * note that the given command is evaluated 
 * by Comspec and io are redirected. Thus 
 * this function is a bit different from the 
 * previous one 
 * 
 * Returns: 
 **/

#ifdef WIN32

static int int_g_spawn_sync_win32(Stack stack, int rhs, int opt, int lhs)
{
  char std_o[_MAX_PATH];
  char std_e[_MAX_PATH];
  char *TMPDir=NULL;
  char *cmd;
  int ret =  RET_BUG;
  int w32mode= TRUE;
  gboolean rep;
  gchar *working_directory=NULL;
  GError *error=NULL;
  NspSMatrix *S;
  nsp_option opts[] ={{"wd",string,NULLOBJ,-1},
		      {"w32mode", s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,2);
  CheckLhs(0,4);
  if ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) 
    {
      Scierror("Error: first argument of %s should be of size > 0\n",NspFname(stack));
      return RET_BUG;
    }
  
  if ( get_optional_args(stack, rhs, opt, opts, &working_directory,&w32mode) == FAIL )
    return RET_BUG;
  
  TMPDir=getenv("NSP_TMPDIR");
  sprintf(std_o,"%s\\std_o",TMPDir);
  sprintf(std_e,"%s\\std_e",TMPDir);

  if (( cmd = nsp_smatrix_elts_concat(S," ",1," ",1))== NULL) 
    return RET_BUG;
  
  rep = nsp_win32_system(working_directory,cmd,FALSE, std_o,std_e,&ret);
  
  if ( lhs <= 0 && rep == FAIL ) 
    {
      if (error != NULL) Scierror("Error: %s\n",error->message);
      goto end;
    }

  if ( nsp_move_boolean(stack,1,(rep== OK) ? TRUE:FALSE)  == FAIL ) goto end;

  if ( lhs <= 1 )
    {
      if ( nsp_spawn_showfile(std_o ) == FAIL) 	goto end;
      if ( nsp_spawn_showfile(std_e )== FAIL)	goto end;
      if (error != NULL) 
	nsp_spawn_showstr(error->message, "cannot convert error message to utf8\n" );
    }
  if ( lhs >= 2)
    {
      /* return the standard output as second argument */
      NspSMatrix *S = nsp_file2SMatrix(std_o);
      if ( S == NULL) goto end;
      MoveObj(stack,2,NSP_OBJECT(S));
    }
  if ( lhs >= 3 )
    {
      /* return the standard error as third argument */
      NspSMatrix *S = nsp_file2SMatrix(std_e);
      if ( S == NULL) goto end;
      MoveObj(stack,3,NSP_OBJECT(S));
    }
  if ( lhs >= 4)
    {
      /* return the error message as third argument */
      char *st = (error != NULL) ? error->message : "";
      if ( nsp_spawn_move(stack,st,"xxx",4) == FAIL) goto end;
    }
  ret = Max(lhs,1);
 end:
  if (cmd != NULL) free(cmd);
  if (error != NULL) g_error_free (error);
  return ret;
} 

static int nsp_spawn_showfile(const char *fname)
{
  int i;
  NspSMatrix *S = NULL;
  if ((S = nsp_file2SMatrix(fname) ) == NULL ) return FAIL;
  for ( i = 0 ; i < S->mn ; i++) 
    Sciprintf("%s\n",S->S[i]);
  nsp_smatrix_destroy(S);
  return OK;
}


static int nsp_win32_system(const char *working_dir, const char *command,BOOL WaitInput, char *std_o,char *std_e, int *ret)
{
  const char *wd;
  /* /u unicode /a ansi */
  char cmd_args[]= "%s /a /c \"%s\" > %s 2> %s";
  char shellCmd[_MAX_PATH];
  char *CmdLine=NULL;
  PROCESS_INFORMATION piProcInfo; 
  STARTUPINFO siStartInfo;
  SECURITY_ATTRIBUTES saAttr; 
  DWORD ExitCode=0;
  char *dirname=nsp_get_cwd();
  char *dirname_native = NULL;
  nsp_tcldstring buffer;

  /* take care that dirname_native is an adress 
   * in buffer. buffer must be freed when dirname_native 
   * is no more used.
   * Note also that changing to dirname seams not to 
   * work when using wine 1.1 !
   */
  dirname_native = nsp_translate_file_name(dirname,&buffer);

  if (dirname_native == NULL)
    {
      Scierror("Failed to convert to native name %s\n",nsp_get_cwd());
      return FAIL;
    }
  saAttr.nLength = sizeof(SECURITY_ATTRIBUTES); 
  saAttr.bInheritHandle = TRUE; 
  saAttr.lpSecurityDescriptor = NULL; 

  ZeroMemory( &piProcInfo, sizeof(PROCESS_INFORMATION) );
  ZeroMemory( &siStartInfo, sizeof(STARTUPINFO) );

  siStartInfo.cb = sizeof(STARTUPINFO); 
  siStartInfo.dwFlags      = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
  siStartInfo.wShowWindow  = SW_HIDE; /* Hide the console window */
  
  if (WaitInput)
    {
      siStartInfo.hStdInput=GetStdHandle(STD_INPUT_HANDLE);
    }
  else
    {
      siStartInfo.hStdInput=NULL;
    }

  siStartInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

  GetEnvironmentVariable("ComSpec", shellCmd, _MAX_PATH);

  CmdLine= malloc( (strlen(shellCmd)+strlen(command)+strlen(cmd_args)
		    +strlen(std_o)+strlen(std_e)+1)*sizeof(char) );
  sprintf(CmdLine,cmd_args,shellCmd,command,std_o,std_e);
#if 0 
  Sciprintf("running %s\n",CmdLine);
  Sciprintf("in native [%s]\n",dirname_native);
  Sciprintf("in %s\n",dirname);
#endif 

  wd = ( working_dir != NULL) ? working_dir : dirname_native;

  if (CreateProcess(NULL, CmdLine, NULL, NULL, TRUE,0, NULL, wd , &siStartInfo, &piProcInfo))
    {
      WaitForSingleObject(piProcInfo.hProcess,INFINITE);

      if ( GetExitCodeProcess(piProcInfo.hProcess,&ExitCode) == STILL_ACTIVE )
	{
	  TerminateProcess(piProcInfo.hProcess,0);
	}

      CloseHandle(piProcInfo.hProcess);
      if (CmdLine) { free(CmdLine);CmdLine=NULL;}
      nsp_tcldstring_free(&buffer);
      *ret =  ExitCode;
      return OK;
    }
  else
    {
      CloseHandle(piProcInfo.hProcess);
      if (CmdLine) { free(CmdLine);CmdLine=NULL;}
      nsp_tcldstring_free(&buffer);
      return FAIL;
    }
  return OK;
}


static NspSMatrix *nsp_file2SMatrix(const char *fname)
{
  NspSMatrix *S = NULL;
  NspFile *F=NULL;
  if ((F=nsp_file_open(fname,"rb",FALSE,TRUE)) == NULLSCIFILE)
    {
      Scierror("Failed to open file %s\n",fname);
      return NULL;
    }
  if ( nsp_read_lines(F,&S,-1) == FAIL)
    {
      Scierror("Failed to read contents of file %s\n",fname);
      return NULL;
    }
  if ( nsp_smatrix_to_utf8(S) == FAIL ) 
    {
      Scierror("Failed to converts contents of file %s to utf8\n",fname);
      return NULL;
    }
  return S;
}



#endif




/**
 * int_g_spawn_async:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface to the g_spawn_async function 
 *
 * Returns: 
 **/

int int_g_spawn_async(Stack stack, int rhs, int opt, int lhs)
{
  GPid pid;
  GSpawnFlags flags = G_SPAWN_SEARCH_PATH 
    | G_SPAWN_CHILD_INHERITS_STDIN;
  gboolean rep;
  gchar *working_directory=NULL;
  GError *error=NULL;
  NspSMatrix *S;
  nsp_option opts[] ={{"wd",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  
  CheckStdRhs(1,2);
  CheckLhs(0,2);
  if ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) 
    {
      Scierror("Error: first argument of %s should be of size > 0\n",NspFname(stack));
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &working_directory) == FAIL )
    return RET_BUG;

  rep = g_spawn_async(working_directory,S->S,NULL,flags,
		      NULL, NULL, &pid,  &error);
  g_spawn_close_pid(pid);

  if ( lhs <= 0 && rep == FALSE ) 
    {
      if (error != NULL) Scierror("Error: %s\n",error->message);
      return RET_BUG;
    }

  if ( nsp_move_boolean(stack,1,rep)  == FAIL ) return RET_BUG;

  if ( lhs >= 2)
    {
      char *st = (error != NULL) ? error->message : "";
      if ( nsp_move_string(stack,2, st,-1) ==FAIL) return RET_BUG;
      if ( error != NULL) g_error_free (error);
    }
  return Max(lhs,1);
} 











