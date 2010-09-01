/* Nsp
 * Copyright (C) 2003-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 * win32 utilities.
 *------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <windows.h>
/* interface conflicts with nsp objects.h */
#ifdef interface 
#undef interface 
#endif 

#include <nsp/nsp.h>
#include <nsp/matrix.h>
#include <nsp/smatrix.h>
#include <nsp/interf.h>
#include <nsp/system.h>

#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383

static NspSMatrix *nsp_query_registry_from_hkey(HKEY hKey, int *leaf);
static NspSMatrix *nsp_query_registry_from_names(char *key,char *subkey,int *leaf);
static HKEY hkey_from_string(char *string);
static NspObject *nsp_query_registry_value(char *key,char *subkey,char *name);
static int nsp_registry_set_value(char *key,char *subkey,char *name,NspObject *Obj);
static void nsp_registry_delete_key(char *key,char *subkey);

int int_nsp_query_registry(Stack stack,int rhs,int opt,int lhs)
{
  int delete = FALSE;
  nsp_option opts[] ={{"delete",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int leaf;
  NspObject *Ret = NULL;
  char *str1,*str2,*str3=NULL;
  CheckStdRhs(2,4);
  CheckLhs(0,2);
  if ((str1 = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((str2 = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ( get_optional_args(stack, rhs, opt, opts, &delete) == FAIL )
    return RET_BUG;
  if ( rhs -opt == 2 )
    {
      NspSMatrix *SMat;
      /*
       * get node or leaf information or delete a key 
       */
      if ( delete == TRUE )
	{
	  CheckLhs(0,0);
	  nsp_registry_delete_key(str1,str2);
	  return 0;
	}
      else 
	{
	  if (( SMat = nsp_query_registry_from_names(str1,str2,&leaf))== NULL) return RET_BUG;
	  MoveObj(stack,1,NSP_OBJECT(SMat));
	  if ( lhs == 2 ) 
	    {
	      if ( nsp_move_string(stack,2,(leaf) ? "l":"n",-1) ==FAIL) return RET_BUG;
	    }
	}
    }
  else if ( rhs -opt == 3 )
    {
      /* get a value 
       *
       */
      if ((str3 = GetString(stack,3)) == (char*)0) return RET_BUG;
      if ((Ret = nsp_query_registry_value(str1,str2,str3))  == NULL) return RET_BUG;
      MoveObj(stack,1,Ret);
      if ( lhs == 2 ) 
	{
	  if ( nsp_move_string(stack,2,"v",-1) ==FAIL) return RET_BUG;
	}
    }
  else if ( rhs -opt == 4 )
    {
      NspObject *obj;
      /*
       * set a value 
       */
      CheckLhs(0,0);
      if ((str3 = GetString(stack,3)) == (char*)0) return RET_BUG;
      if ((obj =nsp_get_object(stack,4)) == NULLOBJ) return RET_BUG;
      if ( ( IsMat(obj) && ((NspMatrix *) obj)->mn == 1)
	   || ( IsSMat(obj) && ((NspSMatrix *) obj)->mn == 1))
	{
	  if ( nsp_registry_set_value(str1,str2,str3,obj) == FAIL) 
	    return RET_BUG;
	}
      else
	{
	  Scierror("Error: argument 4 of %s should be a string or a scalar\n",NspFname(stack));
	  return RET_BUG;
	}
      return 0;
    }
  return Max(lhs,1);
}

/**
 * nsp_query_registry_value:
 * @key: a string 
 * @subkey:  a string 
 * @name:  a string 
 * 
 * gets the value associated to name @name in the key given by @key/@subkey.
 * Only integer and string values are searched.
 *
 * Returns: a new #NspObject or %NULL.
 **/

static NspObject * nsp_query_registry_value(char *key,char *subkey,char *name)
{
  DWORD Length=MAX_PATH, size=4, Type;
  NspObject *Ret=NULLOBJ;
  HKEY hkey,hkey1;
  int  Num=0;
  char Line[MAX_PATH];
  hkey =hkey_from_string(key);
  /* KEY_ENUMERATE_SUB_KEYS  */
  if ( RegOpenKeyEx(hkey,subkey, 0, KEY_READ  , &hkey1) != ERROR_SUCCESS )
    return NULL;
  if ( RegQueryValueEx(hkey1, name, NULL, &Type , (LPBYTE)&Num, &size) == ERROR_SUCCESS 
       && Type == REG_DWORD)
    {
      if ((Ret= (NspObject *) nsp_matrix_create(NVOID,'r',1,1))== NULL)
	return NULL;
      ((NspMatrix *) Ret)->R[0]= Num;
    }
  else if ( RegQueryValueEx(hkey1, name, NULL, &Type, (LPBYTE)&Line, &Length) == ERROR_SUCCESS 
	    &&  Type == REG_SZ )
    {
      if ((Ret= (NspObject *) nsp_smatrix_create(NVOID,1,1,Line,1))== NULL)
	return NULL;
    }
  RegCloseKey(hkey1);
  return Ret;
}

/**
 * nsp_query_registry_from_names:
 * @key: a char pointer 
 * @subkey: a char pointer 
 * 
 * given a pair of names (key/subkey) this function checks if it corresponds to an 
 * HKEY and in case of success returns information about the HKEY as given by the 
 * function @nsp_query_registry_from_hkey(). 
 * example: nsp_query_registry_from_names("HKEY_LOCAL_MACHINE","Software\\GTK\\2.0",&leaf);
 * 
 * Returns: a new #NspMatrix 
 **/

static NspSMatrix *nsp_query_registry_from_names(char *key,char *subkey,int *leaf)
{
  HKEY hkey, hkey1=NULL;
  hkey =hkey_from_string(key);
			
  if ( RegOpenKeyEx(hkey,subkey, 0, KEY_READ , &hkey1) != ERROR_SUCCESS )
    return NULL;
  return nsp_query_registry_from_hkey(hkey1,leaf);
}

/**
 * hkey_from_string:
 * @string: a char pointer 
 * 
 * return keys associated to predefined keywords.
 * 
 * Returns: a possible HKEY 
 **/

static HKEY hkey_from_string(char *string)
{
  if ( strcmp(string,"HKEY_CLASSES_ROOT") == 0 ) 
    return HKEY_CLASSES_ROOT;
  else if ( strcmp(string,"HKEY_CURRENT_USER") == 0 ) 
    return HKEY_CURRENT_USER;
  else if ( strcmp(string,"HKEY_LOCAL_MACHINE") == 0 )
    return HKEY_LOCAL_MACHINE;
  else if ( strcmp(string,"HKEY_USERS") == 0 )
    return HKEY_USERS;
  else if ( strcmp(string,"HKEY_DYN_DATA") == 0 )	
    return HKEY_DYN_DATA;
  return NULL;
}


/**
 * nsp_query_registry_from_hkey:
 * @hKey: an HKEY 
 * 
 * given a key this function returns information on the key and a set of names. 
 * If the key is a node of the registry tree, the names 
 * of the key subnodes are returned and leaf is set to FALSE. If the key is a 
 * leaf the names of the leaf values are returned and leaf is set to TRUE. 
 * In other cases the returned value is NULL. 
 * 
 * For example str=registry("HKEY_LOCAL_MACHINE","Software\\GTK\\2.0");
 *      will return ['DllPath','Path','Version'] and leaf = %t.
 * str=registry("HKEY_LOCAL_MACHINE","Software\\GTK") will return '2.0' and 
 *      leaf = %f.
 * str=registry("HKEY_LOCAL_MACHINE","Software\\GTK\\2.0\\Path") will return 
 *      NULL.
 * 
 * 
 * Returns: NULL or a #SMat containing the subkey names.
 **/
 
static  NspSMatrix * nsp_query_registry_from_hkey(HKEY hKey, int *leaf) 
{ 
  TCHAR    achClass[MAX_PATH] = TEXT("");  /* buffer for class name  */
  DWORD    cchClassName = MAX_PATH;  /* size of class string  */
  DWORD    cSubKeys=0;               /* number of subkeys  */
  DWORD    cbMaxSubKey;              /* longest subkey size  */
  DWORD    cchMaxClass;              /* longest class string  */
  DWORD    cValues;              /* number of values for key  */
  DWORD    cchMaxValue;          /* longest value name  */
  DWORD    cbMaxValueData;       /* longest value data  */
  DWORD    cbSecurityDescriptor; /* size of security descriptor  */
  FILETIME ftLastWriteTime;      /* last write time  */
  DWORD i, retCode; 
  NspSMatrix *S = NULL;

  /* Get the class name and the value count.  */
  retCode = RegQueryInfoKey(
			    hKey,                    /* key handle  */
			    achClass,                /* buffer for class name  */
			    &cchClassName,           /* size of class string  */
			    NULL,                    /* reserved  */
			    &cSubKeys,               /* number of subkeys  */
			    &cbMaxSubKey,            /* longest subkey size  */
			    &cchMaxClass,            /* longest class string  */
			    &cValues,                /* number of values for this key  */
			    &cchMaxValue,            /* longest value name  */
			    &cbMaxValueData,         /* longest value data  */
			    &cbSecurityDescriptor,   /* security descriptor  */
			    &ftLastWriteTime);       /* last write time  */
  
  if (cSubKeys)
    {
      /* Enumerate the subkeys, until RegEnumKeyEx fails. */
      DWORD    cbName ;  /* size of name string  */
      TCHAR    achKey[MAX_KEY_LENGTH];   /* buffer for subkey name */
      /*
       *  Sciprintf( "\nNumber of subkeys: %d max size = %d\n", cSubKeys,cbMaxSubKey);
       */
      if ((S = nsp_smatrix_create(NVOID,0,0, NULL,0))== NULL) return NULL;
      for (i=0; i<cSubKeys; i++) 
        { 
	  cbName = MAX_KEY_LENGTH; 
	  retCode = RegEnumKeyEx(hKey, i, achKey, &cbName, NULL,  NULL,  NULL, &ftLastWriteTime); 
	  switch ( retCode )
	    {
	    case  ERROR_SUCCESS:
	      /* Sciprintf("(%d) %s\n", i+1, achKey); */
	      if ( nsp_row_smatrix_append_string(S, achKey) != OK) 
		{
		  return NULL;
		}
	      break;
	    case  ERROR_ACCESS_DENIED:
	      Sciprintf("(%d) key not found %d\n", i+1,retCode);
	      break;
	    default: 
	      Sciprintf("(%d) key not found or acces denied \n", i+1);
	      break;
	    }
        }
      *leaf = FALSE;
    }
  
  if (cValues) 
    {
      /* Enumerate the key values.  
       * Sciprintf( "\nNumber of values: %d\n", cValues);
       */
      TCHAR  achValue[MAX_VALUE_NAME]; 
      DWORD cchValue = MAX_VALUE_NAME; 
      if ((S = nsp_smatrix_create(NVOID,0,0, NULL,0))== NULL) return NULL;

      for (i=0, retCode=ERROR_SUCCESS; i<cValues; i++) 
        { 
	  cchValue = MAX_VALUE_NAME; 
	  achValue[0] = '\0'; 
	  retCode = RegEnumValue(hKey, i, 
				 achValue, 
				 &cchValue, 
				 NULL, 
				 NULL,
				 NULL,
				 NULL);
	  if (retCode == ERROR_SUCCESS ) 
            { 
	      /* Sciprintf("(%d) %s\n", i+1, achValue); */
	      if ( nsp_row_smatrix_append_string(S, achValue) != OK) 
		{
		  return NULL;
		}
	    } 
	}
      *leaf = TRUE;
    }
  return S;
}



/**
 * nsp_registry_set_value:
 * @key: 
 * @subkey: 
 * @name: 
 * @Obj: 
 * 
 * 
 * 
 * Returns: 
 **/

static int nsp_registry_set_value(char *key,char *subkey,char *name,NspObject *Obj)
{
  HKEY hkey,hkey1;
  DWORD result,dwsize=4, retCode;
  hkey =hkey_from_string(key);
  retCode= RegCreateKeyEx(hkey, subkey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hkey1, &result);
  if ( retCode != ERROR_SUCCESS) return FAIL;
  /* result can be REG_CREATED_NEW_KEY or REG_OPENED_EXISTING_KEY */
  if ( IsMat(Obj) )
    {
      int ivalue = ((NspMatrix *) Obj)->R[0];
      RegSetValueEx(hkey1, name , 0, REG_DWORD, (LPBYTE) &ivalue, dwsize);
    }
  else
    {
      char *str = ((NspSMatrix *) Obj)->S[0];
      RegSetValueEx(hkey1, name , 0, REG_SZ, (LPBYTE) str, strlen(str)+1);
    }
  RegCloseKey(hkey1);
  return OK;
}


/**
 * nsp_registry_delete_key:
 * @key: 
 * @subkey: 
 * 
 * Returns: 
 **/

static void nsp_registry_delete_key(char *key,char *subkey)
{
  HKEY hkey = hkey_from_string(key);
  if ( hkey == NULL) return;
  RegDeleteKey(hkey, subkey);
}

