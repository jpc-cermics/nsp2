
#include "tclInt.h"
#include "tclPort.h"


nsp_string nsp_dirname (char *fileName)
{
  nsp_string result = NULL;
  nsp_tcldstring buffer;
  int pargc;
  char **pargv = NULL;
  nsp_tcldstring_init (&buffer);
  Tcl_SplitPath (fileName, &pargc, &pargv);
  if ((pargc == 1) && (*fileName == '~'))
    {
      /*
       * If there is only one element, and it starts with a tilde,
       * perform tilde substitution and resplit the path.
       */
      ckfree ((char *) pargv);
      fileName = Tcl_TranslateFileName (fileName, &buffer);
      if (fileName == NULL)
	{
	  goto done;
	}
      Tcl_SplitPath (fileName, &pargc, &pargv);
      nsp_tcldstring_set_length (&buffer, 0);
    }
  /*
   * Return all but the last component.  If there is only one
   * component, return it if the path was non-relative, otherwise
   * return the current directory.
   */
  if (pargc > 1)
    {
      Tcl_JoinPath (pargc - 1, pargv, &buffer);
      result = nsp_new_string(nsp_tcldstring_value (&buffer), buffer.length);
    }
  else if ((pargc == 0) || (Tcl_GetPathType (pargv[0]) == TCL_PATH_RELATIVE))
    {
      result = nsp_new_string((tclPlatform == TCL_PLATFORM_MAC) ? ":" : ".", 1);
    }
  else
    {
      result = nsp_new_string(pargv[0], -1);
    }
done:
  if (pargv != NULL) ckfree ((char *) pargv);
  nsp_tcldstring_free (&buffer);
  return result;
}

