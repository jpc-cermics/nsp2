
void SciLink(iflag,rhs,ilib,files,en_names,strf)
     int iflag,*ilib,*rhs;
     char *files[],*en_names[],*strf;
{
  Sciprintf("Sorry: Dynamic linking is not implemented  \r\n");
}

/**
 * nsp_delete_symbols:
 * @ishared: integer 
 * 
 * remove from link table the entries which were 
 * linked from shared library @ishared.
 *
 **/

void nsp_delete_symbols(int i)
{
  Sciprintf("Sorry: Unlinking is not implemented \r\n");
}

int nsp_link_status()
{
  return(0);
}
