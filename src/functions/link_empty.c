
void SciLink(iflag,rhs,ilib,files,en_names,strf)
     int iflag,*ilib,*rhs;
     char *files[],*en_names[],*strf;
{
  Sciprintf("Sorry: Dynamic linking is not implemented  \r\n");
}


void C2F(isciulink)(i) 
     integer *i;
{
  Sciprintf("Sorry: Unlinking is not implemented \r\n");
}

int LinkStatus()
{
  return(0);
}
