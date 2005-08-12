
#include <stdlib.h> 

void * scicos_malloc(size_t size)
{
  return malloc(size);
}

void scicos_free(void *p)
{
  free(p);
}
