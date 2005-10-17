#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <unistd.h>

#define ALSA_PCM_NEW_HW_PARAMS_API
#define ALSA_PCM_NEW_SW_PARAMS_API
#include <alsa/asoundlib.h>
#include <sys/time.h>

#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/soundcard.h>

#define FAIL 0
#define OK 1;
#define NNN  513 

static snd_pcm_t * alsa_open (int channels,unsigned int srate) ;
static int alsa_write_double (snd_pcm_t *alsa_dev, double *data, int frames, int channels) ;
static int xrun_recovery(snd_pcm_t *handle, int err);

static int alsa_play(void)
{
  int i;
  const int  m=22050;
  double x[m];
  static snd_pcm_t * alsa_dev ;
  if ((alsa_dev = alsa_open (1, 22050))== NULL)  return FAIL;

  for (i=0; i < m ; i++) x[i]=sin(2*M_PI*440*i/22050);

  for ( i= 1 ; i < 200 ; i++) 
    {
      fprintf(stderr,"i=%d\n",i);
      if ( alsa_write_double (alsa_dev,x,NNN,m)== FAIL) return FAIL;
    }
  snd_pcm_close (alsa_dev) ;
  return OK;
}

static snd_pcm_t *
alsa_open (int channels,unsigned int samplerate)
{	
  const char * device = "plughw:0" ;
  snd_pcm_t *alsa_dev ;
  snd_pcm_hw_params_t *hw_params ;
  snd_pcm_uframes_t buffer_size, start_threshold ;
  snd_pcm_uframes_t alsa_period_size, alsa_buffer_frames ;
  snd_pcm_sw_params_t *sw_params ;

  int err ;

  /* XXX : il faut pouvoir donner ce parametre en entree 
   * et verifier s'il a été accepté 
   * cela conditionne les write que l'on fait ensuite qui doivent avoir cette 
   * longueur ou plus mais pas moins sinon on bloque sur un read.
   */
  alsa_period_size = 512 ; /* au lieu de 512 */
  alsa_buffer_frames = 2*8 * alsa_period_size ;

  if ((err = snd_pcm_open (&alsa_dev, device, SND_PCM_STREAM_PLAYBACK,0)) < 0)
    {	
      fprintf (stderr, "cannot open audio device \"%s\" (%s)\n", device, snd_strerror (err)) ;
      return NULL ;
    } ;
  
  /* snd_pcm_nonblock (alsa_dev,0);*/

  if ((err = snd_pcm_hw_params_malloc (&hw_params)) < 0)
    {	
      fprintf (stderr, "cannot allocate hardware parameter structure (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_hw_params_any (alsa_dev, hw_params)) < 0)
    {	
      fprintf (stderr, "cannot initialize hardware parameter structure (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_hw_params_set_access (alsa_dev, hw_params, SND_PCM_ACCESS_RW_INTERLEAVED)) < 0)
    {	
      fprintf (stderr, "cannot set access type (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_hw_params_set_format (alsa_dev, hw_params, SND_PCM_FORMAT_FLOAT64)) < 0)
    {	
      fprintf (stderr, "cannot set sample format (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_hw_params_set_rate_near (alsa_dev, hw_params, &samplerate, 0)) < 0)
    {	
      fprintf (stderr, "cannot set sample rate (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_hw_params_set_channels (alsa_dev, hw_params, channels)) < 0)
    {	
      fprintf (stderr, "cannot set channel count (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_hw_params_set_buffer_size_near (alsa_dev, hw_params, &alsa_buffer_frames)) < 0)
    {	
      fprintf (stderr, "cannot set buffer size (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;


  if ((err = snd_pcm_hw_params_set_period_size_near (alsa_dev, hw_params, &alsa_period_size, 0)) < 0)
    {	
      fprintf (stderr, "cannot set period size (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_hw_params (alsa_dev, hw_params)) < 0)
    {	
      fprintf (stderr, "cannot set parameters (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  /* extra check: if we have only one period, this code won't work */
  snd_pcm_hw_params_get_period_size (hw_params, &alsa_period_size, 0) ;
  snd_pcm_hw_params_get_buffer_size (hw_params, &buffer_size) ;
  fprintf(stderr,"Valeurs fixées %d,%d\n",(int) alsa_period_size, (int)buffer_size);  
  if (alsa_period_size == buffer_size)
    {	
      fprintf (stderr, "Can't use period equal to buffer size (%lu == %lu)", alsa_period_size, buffer_size) ;
      return NULL ;
    } ;

  snd_pcm_hw_params_free (hw_params) ;

  if ((err = snd_pcm_sw_params_malloc (&sw_params)) != 0)
    {	
      fprintf (stderr, "%s: snd_pcm_sw_params_malloc: %s", __func__, snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_sw_params_current (alsa_dev, sw_params)) != 0)
    {	
      fprintf (stderr, "%s: snd_pcm_sw_params_current: %s\n", __func__, snd_strerror (err)) ;
      return NULL ;
    } ;

  /* allow the transfer when at least period_size samples can be processed */
  if ((err=snd_pcm_sw_params_set_avail_min(alsa_dev,sw_params,alsa_period_size)) != 0) 
    {
      fprintf (stderr, "%s: Unable ro set avail min for playback: %s\n",__func__, snd_strerror (err)) ;
      return NULL ;
    }

  /* note: set start threshold to delay start until the ring buffer is full */
  snd_pcm_sw_params_current (alsa_dev, sw_params) ;

  if ((err = snd_pcm_sw_params_set_xfer_align (alsa_dev,sw_params,1)) < 0)
    {	
      fprintf (stderr, "cannot set xfer align (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;


  start_threshold = (buffer_size / alsa_period_size) * alsa_period_size;
  if (start_threshold < 1)  start_threshold = 1 ;
  if ((err = snd_pcm_sw_params_set_start_threshold (alsa_dev, sw_params, start_threshold)) < 0)
    {	
      fprintf (stderr, "cannot set start threshold (%s)\n", snd_strerror (err)) ;
      return NULL ;
    } ;

  if ((err = snd_pcm_sw_params (alsa_dev, sw_params)) != 0)
    {	
      fprintf (stderr, "%s: snd_pcm_sw_params: %s", __func__, snd_strerror (err)) ;
      return NULL ;
    } ;
  snd_pcm_sw_params_free (sw_params) ;

  snd_pcm_reset (alsa_dev) ;

  return alsa_dev ;
} /* alsa_open */


static int
alsa_write_double (snd_pcm_t *alsa_dev, double *data, int frames, int channels)
{	
  int total = 0 , retval ;
  while (total < frames)
    {	
      fprintf(stderr,"before %d\n",frames - total);
      retval = snd_pcm_writei (alsa_dev, data + total * channels, frames - total) ;
      fprintf(stderr,"after %d\n",retval);
      /* snd_pcm_prepare (alsa_dev) ; */
      if (retval >= 0)
	{	
	  total += retval ;
	  if (total == frames)
	    return total ;
	  continue ;
	}
      else 
	{
	  fprintf(stderr,"Error: write error %s\n", snd_strerror(retval));	
	  if (xrun_recovery(alsa_dev,retval) < 0) 
	    {
	      fprintf(stderr,"Error: write error %s\n", snd_strerror(retval));
	      return FAIL;
	    }
	  continue;
	}
    }
  return total ;
} 

/*
 *   Underrun and suspend recovery
 */

static int xrun_recovery(snd_pcm_t *handle, int err)
{
  if (err == -EPIPE) 
    {    
      /* under-run */
      err = snd_pcm_prepare(handle);
      if (err < 0)
	{
	  fprintf(stderr,"Error: cannot recover from underrun, prepare failed: %s\n", snd_strerror(err));
	  return err;
	}
      return 0;
    } 
  else if (err == -ESTRPIPE) 
    {
      while ((err = snd_pcm_resume(handle)) == -EAGAIN)
	sleep(1);       /* wait until the suspend flag is released */
      if (err < 0) 
	{
	  err = snd_pcm_prepare(handle);
	  if (err < 0)
	    {
	      fprintf(stderr,"Error: cannot recover from suspend, prepare failed: %s\n", snd_strerror(err));
	      return err;
	    }
	}
      return 0;
    }
  return err;
}


int main (int agrc, char *argv[])
{
  alsa_play();
  return 0 ;
}


