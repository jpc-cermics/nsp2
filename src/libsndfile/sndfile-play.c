/*
** Copyright (C) 1999-2004 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*
* Based on the file sndfile-play.c of libsndfile 
* the part used to play sound was kept here and sounds 
* are transmited through Nsp matrices 
* Jean-Philippe Chancelier 2005
*
*/

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "nsp/object.h"

#if HAVE_ALSA_ASOUNDLIB_H
	#define ALSA_PCM_NEW_HW_PARAMS_API
	#define ALSA_PCM_NEW_SW_PARAMS_API
	#include <alsa/asoundlib.h>
	#include <sys/time.h>
#endif

#if defined (__linux__)
	#include 	<fcntl.h>
	#include 	<sys/ioctl.h>
	#include 	<sys/soundcard.h>

#elif (defined (__MACH__) && defined (__APPLE__))
	#include <Carbon.h>
	#include <CoreAudio/AudioHardware.h>

#elif (defined (sun) && defined (unix))
	#include <fcntl.h>
	#include <sys/ioctl.h>
	#include <sys/audioio.h>

#elif (OS_IS_WIN32 == 1)
	#include <windows.h>
	#include <mmsystem.h>

#endif

#include	<sndfile.h>

#define	SIGNED_SIZEOF(x)	((int) sizeof (x))
#define	BUFFER_LEN			(2048)

/*------------------------------------------------------------------------------
**	Linux/OSS functions for playing a sound.
*/

#if HAVE_ALSA_ASOUNDLIB_H

static snd_pcm_t * alsa_open (int channels,unsigned int srate) ;
static int alsa_write_double (snd_pcm_t *alsa_dev, double *data, int frames, int channels) ;
static int xrun_recovery(snd_pcm_t *handle, int err);

static int alsa_play_matrix (NspMatrix *M,int job)
{	
  SF_INFO sfinfo ;
  static snd_pcm_t * alsa_dev ;
  switch ( job ) 
    {
    case 1 : 
      /* initialisation */
      if ( M != NULL)   sfinfo.channels = M->m;
      if (sfinfo.channels < 1 || sfinfo.channels > 2)
	{	
	  Scierror("Error: channels = %d.\n", sfinfo.channels) ;
	  return FAIL;
	} ;
      sfinfo.samplerate = 22050;
      if ((alsa_dev = alsa_open (sfinfo.channels, sfinfo.samplerate))== NULL) 
	return FAIL;
      else 
	return OK;
      break;
    case 2: 
      /* play */
      return alsa_write_double (alsa_dev,M->R, M->n, M->m);
      break;
    case 3 : 
      /* stop */
      snd_pcm_close (alsa_dev) ;
      return OK;
      break;
    }
  return OK;
} /* alsa_play_matrix */

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

  /* jpc: using float64  */

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
  Sciprintf("Valeurs fixées %d,%d\n",(int) alsa_period_size, (int)buffer_size);  
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


/* XXXX : a finir il semble que snd_pcm_writei doit avoir en entrée 
 * un vecteur de longeur multiple du nombre chosit pour period_size 
 * sinon ça merde snd_pcm_writei peut se bloquer. 
 */

static int
alsa_write_double (snd_pcm_t *alsa_dev, double *data, int frames, int channels)
{	
  int total = 0 , retval ;
  while (total < frames)
    {	
      Sciprintf("before %d\n",frames - total);
      retval = snd_pcm_writei (alsa_dev, data + total * channels, frames - total) ;
      Sciprintf("after %d\n",retval);
      /* snd_pcm_prepare (alsa_dev) ; */
      if (0)
	{	
	  snd_pcm_status_t *status ;
	  snd_pcm_status_alloca (&status) ;
	  if ((retval = snd_pcm_status (alsa_dev, status)) < 0)
	    fprintf (stderr, "alsa_out: xrun. can't determine length\n") ;
	  else if (snd_pcm_status_get_state (status) == SND_PCM_STATE_XRUN)
	    {
	      struct timeval now, diff, tstamp ;
	      gettimeofday (&now, 0) ;
	      snd_pcm_status_get_trigger_tstamp (status, &tstamp) ;
	      timersub (&now, &tstamp, &diff) ;
	      fprintf (stderr, "alsa_write_float xrun: of at least %.3f msecs. resetting stream\n",
		       diff.tv_sec * 1000 + diff.tv_usec / 1000.0) ;
	    }
	} ;

      if (retval >= 0)
	{	
	  total += retval ;
	  if (total == frames)
	    return total ;
	  continue ;
	}
      else 
	{
	  Sciprintf("Error: write error %s\n", snd_strerror(retval));	
	  if (xrun_recovery(alsa_dev,retval) < 0) 
	    {
	      Scierror("Error: write error %s\n", snd_strerror(retval));
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
	  Scierror("Error: cannot recover from underrun, prepare failed: %s\n", snd_strerror(err));
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
	      Scierror("Error: cannot recover from suspend, prepare failed: %s\n", snd_strerror(err));
	      return err;
	    }
	}
      return 0;
    }
  return err;
}


#endif /* HAVE_ALSA_ASOUNDLIB_H */

/*------------------------------------------------------------------------------
**	Linux/OSS functions for playing a sound.
*/

#if defined (__linux__)

static int  linux_open_dsp_device (int channels, int srate) ;

static void
linux_play (int argc, char *argv [])
{	
  static short buffer [BUFFER_LEN] ;
  SNDFILE *sndfile ;
  SF_INFO sfinfo ;
  int		k, audio_device, readcount, subformat ;

  for (k = 1 ; k < argc ; k++)
    {	memset (&sfinfo, 0, sizeof (sfinfo)) ;

      printf ("Playing %s\n", argv [k]) ;
      if (! (sndfile = sf_open (argv [k], SFM_READ, &sfinfo)))
	{	puts (sf_strerror (NULL)) ;
	  continue ;
	} ;

      if (sfinfo.channels < 1 || sfinfo.channels > 2)
	{	printf ("Error : channels = %d.\n", sfinfo.channels) ;
	  continue ;
	} ;

      audio_device = linux_open_dsp_device (sfinfo.channels, sfinfo.samplerate) ;

      subformat = sfinfo.format & SF_FORMAT_SUBMASK ;

      if (subformat == SF_FORMAT_FLOAT || subformat == SF_FORMAT_DOUBLE)
	{	static float float_buffer [BUFFER_LEN] ;
	  double	scale ;
	  int 	m ;
	  
	  sf_command (sndfile, SFC_CALC_SIGNAL_MAX, &scale, sizeof (scale)) ;
	  if (scale < 1e-10)
	    scale = 1.0 ;
	  else
	    scale = 32700.0 / scale ;

	  while ((readcount = sf_read_float (sndfile, float_buffer, BUFFER_LEN)))
	    {	
	      for (m = 0 ; m < readcount ; m++)
		buffer [m] = scale * float_buffer [m] ;
	      write (audio_device, buffer, readcount * sizeof (short)) ;
	    } ;
	}
      else
	{	
	  while ((readcount = sf_read_short (sndfile, buffer, BUFFER_LEN)))
	    write (audio_device, buffer, readcount * sizeof (short)) ;
	} ;
      close (audio_device) ;
      sf_close (sndfile) ;
    } ;
  return ;
} /* linux_play */

static int
linux_open_dsp_device (int channels, int srate)
{	int fd, stereo, temp, error ;

  if ((fd = open ("/dev/dsp", O_WRONLY, 0)) == -1 &&
      (fd = open ("/dev/sound/dsp", O_WRONLY, 0)) == -1)
    {	
      perror ("linux_open_dsp_device : open ") ;
      exit (1) ;
    } ;

  stereo = 0 ;
  if (ioctl (fd, SNDCTL_DSP_STEREO, &stereo) == -1)
    { 	/* Fatal error */
      perror ("linux_open_dsp_device : stereo ") ;
      exit (1) ;
    } ;

  if (ioctl (fd, SNDCTL_DSP_RESET, 0))
    {	
      perror ("linux_open_dsp_device : reset ") ;
      exit (1) ;
    } ;

  temp = 16 ;
  if ((error = ioctl (fd, SOUND_PCM_WRITE_BITS, &temp)) != 0)
    {	
      perror ("linux_open_dsp_device : bitwidth ") ;
      exit (1) ;
    } ;

  if ((error = ioctl (fd, SOUND_PCM_WRITE_CHANNELS, &channels)) != 0)
    {	
      perror ("linux_open_dsp_device : channels ") ;
      exit (1) ;
    } ;

  if ((error = ioctl (fd, SOUND_PCM_WRITE_RATE, &srate)) != 0)
    {	
      perror ("linux_open_dsp_device : sample rate ") ;
      exit (1) ;
    } ;

  if ((error = ioctl (fd, SNDCTL_DSP_SYNC, 0)) != 0)
    {	
      perror ("linux_open_dsp_device : sync ") ;
      exit (1) ;
    } ;

  return 	fd ;
} /* linux_open_dsp_device */

#endif /* __linux__ */

/*------------------------------------------------------------------------------
**	Mac OS X functions for playing a sound.
*/

#if (defined (__MACH__) && defined (__APPLE__)) /* MacOSX */

typedef struct
{	AudioStreamBasicDescription		format ;

  UInt32 			buf_size ;
  AudioDeviceID 	device ;

  SNDFILE 		*sndfile ;
  SF_INFO 		sfinfo ;

  int				fake_stereo ;
  int				done_playing ;
} MacOSXAudioData ;

#include <math.h>

static OSStatus
macosx_audio_out_callback (AudioDeviceID device, const AudioTimeStamp* current_time,
			   const AudioBufferList* data_in, const AudioTimeStamp* time_in,
			   AudioBufferList*	data_out, const AudioTimeStamp* time_out,
			   void* client_data)
{	MacOSXAudioData	*audio_data ;
  int		size, sample_count, read_count, k ;
  float	*buffer ;

  /* Prevent compiler warnings. */
  device = device ;
  current_time = current_time ;
  data_in = data_in ;
  time_in = time_in ;
  time_out = time_out ;

  audio_data = (MacOSXAudioData*) client_data ;

  size = data_out->mBuffers [0].mDataByteSize ;
  sample_count = size / sizeof (float) ;

  buffer = (float*) data_out->mBuffers [0].mData ;

  if (audio_data->fake_stereo != 0)
    {	read_count = sf_read_float (audio_data->sndfile, buffer, sample_count / 2) ;

      for (k = read_count - 1 ; k >= 0 ; k--)
	{	buffer [2 * k	] = buffer [k] ;
	  buffer [2 * k + 1] = buffer [k] ;
	} ;
      read_count *= 2 ;
    }
  else
    read_count = sf_read_float (audio_data->sndfile, buffer, sample_count) ;

  /* Fill the remainder with zeroes. */
  if (read_count < sample_count)
    {	if (audio_data->fake_stereo == 0)
	memset (&(buffer [read_count]), 0, (sample_count - read_count) * sizeof (float)) ;
      /* Tell the main application to terminate. */
      audio_data->done_playing = SF_TRUE ;
    } ;

  return noErr ;
} /* macosx_audio_out_callback */

static void
macosx_play (int argc, char *argv [])
{	MacOSXAudioData 	audio_data ;
  OSStatus	err ;
  UInt32		count, buffer_size ;
  int 		k ;

  audio_data.fake_stereo = 0 ;
  audio_data.device = kAudioDeviceUnknown ;

  /*  get the default output device for the HAL */
  count = sizeof (AudioDeviceID) ;
  if ((err = AudioHardwareGetProperty (kAudioHardwarePropertyDefaultOutputDevice,
				       &count, (void *) &(audio_data.device))) != noErr)
    {	printf ("AudioHardwareGetProperty (kAudioDevicePropertyDefaultOutputDevice) failed.\n") ;
      return ;
    } ;

  /*  get the buffersize that the default device uses for IO */
  count = sizeof (UInt32) ;
  if ((err = AudioDeviceGetProperty (audio_data.device, 0, false, kAudioDevicePropertyBufferSize,
				     &count, &buffer_size)) != noErr)
    {	printf ("AudioDeviceGetProperty (kAudioDevicePropertyBufferSize) failed.\n") ;
      return ;
    } ;

  /*  get a description of the data format used by the default device */
  count = sizeof (AudioStreamBasicDescription) ;
  if ((err = AudioDeviceGetProperty (audio_data.device, 0, false, kAudioDevicePropertyStreamFormat,
				     &count, &(audio_data.format))) != noErr)
    {	printf ("AudioDeviceGetProperty (kAudioDevicePropertyStreamFormat) failed.\n") ;
      return ;
    } ;

  /* Base setup completed. Now play files. */
  for (k = 1 ; k < argc ; k++)
    {	printf ("Playing %s\n", argv [k]) ;
      if (! (audio_data.sndfile = sf_open (argv [k], SFM_READ, &(audio_data.sfinfo))))
	{	puts (sf_strerror (NULL)) ;
	  continue ;
	} ;

      if (audio_data.sfinfo.channels < 1 || audio_data.sfinfo.channels > 2)
	{	printf ("Error : channels = %d.\n", audio_data.sfinfo.channels) ;
	  continue ;
	} ;

      audio_data.format.mSampleRate = audio_data.sfinfo.samplerate ;

      if (audio_data.sfinfo.channels == 1)
	{	audio_data.format.mChannelsPerFrame = 2 ;
	  audio_data.fake_stereo = 1 ;
	}
      else
	audio_data.format.mChannelsPerFrame = audio_data.sfinfo.channels ;

      if ((err = AudioDeviceSetProperty (audio_data.device, NULL, 0, false, kAudioDevicePropertyStreamFormat,
					 sizeof (AudioStreamBasicDescription), &(audio_data.format))) != noErr)
	{	printf ("AudioDeviceSetProperty (kAudioDevicePropertyStreamFormat) failed.\n") ;
	  return ;
	} ;

      /*  we want linear pcm */
      if (audio_data.format.mFormatID != kAudioFormatLinearPCM)
	return ;

      /* Fire off the device. */
      if ((err = AudioDeviceAddIOProc (audio_data.device, macosx_audio_out_callback,
				       (void *) &audio_data)) != noErr)
	{	printf ("AudioDeviceAddIOProc failed.\n") ;
	  return ;
	} ;

      err = AudioDeviceStart (audio_data.device, macosx_audio_out_callback) ;
      if	(err != noErr)
	return ;

      audio_data.done_playing = SF_FALSE ;

      while (audio_data.done_playing == SF_FALSE)
	usleep (10 * 1000) ; /* 10 000 milliseconds. */

      if ((err = AudioDeviceStop (audio_data.device, macosx_audio_out_callback)) != noErr)
	{	printf ("AudioDeviceStop failed.\n") ;
	  return ;
	} ;

      err = AudioDeviceRemoveIOProc (audio_data.device, macosx_audio_out_callback) ;
      if (err != noErr)
	{	printf ("AudioDeviceRemoveIOProc failed.\n") ;
	  return ;
	} ;

      sf_close (audio_data.sndfile) ;
    } ;

  return ;
} /* macosx_play */

#endif /* MacOSX */


/*------------------------------------------------------------------------------
**	Win32 functions for playing a sound.
**
**	This API sucks. Its needlessly complicated and is *WAY* too loose with
**	passing pointers arounf in integers and and using char* pointers to
**  point to data instead of short*. It plain sucks!
*/

#if (OS_IS_WIN32 == 1)

#define	WIN32_BUFFER_LEN	(1<<15)

typedef struct
{	HWAVEOUT	hwave ;
  WAVEHDR		whdr [2] ;

  CRITICAL_SECTION	mutex ;		/* to control access to BuffersInUSe */
  HANDLE		Event ;			/* signal that a buffer is free */

  short		buffer [WIN32_BUFFER_LEN / sizeof (short)] ;
  int			current, bufferlen ;
  int			BuffersInUse ;

  SNDFILE 	*sndfile ;
  SF_INFO 	sfinfo ;

  sf_count_t	remaining ;
} Win32_Audio_Data ;


static void
win32_play_data (Win32_Audio_Data *audio_data)
{	int thisread, readcount ;

  /* fill a buffer if there is more data and we can read it sucessfully */
  readcount = (audio_data->remaining > audio_data->bufferlen) ? audio_data->bufferlen : (int) audio_data->remaining ;

  thisread = (int) sf_read_short (audio_data->sndfile, (short *) (audio_data->whdr [audio_data->current].lpData), readcount) ;

  audio_data->remaining -= thisread ;

  if (thisread > 0)
    {	/* Fix buffer length if this is only a partial block. */
      if (thisread < audio_data->bufferlen)
	audio_data->whdr [audio_data->current].dwBufferLength = thisread * sizeof (short) ;

      /* Queue the WAVEHDR */
      waveOutWrite (audio_data->hwave, (LPWAVEHDR) &(audio_data->whdr [audio_data->current]), sizeof (WAVEHDR)) ;

      /* count another buffer in use */
      EnterCriticalSection (&audio_data->mutex) ;
      audio_data->BuffersInUse ++ ;
      LeaveCriticalSection (&audio_data->mutex) ;

      /* use the other buffer next time */
      audio_data->current = (audio_data->current + 1) % 2 ;
    } ;

  return ;
} /* win32_play_data */

static void CALLBACK
win32_audio_out_callback (HWAVEOUT hwave, UINT msg, DWORD data, DWORD param1, DWORD param2)
{	Win32_Audio_Data	*audio_data ;

  /* Prevent compiler warnings. */
  hwave = hwave ;
  param1 = param2 ;

  if (data == 0)
    return ;

  /*
  ** I consider this technique of passing a pointer via an integer as
  ** fundamentally broken but thats the way microsoft has defined the
  ** interface.
  */
  audio_data = (Win32_Audio_Data*) data ;

  /* let main loop know a buffer is free */
  if (msg == MM_WOM_DONE)
    {	EnterCriticalSection (&audio_data->mutex) ;
      audio_data->BuffersInUse -- ;
      LeaveCriticalSection (&audio_data->mutex) ;
      SetEvent (audio_data->Event) ;
    } ;

  return ;
} /* win32_audio_out_callback */

/* This is needed for earlier versions of the M$ development tools. */
#ifndef DWORD_PTR
#define DWORD_PTR DWORD
#endif

static void
win32_play (int argc, char *argv [])
{	Win32_Audio_Data	audio_data ;

  WAVEFORMATEX wf ;
  int	k, error ;

  audio_data.sndfile = NULL ;
  audio_data.hwave = 0 ;

  for (k = 1 ; k < argc ; k++)
    {	printf ("Playing %s\n", argv [k]) ;

      if (! (audio_data.sndfile = sf_open (argv [k], SFM_READ, &(audio_data.sfinfo))))
	{	puts (sf_strerror (NULL)) ;
	  continue ;
	} ;

      audio_data.remaining = audio_data.sfinfo.frames ;
      audio_data.current = 0 ;

      InitializeCriticalSection (&audio_data.mutex) ;
      audio_data.Event = CreateEvent (0, FALSE, FALSE, 0) ;

      wf.nChannels = audio_data.sfinfo.channels ;
      wf.wFormatTag = WAVE_FORMAT_PCM ;
      wf.cbSize = 0 ;
      wf.wBitsPerSample = 16 ;

      wf.nSamplesPerSec = audio_data.sfinfo.samplerate ;

      wf.nBlockAlign = audio_data.sfinfo.channels * sizeof (short) ;

      wf.nAvgBytesPerSec = wf.nBlockAlign * wf.nSamplesPerSec ;

      error = waveOutOpen (&(audio_data.hwave), WAVE_MAPPER, &wf, (DWORD_PTR) win32_audio_out_callback,
			   (DWORD_PTR) &audio_data, CALLBACK_FUNCTION) ;
      if (error)
	{	puts ("waveOutOpen failed.") ;
	  audio_data.hwave = 0 ;
	  continue ;
	} ;

      audio_data.whdr [0].lpData = (char*) audio_data.buffer ;
      audio_data.whdr [1].lpData = ((char*) audio_data.buffer) + sizeof (audio_data.buffer) / 2 ;

      audio_data.whdr [0].dwBufferLength = sizeof (audio_data.buffer) / 2 ;
      audio_data.whdr [1].dwBufferLength = sizeof (audio_data.buffer) / 2 ;

      audio_data.whdr [0].dwFlags = 0 ;
      audio_data.whdr [1].dwFlags = 0 ;

      /* length of each audio buffer in samples */
      audio_data.bufferlen = sizeof (audio_data.buffer) / 2 / sizeof (short) ;

      /* Prepare the WAVEHDRs */
      if ((error = waveOutPrepareHeader (audio_data.hwave, &(audio_data.whdr [0]), sizeof (WAVEHDR))))
	{	printf ("waveOutPrepareHeader [0] failed : %08X\n", error) ;
	  waveOutClose (audio_data.hwave) ;
	  continue ;
	} ;

      if ((error = waveOutPrepareHeader (audio_data.hwave, &(audio_data.whdr [1]), sizeof (WAVEHDR))))
	{	printf ("waveOutPrepareHeader [1] failed : %08X\n", error) ;
	  waveOutUnprepareHeader (audio_data.hwave, &(audio_data.whdr [0]), sizeof (WAVEHDR)) ;
	  waveOutClose (audio_data.hwave) ;
	  continue ;
	} ;

      /* Fill up both buffers with audio data */
      audio_data.BuffersInUse = 0 ;
      win32_play_data (&audio_data) ;
      win32_play_data (&audio_data) ;

      /* loop until both buffers are released */
      while (audio_data.BuffersInUse > 0)
	{
	  /* wait for buffer to be released */
	  WaitForSingleObject (audio_data.Event, INFINITE) ;

	  /* refill the buffer if there is more data to play */
	  win32_play_data (&audio_data) ;
	} ;

      waveOutUnprepareHeader (audio_data.hwave, &(audio_data.whdr [0]), sizeof (WAVEHDR)) ;
      waveOutUnprepareHeader (audio_data.hwave, &(audio_data.whdr [1]), sizeof (WAVEHDR)) ;

      waveOutClose (audio_data.hwave) ;
      audio_data.hwave = 0 ;

      DeleteCriticalSection (&audio_data.mutex) ;

      sf_close (audio_data.sndfile) ;
    } ;

} /* win32_play */

#endif /* Win32 */

/*------------------------------------------------------------------------------
**	Solaris.
*/

#if (defined (sun) && defined (unix)) /* ie Solaris */

static void
solaris_play (int argc, char *argv [])
{	static short 	buffer [BUFFER_LEN] ;
  audio_info_t	audio_info ;
  SNDFILE			*sndfile ;
  SF_INFO			sfinfo ;
  unsigned long	delay_time ;
  long			k, start_count, output_count, write_count, read_count ;
  int				audio_fd, error, done ;

  for (k = 1 ; k < argc ; k++)
    {	printf ("Playing %s\n", argv [k]) ;
      if (! (sndfile = sf_open (argv [k], SFM_READ, &sfinfo)))
	{	puts (sf_strerror (NULL)) ;
	  continue ;
	} ;

      if (sfinfo.channels < 1 || sfinfo.channels > 2)
	{	printf ("Error : channels = %d.\n", sfinfo.channels) ;
	  continue ;
	} ;

      /* open the audio device - write only, non-blocking */
      if ((audio_fd = open ("/dev/audio", O_WRONLY | O_NONBLOCK)) < 0)
	{	perror ("open (/dev/audio) failed") ;
	  return ;
	} ;

      /*	Retrive standard values. */
      AUDIO_INITINFO (&audio_info) ;

      audio_info.play.sample_rate = sfinfo.samplerate ;
      audio_info.play.channels = sfinfo.channels ;
      audio_info.play.precision = 16 ;
      audio_info.play.encoding = AUDIO_ENCODING_LINEAR ;
      audio_info.play.gain = AUDIO_MAX_GAIN ;
      audio_info.play.balance = AUDIO_MID_BALANCE ;

      if ((error = ioctl (audio_fd, AUDIO_SETINFO, &audio_info)))
	{	perror ("ioctl (AUDIO_SETINFO) failed") ;
	  return ;
	} ;

      /* Delay time equal to 1/4 of a buffer in microseconds. */
      delay_time = (BUFFER_LEN * 1000000) / (audio_info.play.sample_rate * 4) ;

      done = 0 ;
      while (! done)
	{	read_count = sf_read_short (sndfile, buffer, BUFFER_LEN) ;
	  if (read_count < BUFFER_LEN)
	    {	memset (&(buffer [read_count]), 0, (BUFFER_LEN - read_count) * sizeof (short)) ;
	      /* Tell the main application to terminate. */
	      done = SF_TRUE ;
	    } ;

	  start_count = 0 ;
	  output_count = BUFFER_LEN * sizeof (short) ;

	  while (output_count > 0)
	    {	/* write as much data as possible */
	      write_count = write (audio_fd, &(buffer [start_count]), output_count) ;
	      if (write_count > 0)
		{	output_count -= write_count ;
		  start_count += write_count ;
		}
	      else
		{	/*	Give the audio output time to catch up. */
		  usleep (delay_time) ;
		} ;
	    } ; /* while (outpur_count > 0) */
	} ; /* while (! done) */

      close (audio_fd) ;
    } ;

  return ;
} /* solaris_play */

#endif /* Solaris */

/*
 *	Main function.
 */

int nsp_play (NspMatrix *M,int init)
{
#if defined (__linux__)
#if HAVE_ALSA_ASOUNDLIB_H
  if (access ("/proc/asound/cards", R_OK) == 0)
    return alsa_play_matrix(M,init);
  else
#endif
    return 0;
    /* XXXXX linux_play (argc, argv) ; */
#elif (defined (__MACH__) && defined (__APPLE__))
  macosx_play (argc, argv) ;
#elif (defined (sun) && defined (unix))
  solaris_play (argc, argv) ;
#elif (OS_IS_WIN32 == 1)
  win32_play (argc, argv) ;
#elif defined (__BEOS__)
  printf ("This program cannot be compiled on BeOS.\n") ;
  printf ("Instead, compile the file sfplay_beos.cpp.\n") ;
  return 1 ;
#else
  puts ("*** Playing sound not yet supported on this platform.") ;
  puts ("*** Please feel free to submit a patch.") ;
  return 1 ;
#endif
  return 0 ;
} /* main */

