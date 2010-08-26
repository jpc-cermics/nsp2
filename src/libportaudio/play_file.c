/* Nsp
 * Copyright (C) 2009-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 * Interface with the portaudio library. 
 *
 * The main function is nsp_play_file 
 * which is interface as play_file 
 *
 */

#include <glib.h>
#include <gtk/gtk.h>
#include <sndfile.h>
#include <portaudio.h>
#include <signal.h> 
#include <nsp/interf.h>
#include "pansp.h"

/* transmited data to callback */

typedef struct {
  SNDFILE *sndFile; /* soundfile */
  SF_INFO sfInfo;   /* sndfile informations */
} cb_data_file;

/* transmited data to the thread */

typedef struct {
  char* file;      /* path to file to be played */
  int o_device;    /* play on o_device or default device if -1 */
  int err;         /* an error occured */
  IOFun  pa_print;  /* function to be used for error report */
} thread_data;

static thread_data threadData ={0};

static void playfile( thread_data *threadData);

gpointer play_thread(gpointer data)
{
  playfile((thread_data *) data);
  return NULL;
}



/**
 * nsp_play_file:
 * @file: 
 * @sync: 
 * @device: 
 * 
 * Play a wav file using linsndfile for reading data 
 * and portaudio to play the file contents. 
 * The file is played in a thread. If sync is set to %TRUE
 * then a gtk_main will wait for the end of the play 
 * and the play can be aborted by a Ctrl-C.
 * When sync is %FALSE the play can be interupted by a 
 * call with file set to "" or %NULL or reset by an 
 * other call to nsp_play_file().
 * 
 * Returns: %OK or %FAIL 
 **/

/* XXX we could add a menu to stop a current play.
 *
 */

int nsp_play_file(const char *file,int sync,int device)
{
  nsp_finish_pa_thread();
  if ( file == NULL ||  file[0]=='\0') return OK;
  /* play in a thread */
  nsp_pa_thread_set_status(NSP_PA_ACTIVE);

  if ( threadData.file != NULL) g_free( threadData.file);
  /* we need here to copy file */
  threadData.file =  g_strdup(file);
  threadData.o_device = device;
  threadData.pa_print = Scierror;
  if (!g_thread_supported ()) g_thread_init (NULL);
  g_thread_create(play_thread,&threadData,FALSE,NULL);
  /* if sync is TRUE */
  if ( sync == TRUE )
    {
      /* just print in case of error */
      threadData.pa_print = Sciprintf;
      /* we need to wait for the end */
      guint timer_pa = g_timeout_add(100,  (GSourceFunc) timeout_portaudio , NULL);
      signal(SIGINT,controlC_handler_portaudio);
      while (1) 
	{
	  gtk_main();
	  /* be sure that gtk_main_quit was activated by proper event */
	  if ( nsp_pa_thread_get_status() == NSP_PA_INACTIVE ) break;
	}
      g_source_remove(timer_pa);
      /* back to default */
      signal(SIGINT,controlC_handler);
      g_free(threadData.file);
      threadData.file = NULL;
      return threadData.err ;
    }
  return OK;
}

/* portaudio callback 
 *
 */

static int cb_playfile( const void *inputBuffer, void *outputBuffer,
			unsigned long framesPerBuffer,
			const PaStreamCallbackTimeInfo* timeInfo,
			PaStreamCallbackFlags statusFlags,
			void *cbdata)
{
  int i;
  float *out= outputBuffer;
  cb_data_file *data = cbdata;
  unsigned int read = framesPerBuffer, eread;
  eread= sf_readf_float(data->sndFile,out,read);
  for (i= eread ; i < read ; i++) out[i]=0;
  return ( eread < read ) ? paComplete : paContinue;
}

/* play a wav file. data read by libsndfile.
 */

static void playfile(thread_data *data)
{
  int max;
  cb_data_file cbdata;   /* used to transmit informations to callback */
  PaStream *ostream; 
  PaStreamParameters ostream_p;
  PaError err;   

  data->err=OK;

  if ((err = Pa_Initialize()) != paNoError)
    {
      data->pa_print("Error: in portaudio, %s\n",Pa_GetErrorText(err));
      data->err=FAIL; goto end;
    }

  cbdata.sfInfo.format = 0;
  cbdata.sndFile = sf_open(data->file, SFM_READ, &cbdata.sfInfo);
  if (!cbdata.sndFile) 
    {
      data->pa_print("Error: sf_open failed to open file %s\n",data->file);
      data->err=FAIL;goto end;
    }
  
  ostream_p.device = data->o_device;
  if ( ostream_p.device < 0  ) 
    ostream_p.device = Pa_GetDefaultOutputDevice();
  if ( ostream_p.device > Pa_GetDeviceCount() )
    {
      Sciprintf("Warning: device %d is out of range. I will use %d\n",
		ostream_p.device,
		Pa_GetDefaultOutputDevice());
      ostream_p.device = Pa_GetDefaultOutputDevice();
    }
  if ( ostream_p.device == paNoDevice ) 
    {
      data->pa_print("Error: in portaudio, output device %d not found \n",
	       data->o_device);
      data->err=FAIL;goto end;
  }
  
  max = Pa_GetDeviceInfo(ostream_p.device)->maxOutputChannels;

  if ( max < cbdata.sfInfo.channels)
    {
      data->pa_print("Error: in portaudio, output device max channel is %d < %s \n",
	       max,  cbdata.sfInfo.channels);
      data->err=FAIL;goto end;
    }
  
  ostream_p.channelCount = cbdata.sfInfo.channels;
  ostream_p.sampleFormat = paFloat32;
  ostream_p.suggestedLatency = Pa_GetDeviceInfo(ostream_p.device )->defaultLowOutputLatency;
  ostream_p.hostApiSpecificStreamInfo = 0;

  if ((err = Pa_IsFormatSupported(NULL, &ostream_p, cbdata.sfInfo.samplerate))
      != paNoError)
    {
      data->pa_print("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto end;
    }

  err = Pa_OpenStream(&ostream,
		      NULL,
		      &ostream_p,
		      cbdata.sfInfo.samplerate,
		      512, /* frames per buffer */
		      paNoFlag,
		      cb_playfile,
		      &cbdata );
  if (err)
    {
      data->pa_print("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      goto end;
    }

  if ((err = Pa_StartStream(ostream))!= paNoError)
    {
      data->pa_print("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto end;
    }

  while ( Pa_IsStreamActive(ostream) ) {
    if ( nsp_pa_thread_get_status() == NSP_PA_END ) 
      {
	/* stopped by the user in the other thread */
	Pa_AbortStream(ostream);
	break;
      }
    Pa_Sleep(500);
  }
    
  if ((err = Pa_CloseStream(ostream)) != paNoError)
    {
      data->pa_print("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto end;
    }
		
 end :
  if (cbdata.sndFile != NULL) sf_close(cbdata.sndFile);
  Pa_Terminate();
  nsp_pa_thread_set_status(NSP_PA_INACTIVE);
}




