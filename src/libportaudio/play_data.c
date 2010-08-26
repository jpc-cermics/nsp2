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
 */

#include <glib.h>
#include <gtk/gtk.h>
#include <sndfile.h>
#include <portaudio.h>
#include <signal.h> 
#include <nsp/interf.h>
#include "pansp.h"
#include <nsp/matrix.h>


#define FRAMES_PER_BUFFER   (1024)

/* transmited data to callback */

typedef struct {
  NspMatrix* M;    /* matrix to be played nchannels x samples */
  int position;
} cb_data;

/* transmited data to the thread */

typedef struct {
  NspMatrix* M;    /* matrix to be played nchannels x samples */
  int o_device;    /* play on o_device or default device if -1 */
  int err;         /* an error occured */
  int sample_rate; /* sample rate */
  IOFun  pa_print; /* function to be used for error report */
} thread_data;

static thread_data threadData ={0};

static void play_data( thread_data *srvData);

gpointer play_data_thread(gpointer data)
{
  play_data((thread_data *) data);
  return NULL;
}


int nsp_play_data(NspMatrix *M,int sample_rate, int sync,int device)
{
  NspMatrix *Mc;
  nsp_finish_pa_thread();
  if ( M->m == 0 || M->n == 0) return OK;
  if ((  Mc=nsp_matrix_copy(M))==NULLMAT) return FAIL;
  /* play in a thread */
  nsp_pa_thread_set_status(NSP_PA_ACTIVE);
  /* nsp_ignore_thread_log(); */
  if ( threadData.M != NULL) nsp_matrix_destroy(threadData.M);
  /* we need here to copy M*/
  threadData.M =  Mc;
  threadData.o_device = device;
  threadData.sample_rate = sample_rate;
  threadData.pa_print = Scierror;
  if (!g_thread_supported ()) g_thread_init (NULL);
  g_thread_create(play_data_thread,&threadData,FALSE,NULL);
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
      nsp_matrix_destroy(threadData.M);
      threadData.M=NULL;
      return threadData.err ;
    }
  return OK;
}

/* callback 
 *
 */

static int cb_play_data( const void *inputBuffer, void *outputBuffer,
			 unsigned long framesPerBuffer,
			 const PaStreamCallbackTimeInfo* timeInfo,
			 PaStreamCallbackFlags statusFlags,
			 void *cbdata)
{
  int i;
  float *out= outputBuffer;
  cb_data *data = cbdata;
  int n = Min(framesPerBuffer*data->M->m,data->M->mn - data->position);
  for ( i= 0 ; i <  n ; i++) 
    out[i]= (float) data->M->R[data->position + i];
  data->position += n;
  for ( ; i < framesPerBuffer*data->M->m ; i++) out[i]=0;
  return ( data->position >= data->M->mn ) ? paComplete : paContinue;
}

/* play a wav file. data read by libsndfile.
 */

static void play_data(thread_data *data)
{
  int max;
  cb_data cbdata = {data->M,0}; 
  PaStream *ostream; 
  PaStreamParameters ostream_p;
  PaError err;   

  data->err=OK;

  if ((err = Pa_Initialize()) != paNoError)
    {
      data->pa_print("Error: in portaudio, %s\n",Pa_GetErrorText(err));
      data->err=FAIL; goto end;
    }
  ostream_p.device = data->o_device;
  if ( ostream_p.device == -1 ) 
    ostream_p.device = Pa_GetDefaultOutputDevice();

  if ( ostream_p.device == paNoDevice ) 
    {
      data->pa_print("Error: in portaudio, output device %d not found \n",
	       data->o_device);
      data->err=FAIL;goto end;
  }
  
  max = Pa_GetDeviceInfo(ostream_p.device)->maxOutputChannels;

  if ( max < data->M->m )
    {
      data->pa_print("Error: in portaudio, output device max channel is %d < %d \n",
	       max, data->M->m);
      data->err=FAIL;goto end;
    }
  
  ostream_p.channelCount = data->M->m;
  ostream_p.sampleFormat = paFloat32;
  ostream_p.suggestedLatency = Pa_GetDeviceInfo(ostream_p.device )->defaultLowOutputLatency;
  ostream_p.hostApiSpecificStreamInfo = 0;

  if ((err = Pa_IsFormatSupported(NULL, &ostream_p, data->sample_rate)) != paNoError)
    {
      data->pa_print("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto end;
    }

  err = Pa_OpenStream(&ostream,
		      NULL,
		      &ostream_p,
		      data->sample_rate,
		      FRAMES_PER_BUFFER, /* frames per buffer */
		      paNoFlag,
		      cb_play_data,
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
  Pa_Terminate();
  nsp_pa_thread_set_status(NSP_PA_INACTIVE);
}

