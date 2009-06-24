/* Nsp
 * Copyright (C) 2009 Jean-Philippe Chancelier Enpc/Cermics
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
#include <nsp/matutil.h>

#include "pansp.h"

#define FRAMES_PER_BUFFER   (1024)

/* transmited data to callback */

typedef struct {
  NspMatrix* M;    /* matrix to be played nchannels x samples */
  int position;
} cb_data;

/* transmited data to the thread */

typedef struct {
  NspMatrix* M;    /* matrix to be played nchannels x samples */
  int i_device;    /* record on  i_device or default device if -1 */
  int err;         /* an error occured */
  IOFun  pa_print;  /* function to be used for error report */
  int sample_rate;
  int channels;
  int o_device;       /* play recorded data on play device */
} thread_data;

static thread_data threadData ={0};


static void record_data( thread_data *srvData);

gpointer record_data_thread(gpointer data)
{
  record_data((thread_data *) data);
  return NULL;
}


int nsp_record_data(NspMatrix **M,int seconds,int sample_rate,int channels, int device, int o_device)
{
  int inc=-1;
  NspMatrix *Loc=NULL;
  *M= NULL;
  nsp_finish_pa_thread();
  /* record in a thread */
  nsp_pa_thread_set_status(NSP_PA_ACTIVE);
  /* create a matrix for storing data */
  if ( seconds != -1 ) 
    {
      if ( (Loc = nsp_matrix_create(NVOID,'r',channels, seconds*sample_rate)) == NULLMAT ) 
	return FAIL;
    }
  threadData.M = Loc;
  threadData.i_device = device;
  threadData.sample_rate = sample_rate;
  threadData.channels = channels;
  threadData.pa_print = Scierror;
  threadData.o_device = o_device;
  if (!g_thread_supported ()) g_thread_init (NULL);
  g_thread_create(record_data_thread,&threadData,FALSE,NULL);
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
  if ( seconds == -1 ) 
    {
      Loc =  threadData.M;
    }
  nsp_float2double (&Loc->mn, Loc->F, &inc, Loc->R, &inc);
  *M= threadData.M;
  /* */
  threadData.M=NULL;
  if ( threadData.err == FAIL ) 
    {
      nsp_matrix_destroy(Loc);
      *M= NULL;
    }
  return threadData.err ;
}



static void record_data(thread_data *data)
{
  PaStreamParameters istream_p;
  PaStreamParameters ostream_p;
  PaStream *stream;
  PaError err;
  int i, offset = 0,  totalFrames, channels=2, dynamic=FALSE;

  data->err=OK;
  
  if ( data->M != NULL ) 
    {
      totalFrames = data->M->n;
      for( i=0; i< data->M->mn; i++ ) data->M->F[i]=0;
      channels = data->M->m;
    }
  else
    {
      dynamic = TRUE;
      if ( (data->M = nsp_matrix_create(NVOID,'r',channels, FRAMES_PER_BUFFER)) == NULLMAT ) 
	{
	  data->err= FAIL;
	  return;
	}
      for( i=0; i< data->M->mn; i++ ) data->M->F[i]=0;
    }

  if ((err = Pa_Initialize()) != paNoError ) goto error;

  if ( data->i_device== -1)
    istream_p.device = Pa_GetDefaultInputDevice();
  else
    istream_p.device = data->i_device;
    
  if (istream_p.device == paNoDevice) 
    {
      data->pa_print("Error: No default input device.\n");
      goto error;
    }

  istream_p.channelCount = channels;
  istream_p.sampleFormat = paFloat32;
  istream_p.suggestedLatency = Pa_GetDeviceInfo( istream_p.device )->defaultLowInputLatency;
  istream_p.hostApiSpecificStreamInfo = NULL;


  if ((err = Pa_IsFormatSupported(&istream_p,NULL, data->sample_rate)) != paNoError)
    {
      data->pa_print("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto error;
    }

  if ( data->o_device >= -1 ) 
    {
      if ( data->o_device == -1 ) 
	ostream_p.device = Pa_GetDefaultOutputDevice();
      else
	ostream_p.device =  data->o_device;
      ostream_p.channelCount = channels;
      ostream_p.sampleFormat = paFloat32;
      ostream_p.suggestedLatency = Pa_GetDeviceInfo( ostream_p.device )->defaultLowInputLatency;
      ostream_p.hostApiSpecificStreamInfo = NULL;
      if ((err = Pa_IsFormatSupported(NULL,&ostream_p,data->sample_rate)) != paNoError)
	{
	  data->pa_print("Error: in portaudio, %s\n", Pa_GetErrorText(err));
	  data->err=FAIL;goto error;
	}
    }
  
  err = Pa_OpenStream(&stream,
		      &istream_p,
		      (data->o_device >= -1) ? &ostream_p: NULL,
		      data->sample_rate,
		      FRAMES_PER_BUFFER,
		      paClipOff, 
		      NULL, 
		      NULL );

  if( err != paNoError ) goto error;

  if(( err = Pa_StartStream( stream )) != paNoError ) goto error;

  if ( dynamic == TRUE ) 
    {
      while (1)
	{
	  err = Pa_ReadStream( stream, data->M->F + offset , FRAMES_PER_BUFFER);
	  if( err != paNoError ) goto error;
	  offset += FRAMES_PER_BUFFER * channels;
	  if ( nsp_pa_thread_get_status() == NSP_PA_END ) 
	    {
	      Pa_AbortStream(stream);
	      break;
	    }
	  if ( data->o_device >= -1 )
	    {
	      err = Pa_WriteStream(stream, data->M->F + offset , FRAMES_PER_BUFFER);
	      if( err != paNoError ) goto error;
	    }

	  data->M->n += FRAMES_PER_BUFFER;
	  data->M->F = realloc(data->M->F, data->M->n*channels*sizeof(double));
	  if ( data->M->F == NULL) 
	    {
	      data->M->m = data->M->n =0;
	      goto error;
	    }
	  data->M->mn =  data->M->m * data->M->n;
	}
    }
  else 
    {
      while (1)
	{
	  int n = Min( FRAMES_PER_BUFFER, (data->M->n - offset));
	  err = Pa_ReadStream( stream, data->M->F + offset , n);
	  if( err != paNoError ) goto error;
	  if ( data->o_device >= -1)
	    {
	      err = Pa_WriteStream(stream, data->M->F + offset ,n);
	      if( err != paNoError ) goto error;
	    }
	  offset += n* channels;
	  if ( nsp_pa_thread_get_status() == NSP_PA_END ) 
	    {
	      Pa_AbortStream(stream);
	      break;
	    }
	  if ( offset >= data->M->n ) break;
	}
    }

  if ((err = Pa_CloseStream( stream )) != paNoError ) goto error;
  
  Pa_Terminate();
  nsp_pa_thread_set_status(NSP_PA_INACTIVE);
  return;

 error:
  Pa_Terminate();
  nsp_pa_thread_set_status(NSP_PA_INACTIVE);
  data->err=FAIL;
  data->pa_print("Error: %s\n", Pa_GetErrorText( err ));
  return  ;
}

