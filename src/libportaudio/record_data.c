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
  int o_device;    /* play on o_device or default device if -1 */
  int err;         /* an error occured */
  IOFun  pa_print;  /* function to be used for error report */
  int sample_rate;
  int channels;
} thread_data;

static thread_data threadData ={0};


static void record_data( thread_data *srvData);

gpointer record_data_thread(gpointer data)
{
  record_data((thread_data *) data);
  return NULL;
}


int nsp_record_data(NspMatrix **M,int seconds,int sample_rate,int channels, int device)
{
  int inc=-1;
  NspMatrix *Loc;
  *M= NULL;
  nsp_finish_pa_thread();
  /* record in a thread */
  nsp_pa_thread_set_status(NSP_PA_ACTIVE);
  /* create a matrix for storing data */
  if ( (Loc = nsp_matrix_create(NVOID,'r',channels, seconds*sample_rate)) == NULLMAT ) 
    return FAIL;
  threadData.M = Loc;
  threadData.o_device = device;
  threadData.sample_rate = sample_rate;
  threadData.channels = channels;
  threadData.pa_print = Scierror;
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
  PaStreamParameters inputParameters;
  PaStream *stream;
  PaError err;
  int i;
  int totalFrames;

  data->err=OK;
  
  totalFrames = data->M->n;

  for( i=0; i< data->M->mn; i++ ) data->M->F[i]=0;


  if ((err = Pa_Initialize()) != paNoError ) goto error;


  if ( data->o_device== -1)
    inputParameters.device = Pa_GetDefaultInputDevice();
  else
    inputParameters.device = data->o_device;
    
  if (inputParameters.device == paNoDevice) 
    {
      data->pa_print("Error: No default input device.\n");
      goto error;
    }

  inputParameters.channelCount = data->M->m;
  inputParameters.sampleFormat = paFloat32;
  inputParameters.suggestedLatency = Pa_GetDeviceInfo( inputParameters.device )->defaultLowInputLatency;
  inputParameters.hostApiSpecificStreamInfo = NULL;

  err = Pa_OpenStream(
		      &stream,
		      &inputParameters,
		      NULL, 
		      data->sample_rate,
		      FRAMES_PER_BUFFER,
		      paClipOff, 
		      NULL, 
		      NULL );

  if( err != paNoError ) goto error;

  if(( err = Pa_StartStream( stream )) != paNoError ) goto error;
  
  err = Pa_ReadStream( stream, data->M->F, totalFrames );
  if( err != paNoError ) goto error;
    
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

