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
#include <sndfile.h>
#include <portaudio.h>
#include <nsp/interf.h>

#define FRAMES_PER_BUFFER   (1024)

/* transmited data to callback */

typedef struct {
  NspMatrix* M;    /* matrix to be played nchannels x samples */
  int position;
} cb_data;

/* transmited data to the thread */

typedef struct {
  NspMatrix* M;    /* matrix to be played nchannels x samples */
  double duration; /* play duration seconds */
  int channel ;    /* play channel i or all if channel == -1 */
  int o_device;    /* play on o_device or default device if -1 */
  int err;         /* an error occured */
} thread_data;

static thread_data srvData ={0};
static int active = 0;

static void play_data_nocb( thread_data *srvData);
static void play_data_nocb_stop(thread_data *data);
static void play_data_nocb_more(thread_data *data);


int nsp_play_data_nocb(NspMatrix *M, int flag)
{
  if ( active == 1 && flag == 0 ) 
    {
      /* finish active thread */
      active = 0;
      while ( active == 0 ) {};
      /* now the child have fixed active to 2 */
    }
  /* play */
  active = 1;
  srvData.M = M;
  srvData.o_device = 0;
  switch ( flag ) 
    {
    case 0: 
      play_data_nocb(&srvData);break;
    case 1: 
      play_data_nocb_more(&srvData);break;
    case 2:
      play_data_nocb_stop(&srvData);break;
    }
  return OK;
}

/* play without call back.
 *
 */

static PaStream *ostream; 

static void play_data_nocb(thread_data *data)
{
  int max, offset=0;
  PaStreamParameters ostream_p;
  PaError err;   
  float buffer_stereo[FRAMES_PER_BUFFER][2]; /* stereo output buffer */
  float buffer_mono[FRAMES_PER_BUFFER][1]; /* mono output buffer */
  
  data->err=OK;

  if ((err = Pa_Initialize()) != paNoError)
    {
      Scierror("Error: in portaudio, %s\n",Pa_GetErrorText(err));
      data->err=FAIL; goto end;
    }
  ostream_p.device = data->o_device;
  if ( ostream_p.device == -1 ) 
    ostream_p.device = Pa_GetDefaultOutputDevice();

  if ( ostream_p.device == paNoDevice ) 
    {
      Scierror("Error: in portaudio, output device %d not found \n",
	       data->o_device);
      data->err=FAIL;goto end;
  }
  
  max = Pa_GetDeviceInfo(ostream_p.device)->maxOutputChannels;

  if ( data->M->m > 2 )
    {
      Scierror("Error: in portaudio here only stereo or mono sounds (%d>2) \n", 
	       data->M->m);
      data->err=FAIL;goto end;
    }

  ostream_p.channelCount = data->M->m;
  ostream_p.sampleFormat = paFloat32;
  ostream_p.suggestedLatency = Pa_GetDeviceInfo(ostream_p.device )->defaultLowOutputLatency;
  ostream_p.hostApiSpecificStreamInfo = 0;

  if ((err = Pa_IsFormatSupported(NULL, &ostream_p, 44100)) != paNoError)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto end;
    }
  
  err = Pa_OpenStream(&ostream,
		      NULL,
		      &ostream_p,
		      44100,
		      FRAMES_PER_BUFFER, /* frames per buffer */
		      paNoFlag,
		      NULL,
		      NULL);
  
  if (err)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      goto end;
    }

  if ((err = Pa_StartStream(ostream))!= paNoError)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto end;
    }
  
  /* -- Here's the loop where we pass data from input to output -- */

  while (1) 
    {
      int i,j, n;
      n = Min( FRAMES_PER_BUFFER, (data->M->n - offset));
      if ( data->M->m == 2) 
	{
	  for( i=0; i < n ; i++ )
	    {
	      for ( j = 0 ; j < data->M->m ; j++) 
		buffer_stereo[i][j] = data->M->R[j+ data->M->m*(i+offset)];
	    }
	  for (  ; i < FRAMES_PER_BUFFER; i++)
	    {
	      for ( j = 0 ; j < data->M->m ; j++) 
		buffer_stereo[i][j] = buffer_stereo[i][j] = 0;
	    }
	  offset += n;
	  err = Pa_WriteStream( ostream, buffer_stereo, FRAMES_PER_BUFFER );
	}
      else 
	{
	  for( i=0; i < n ; i++ )
	    {
	      for ( j = 0 ; j < data->M->m ; j++) 
		buffer_mono[i][j] = data->M->R[j+ data->M->m*(i+offset)];
	    }
	  for (  ; i < FRAMES_PER_BUFFER; i++)
	    {
	      for ( j = 0 ; j < data->M->m ; j++) 
		buffer_mono[i][j] = buffer_mono[i][j] = 0;
	    }
	  offset += n;
	  err = Pa_WriteStream( ostream, buffer_mono, FRAMES_PER_BUFFER );
	}
      if( err != paNoError ) goto end;
      if ( offset >= data->M->n ) break;
    }
  
  /* -- Now we stop the stream -- */
  err = Pa_StopStream( ostream );
  if( err != paNoError ) goto end;
  
  if ((err = Pa_CloseStream(ostream)) != paNoError)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto end;
    }
		
 end :
  Pa_Terminate();
  active = 2;
}


static void play_data_nocb_more(thread_data *data)
{
  PaError err;   
  float buffer_stereo[FRAMES_PER_BUFFER][2]; /* stereo output buffer */
  float buffer_mono[FRAMES_PER_BUFFER][1]; /* mono output buffer */
  int offset = 0;
  while (1) 
    {
      int i,j, n;
      n = Min( FRAMES_PER_BUFFER, (data->M->n - offset));
      if ( data->M->m == 2) 
	{
	  for( i=0; i < n ; i++ )
	    {
	      for ( j = 0 ; j < data->M->m ; j++) 
		buffer_stereo[i][j] = data->M->R[j+ data->M->m*(i+offset)];
	    }
	  for (  ; i < FRAMES_PER_BUFFER; i++)
	    {
	      for ( j = 0 ; j < data->M->m ; j++) 
		buffer_stereo[i][j] = buffer_stereo[i][j] = 0;
	    }
	  offset += n;
	  err = Pa_WriteStream( ostream, buffer_stereo, FRAMES_PER_BUFFER );
	}
      else 
	{
	  for( i=0; i < n ; i++ )
	    {
	      for ( j = 0 ; j < data->M->m ; j++) 
		buffer_mono[i][j] = data->M->R[j+ data->M->m*(i+offset)];
	    }
	  for (  ; i < FRAMES_PER_BUFFER; i++)
	    {
	      for ( j = 0 ; j < data->M->m ; j++) 
		buffer_mono[i][j] = buffer_mono[i][j] = 0;
	    }
	  offset += n;
	  err = Pa_WriteStream( ostream, buffer_mono, FRAMES_PER_BUFFER );
	}
      if( err != paNoError ) goto end;
      if ( offset >= data->M->n ) break;
    }
  return;

 end :
  Pa_Terminate();
  active = 2;
}


static void play_data_nocb_stop(thread_data *data)
{
  PaError err;   
  /* -- Now we stop the stream -- */
  err = Pa_StopStream( ostream );
  if( err != paNoError ) goto end;
  
  if ((err = Pa_CloseStream(ostream)) != paNoError)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      data->err=FAIL;goto end;
    }
		
 end :
  Pa_Terminate();
  active = 2;
}
