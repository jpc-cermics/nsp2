/* Nsp
 * Copyright (C) 2013-2013 Jean-Philippe Chancelier Enpc/Cermics
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
 * get a file from url with libsoup 
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <libsoup/soup.h>
#include <nsp/nsp.h>
#include <nsp/interf.h>
#include <nsp/smatrix.h>

static NspSMatrix *nsp_smatrix_from_data(const char *s,int lenght)
{
  return nsp_smatrix_split_string(s, "\n", TRUE);
}


static NspSMatrix *get_url (SoupSession *session,const char *url, 
			    gboolean debug, gboolean quiet )
{
  gboolean head = FALSE;
  const char *name;
  SoupMessage *msg;
  const char *header;
  
  msg = soup_message_new (head ? "HEAD" : "GET", url);
  soup_message_set_flags (msg, SOUP_MESSAGE_NO_REDIRECT);

  soup_session_send_message (session, msg);

  name = soup_message_get_uri (msg)->path;

  if (debug || head) 
    {
      SoupMessageHeadersIter iter;
      const char *hname, *value;
      char *path = soup_uri_to_string (soup_message_get_uri (msg), TRUE);

      g_print ("%s %s HTTP/1.%d\n", msg->method, path,
	       soup_message_get_http_version (msg));
      soup_message_headers_iter_init (&iter, msg->request_headers);
      while (soup_message_headers_iter_next (&iter, &hname, &value))
	{
	  g_print ("%s: %s\r\n", hname, value);
	}
      g_print ("\n");

      g_print ("HTTP/1.%d %d %s\n",
	       soup_message_get_http_version (msg),
	       msg->status_code, msg->reason_phrase);

      soup_message_headers_iter_init (&iter, msg->response_headers);
      while (soup_message_headers_iter_next (&iter, &hname, &value))
	{
	  g_print ("%s: %s\r\n", hname, value);
	}
      g_print ("\n");
    }
  else if (msg->status_code == SOUP_STATUS_SSL_FAILED) 
    {
      /*
      GTlsCertificateFlags flags;
      if (soup_message_get_https_status (msg, NULL, &flags))
	g_print ("%s: %d %s (0x%x)\n", name, msg->status_code, msg->reason_phrase, flags);
      else
	g_print ("%s: %d %s (no handshake status)\n", name, msg->status_code, msg->reason_phrase);
      */
    } 
  else if (!quiet || SOUP_STATUS_IS_TRANSPORT_ERROR (msg->status_code))
    {
      g_print ("%s: %d %s\n", name, msg->status_code, msg->reason_phrase);
    }
  
  if (SOUP_STATUS_IS_REDIRECTION (msg->status_code)) 
    {
      header = soup_message_headers_get_one (msg->response_headers,
					     "Location");
      if (header) 
	{
	  SoupURI *uri;
	  char *uri_string;
	  if (!debug && !quiet)
	    g_print ("  -> %s\n", header);
	  uri = soup_uri_new_with_base (soup_message_get_uri (msg), header);
	  uri_string = soup_uri_to_string (uri, FALSE);
	  get_url (session, uri_string, debug,quiet);
	  g_free (uri_string);
	  soup_uri_free (uri);
	}
    } 
  else if (SOUP_STATUS_IS_SUCCESSFUL (msg->status_code)) 
    {
      NspSMatrix *S;
      S = nsp_smatrix_from_data(msg->response_body->data,
				msg->response_body->length);
      return S;
    }
  return NULL;
}

NspSMatrix *get_file(const char *url, const char *proxy, const char *ca_file,
		     gboolean quiet, gboolean debug)
{
  NspSMatrix *S;
  SoupSession *session = NULL;
  GMainLoop *loop = NULL;
  gboolean synchronous=TRUE ;
  SoupURI *proxy_uri, *parsed;
  
  parsed = soup_uri_new (url);
  if (!parsed) {
    Scierror("Error: could not parse '%s' as a URL\n", url);
    return NULL;
  }
  soup_uri_free (parsed);

  session = g_object_new (synchronous ? SOUP_TYPE_SESSION_SYNC : SOUP_TYPE_SESSION_ASYNC,
			  SOUP_SESSION_SSL_CA_FILE, ca_file,
			  SOUP_SESSION_ADD_FEATURE_BY_TYPE, SOUP_TYPE_CONTENT_DECODER,
			  SOUP_SESSION_ADD_FEATURE_BY_TYPE, SOUP_TYPE_COOKIE_JAR,
			  SOUP_SESSION_USER_AGENT, "get ",
			  SOUP_SESSION_ACCEPT_LANGUAGE_AUTO, TRUE,
			  /* SOUP_SESSION_USE_NTLM, ntlm, */
			  NULL);
  if (proxy) 
    {
      proxy_uri = soup_uri_new (proxy);
      if (!proxy_uri) 
	{
	   Scierror("Error: could not parse '%s' as URI for proxy\n",
		    proxy);
	   soup_uri_free (parsed);
	   return NULL;
	}
      g_object_set (G_OBJECT (session), 
		    SOUP_SESSION_PROXY_URI, proxy_uri,
		    NULL);
      soup_uri_free (proxy_uri);
    } 
  else
    {
      /* soup_session_add_feature_by_type (session, SOUP_TYPE_PROXY_RESOLVER_DEFAULT); */
    }
  
  if (!synchronous)
    {

      loop = g_main_loop_new (NULL, TRUE);
    }

  S = get_url (session, url, debug,quiet);

  if (!synchronous)
    {
      g_main_loop_unref (loop);
    }

  soup_session_abort (session);
  g_object_unref (session);

  return S;
}

