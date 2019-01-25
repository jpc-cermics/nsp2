port = 7777;
verbose = %f;
dont_reuse_address = %f;
non_blocking = %f;
use_udp = %f;
cancel_timeout = 0;
read_timeout = 0;
delay = 0;
unix_socket = "";
tls_cert_file = "";

function str= socket_address_to_string (address)
  // utility function: returns the inet address
  // and port as a string 
  str = "";
  if is(address, %types.GInetSocketAddress) then
    // is(address, %types.GSocketAddress)
    inet_address = address.get_address[];
    // str = g_inet_address_to_string (inet_address);
    str = inet_address.to_string[];
    port = address.get_port[];
    str = sprintf ("%s:%d", str, port);
  end
endfunction

function ensure_socket_condition (socket,condition,cancellable)
  function y= source_ready (loop)
    // quit the loop 
    loop.quit[];
    y = %f;
  endfunction
  // source = g_socket_create_source (socket, condition, cancellable);
  source = socket.create_source[condition]; // cancellable];
  loop=g_main_loop_new();
  source.set_callback['GSocket',source_ready,loop];
  source.attach[];
  printf("waiting in a gtk_loop for something to read\n")
  loop.run[];
  printf("ready to read\n");
endfunction

function ensure_stream_ok (stream) 
  // wait for the stream to be ready
  // i.e enters a gtk_main_loop until the stream
  // 
  function y= source_ready (loop)
    // quit the loop 
    loop.quit[];
    y = %f;
  endfunction
  // XXX: maybe we should check here that the stream is pollable
  source = stream.create_source[]; // XXX cancellable can be added
  loop=g_main_loop_new();
  source.set_callback['GPollable',source_ready,loop];
  source.attach[];
  printf("waiting in a gtk_loop for something to read/write\n")
  loop.run[];
  printf("ready to read/write\n");
endfunction

// WIP:
// cancellable proceed with thread and we should not use threads with
// nsp-callback functions. Thus we have to find a way to use cancellable
// with predefined callbacks at C-level, since here we only want to implement a
// timeout.

if cancel_timeout then
  cancellable = g_cancellable_new ();
  thread = g_thread_new ("cancel", cancel_thread, cancellable);
else
  cancellable = "";
end

cancellable = g_cancellable_new ();

if tls_cert_file <> "" then
  tlscert = g_tls_certificate_new_from_file (tls_cert_file);
end

// use tcp
socket_type = GIO.SOCKET_TYPE_STREAM;
// socket_family = GIO.SOCKET_FAMILY_UNIX;
socket_family = GIO.SOCKET_FAMILY_IPV4;

socket = g_socket_new (socket_family, socket_type, 0);

if non_blocking then
  // g_socket_set_blocking (socket, FALSE);
  socket.set_blocking[%f];
end

src_address = g_inet_socket_address_new (g_inet_address_new_any (GIO.SOCKET_FAMILY_IPV4), port);
socket.bind[src_address, ~dont_reuse_address];

socket.listen[];
// address = g_socket_get_local_address (socket, &error);
address = socket.get_local_address[];
  
display_addr = socket_address_to_string (address);
printf ("Listening on %s ... \n", display_addr);

ensure_socket_condition (socket, GLIB.IO_IN, cancellable);

new_socket = socket.accept[];// [cancellable= cancellable];

if non_blocking then new_socket.set_blocking[%f]; end
if read_timeout then new_socket.set_timeout[read_timeout]; end

address = new_socket.get_remote_address[];
display_addr = socket_address_to_string (address);
printf ("got a new connection from %s\n", display_addr);
    
recv_socket = new_socket;

connection =recv_socket.connection_factory_create_connection[];
istream = connection.get_input_stream[]
ostream = connection.get_output_stream[]

while %t
  // wait for somethink to read
  ensure_stream_ok (istream)
  str=istream.read[];
  if str == "" then
    printf("Error receiving from client, we stop\n");
    break;
  end
  printf("received ""%s"" from client\n",str);
  [ok,H]=execstr(sprintf('%s',str),errcatch=%t);
  if ~ok then
    str=sprintf('failed to execute %s',str);
  else
    str=catenate(sprint(H,as_read=%t,base64=%t),sep='\n');
  end
  ensure_stream_ok(ostream);
  ostream.write[sprintf('%d',length(str))];
  ensure_stream_ok (istream)
  stri=istream.read[];
  if stri <> "OK" then pause wrong;end
  ensure_stream_ok(ostream);
  ostream.write[str];
end

connection.close[];
socket.close[];
