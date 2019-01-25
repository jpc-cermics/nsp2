socket_type = GIO.SOCKET_TYPE_STREAM;
socket_family = GIO.SOCKET_FAMILY_IPV4;
socket= g_socket_new (socket_family, socket_type, 0);

function str= socket_address_to_string (address)
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
  pause 
  function y= source_ready (loop)
    // quit the loop 
    loop.quit[];
    y = %f;
  endfunction
  // source = g_socket_create_source (socket, condition, cancellable);
  source = socket.create_source[condition]; // cancellable];
  loop=g_main_loop_new();
  source.set_callback[source_ready,loop];
  source.attach[];
  printf("waiting in a gtk_loop for something to read\n")
  loop.run[];
  printf("ready to read\n");
endfunction

read_timeout = -1;
if read_timeout >= 0 then 
  // g_socket_set_timeout (*socket, read_timeout);
  socket.set_timeout[read_timeout];
end

cancel_timeout = 0;

if cancel_timeout>=0 then
  cancellable = g_cancellable_new ();
  // thread = g_thread_new ("cancel", cancel_thread, cancellable);
else
  cancellable = NULL;// XXXXX
end

cancellable = g_cancellable_new ();


unix_socket = %f;

if unix_socket then
      addr = socket.address_from_string[argument];
      //connectable = G_SOCKET_CONNECTABLE (addr);
else
  host = "127.0.0.1";
  port = 7777;
  connectable = g_network_address_parse (host,port);
end

function [rep,address]=check_connection(connectable)
  enumerator = connectable.enumerate[];
  while %t
    // *address = g_socket_address_enumerator_next (enumerator, cancellable, error);
    address = enumerator.next[]; // cancellable];
    if type(address,'short')=='none' then
      // "No more addresses to try" when no more address
      error("No more addresses to try");return;
    end
    // g_socket_connect (*socket, *address, cancellable, &err))
    // ensure_socket_condition (socket,GLIB.IO_IN,cancellable)
    ok = execstr('sc=socket.connect[address, cancellable=cancellable]',errcatch=%t);
    if ~ok then rep=0; return; end
    if sc then rep=1; return;end 
    printf("Connection to %s failed: trying next\n", address.to_string[]);
  end
endfunction

// we loop until a server answers 
while %t
  printf("trying to connect to server: ");
  [rep,address]=check_connection(connectable);
  if rep == 1 then break;end
  printf("failed\n");
  xpause(500000);
end

str= socket_address_to_string (address)
printf("Connected to %s\n", str);

// faire directement une inet_socket_address
// other_address = g_inet_socket_address_new_from_string("127.0.0.0",89600)
// src_address = g_socket_get_local_address (*socket, error);
// connection = G_IO_STREAM (g_socket_connection_factory_create_connection (*socket));

connection = socket.connection_factory_create_connection[];

istream = connection.get_input_stream[];
ostream = connection.get_output_stream[];

pause before sending 

ostream.write["pipo"];
printf ("closing socket\n");

connection.close[]; // cancellable]
socket.close[];

