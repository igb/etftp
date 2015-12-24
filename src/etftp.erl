-module(etftp).

-export([tftp_server_start/0,handle_packet/1]).


tftp_server_start() ->
     {ok, Socket}=gen_udp:open(69, [binary, {active,false}]),
     listen(Socket).

listen(Socket) ->
    {ok, {_,_,Packet}}=gen_udp:recv(Socket, 0),
    spawn(?MODULE, handle_packet, [Packet]),
    listen(Socket).


handle_packet(Packet)->
	
  io:format("Packet: ~p~n", [Packet]),
  <<Opcode:16,Body/binary>> = Packet,
    io:format("Op: ~p~n", [Opcode]),
    case Opcode of
    	 1 -> handle_read(Body);
	 2 -> handle_write(Body);
	 3 -> handle_data(Body);
	 4 -> handle_ack(Body);
	 5 -> handle_error(Body)
    end.


handle_read(Body)->
  Rrq = parse_rq(Body).

handle_write(Body)->
  Wrq = parse_rq(Body).

handle_data(Body)->
 ok.

handle_ack(Body)->
 ok.

handle_error(Body)->
 ok.


parse_rq(Body)->
 {FileEnd,_} = binary:match(Body, <<0>>),
 FileName  =  binary:part(Body, 0, FileEnd),
 Mode  =  binary:part(Body, FileEnd + 1 , (byte_size(Body) - (FileEnd + 1)) - 1),
 io:format("file: ~p ~nmode: ~p~n~n", [FileName, Mode]),
 [{file, FileName}, {mode, Mode}].

 

