-module(etftp).

-export([tftp_server_start/0,listen/1,handle_message/4]).


tftp_server_start() ->
     {ok, Socket}=gen_udp:open(69, [binary, {active,false}]),
     listen(Socket).

listen(Socket) ->
    {ok, {Address,Port,Packet}}=gen_udp:recv(Socket, 0),
    spawn(?MODULE, handle_message, [Socket, Address, Port, Packet]),
    ?MODULE:listen(Socket).


handle_message(Socket, Address, Port, Packet)->
	
  io:format("Packet: ~p~n", [Packet]),
  <<Opcode:16,Body/binary>> = Packet,
    io:format("Op: ~p~n", [Opcode]),
    case Opcode of
    	 1 -> handle_read(Socket, Address, Port, Body);
	 2 -> handle_write(Socket, Address, Port, Body);
	 3 -> handle_data(Socket, Address, Port, Body);
	 4 -> handle_ack(Socket,Address, Port,Body);
	 5 -> handle_error(Socket,Address, Port, Body)
    end.


handle_read(Socket, Address, Port, Body)->
  Rrq = parse_rq(Body),
  send_error(Socket, Address, Port, <<0:16>>, "Unsupported operation.").

handle_write(Socket, Address, Port, Body)->
  Wrq = parse_rq(Body).

handle_data(Socket, Address, Port, Body)->
 ok.

handle_ack(Socket, Address, Port, Body)->
 ok.

handle_error(Socket, Address, Port, Body)->
  send_error(Socket, Address, Port, <<0:16>>, "Unsupported operation.").

send_error(Socket, Address, Port, ErrorCode, ErrorMessage)->
   BinaryMessage = binary:list_to_bin(ErrorMessage),
   gen_udp:send(Socket, Address, Port, <<5:16,ErrorCode/binary, BinaryMessage/binary, 0>>).

parse_rq(Body)->
 {FileEnd,_} = binary:match(Body, <<0>>),
 FileName  =  binary:part(Body, 0, FileEnd),
 Mode  =  binary:part(Body, FileEnd + 1 , (byte_size(Body) - (FileEnd + 1)) - 1),
 io:format("file: ~p ~nmode: ~p~n~n", [FileName, Mode]),
 [{file, FileName}, {mode, Mode}].

 

