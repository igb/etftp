-module(etftp).

-export([tftp_server_start/0,tftp_server_start/1,listen/2,handle_message/5,get_available_files/1]).

tftp_server_start() ->
     tftp_server_start("./").

tftp_server_start(Directory) ->		     
     {ok, Socket}=gen_udp:open(69, [binary, {active,false}]),
     listen(Socket, Directory).

listen(Socket, Directory) ->
    AvailableFiles=?MODULE:get_available_files(Directory),
    {ok, {Address,Port,Packet}}=gen_udp:recv(Socket, 0),
    spawn(?MODULE, handle_message, [Socket, Address, Port, Packet, AvailableFiles]),
    ?MODULE:listen(Socket, Directory).


handle_message(Socket, Address, Port, Packet, AvailableFiles)->
	
  io:format("Packet: ~p~n", [Packet]),
  <<Opcode:16,Body/binary>> = Packet,
    io:format("Op: ~p~n", [Opcode]),
    case Opcode of
    	 1 -> handle_read(Socket, Address, Port, Body, AvailableFiles);
	 2 -> handle_write(Socket, Address, Port, Body);
	 3 -> handle_data(Socket, Address, Port, Body);
	 4 -> handle_ack(Socket,Address, Port,Body);
	 5 -> handle_error(Socket,Address, Port, Body)
    end.


handle_read(Socket, Address, Port, Body, AvailableFiles)->
  [{file, FileName},_] = parse_rq(Body),
  io:format("~p~n", [AvailableFiles]),
  File = lists:keyfind(binary:bin_to_list(FileName), 1, AvailableFiles),
  case File of 
       false -> send_error(Socket, Address, Port, <<1:16>>, "File not found.");
       _ -> send_error(Socket, Address, Port, <<0:16>>, "Unsupported operation.")
  end.

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

% lists all *regular* files in the given directory and returns a list of tuples
% of the form {FileName, FullPathOfFile} 
get_available_files(Directory)->
	{ok, FileNames}=file:list_dir(Directory),
	lists:zf(fun(FileName)-> FullPath = lists:flatten([Directory, FileName]),
					{ok, FileInfo}=file:read_file_info(FullPath),
					{file_info,_,Type,_,_,_,_,_,_,_,_,_,_,_}=FileInfo,
					case Type of 
					     regular -> {true, {FileName, FullPath}};
					     _ -> false
					 end
			end, FileNames).
				


