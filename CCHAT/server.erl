-module(server).
-export([start/1, stop/1]).


start(ServerAtom) ->
    genserver:start(ServerAtom, [], fun handle/2).


stop(ServerAtom) ->
    genserver:update(ServerAtom, fun stop_channels/2),
    genserver:request(ServerAtom, stop_channels),
    genserver:stop(ServerAtom).


handle(Channels, {join, Channel, ClientPid}) ->
    case lists:member(Channel, Channels) of
        false ->
            channel:start(Channel),
            {reply, genserver:request(list_to_atom(Channel), {join, ClientPid}), [Channel | Channels]};
        true ->
            {reply, genserver:request(list_to_atom(Channel), {join, ClientPid}), Channels}
    end.


stop_channels(Channels, _) ->
    {reply, [ genserver:stop(list_to_atom(Channel)) || Channel <- Channels ], []}.
