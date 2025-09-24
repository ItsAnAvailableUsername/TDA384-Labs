-module(channel).
-export([start/1]).

-record(channel_st, {
          name,
          client_pids
         }).


start(Channel) ->
    genserver:start(list_to_atom(Channel), #channel_st{name = Channel, client_pids = []}, fun handle/2).


handle(St, {join, ClientPid}) ->
    ClientPids = St#channel_st.client_pids,
    case lists:member(ClientPid, ClientPids) of
        false -> {reply, ok, St#channel_st{client_pids = [ClientPid | ClientPids]}};
        true -> {reply, user_already_joined, St}
    end;

handle(St, {leave, ClientPid}) ->
    ClientPids = St#channel_st.client_pids,
    case lists:member(ClientPid, ClientPids) of
        false -> {reply, user_not_joined, St};
        true -> {reply, ok, St#channel_st{client_pids = lists:delete(ClientPid, ClientPids)}}
    end;

handle(St, {message_send, Nick, Msg, ClientPid}) ->
    ClientPids = St#channel_st.client_pids,
    case lists:member(ClientPid, ClientPids) of
        false -> {reply, user_not_joined, St};
        true ->
            [ genserver:request(ClientPidPrime, {message_receive, St#channel_st.name, Nick, Msg}) || ClientPidPrime <- lists:delete(ClientPid, ClientPids) ],
            {reply, ok, St}
    end.
