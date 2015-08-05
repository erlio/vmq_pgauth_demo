%% Copyright 2015 Erlio GmbH Basel Switzerland (http://erl.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(vmq_pgauth_demo).

-behaviour(auth_on_register_hook).
-behaviour(auth_on_subscribe_hook).
-behaviour(auth_on_publish_hook).

-export([auth_on_register/5,
         auth_on_publish/6,
         auth_on_subscribe/3]).

-define(POOL, vmq_pgauth_pool).

auth_on_register({_IpAddr, _Port} = Peer, {MountPoint, ClientId} = SubscriberId, UserName, Password, CleanSession) ->
    Ret =
    vmq_pgauth_worker:equery(?POOL,
        "select count(*) from on_register where mountpoint = $1 and clientid = $2 and username=$3 and password=$4",
        [MountPoint, ClientId,
         empty_string_if_undefined(UserName),
         empty_string_if_undefined(Password)]
    ),
    case Ret of
        {ok, _, [{1}]} ->
            error_logger:info_msg("auth_on_register: ~p ~p ~p ~p ~p", [Peer, SubscriberId, UserName, Password, CleanSession]),
            ok;
        _ ->
            next
    end.

auth_on_publish(UserName, {MountPoint, ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    Ret =
    case IsRetain of
        true ->
            vmq_pgauth_worker:equery(?POOL,
                "select count(*) from on_publish,on_register where mountpoint = $1 and clientid = $2 and username=$3 and topic=$4 and qos >= $5 and maxpayloadsize >= $6 and allow_retain=TRUE",
                [MountPoint, ClientId,
                 empty_string_if_undefined(UserName),
                 flatten_topic(Topic),
                 QoS,
                 byte_size(Payload)]
            );
        false ->
            vmq_pgauth_worker:equery(?POOL,
                "select count(*) from on_publish,on_register where mountpoint = $1 and clientid = $2 and username=$3 and topic=$4 and qos >= $5 and maxpayloadsize >= $6",
                [MountPoint, ClientId,
                 empty_string_if_undefined(UserName),
                 flatten_topic(Topic),
                 QoS,
                 byte_size(Payload)]
            )
    end,
    case Ret of
        {ok, _, [{1}]} ->
            error_logger:info_msg("auth_on_publish: ~p ~p ~p ~p ~p ~p", [UserName, SubscriberId, QoS, Topic, Payload, IsRetain]),
            ok;
        _ ->
            next
    end.

auth_on_subscribe(UserName, {MountPoint, ClientId} = SubscriberId, Topics) ->
    Ret =
    vmq_pgauth_worker:equery(?POOL,
        "select topic, qos from on_subscribe,on_register where mountpoint = $1 and clientid = $2 and username=$3",
        [MountPoint, ClientId, empty_string_if_undefined(UserName)]),
    case Ret of
        {ok, _, TopicQoSList} when length(TopicQoSList) >= length(Topics) ->
            FoldRet =
            lists:foldl(fun({Topic, QoS}, true) ->
                                case lists:keyfind(list_to_binary(Topic), 1, TopicQoSList) of
                                    {_, MaxQoS} when MaxQoS >= QoS -> true;
                                    _ -> false
                                end;
                           (_, false) ->
                                false
                        end, true, Topics),
            case FoldRet of
                true ->
                    error_logger:info_msg("auth_on_subscribe: ~p ~p ~p", [UserName, SubscriberId, Topics]),
                    ok;
                false ->
                    next
            end;
        _ ->
            next
    end.

%% Internal
empty_string_if_undefined(undefined) -> "";
empty_string_if_undefined(S) -> S.
flatten_topic(T) ->
    lists:flatten(vmq_topic:unword(T)).
