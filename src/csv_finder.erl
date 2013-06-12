%%% ==================================================================
%%% @author zhuoyikang
%%% @doc csv模块的查询封装
%%% @end
%%% ==================================================================
-module(csv_finder).
-compile([export_all]).

-include_lib("stdlib/include/ms_transform.hrl").

%% 返回所有的有效的Keys
keys(TableName) ->
  M = ets:fun2ms(fun(Element) -> erlang:element(2, Element) end),
  ets:select(TableName, M).

all(TableName) ->
  ets:tab2list(TableName).

%% 检查某表中key是否有效
exist(TableName, TableKey) -> ets:member(TableName, TableKey).

%% 按照属性查找记录
get(TableName, TableKey, AttrList) when is_list(AttrList) ->
  case find(TableName, TableKey) of
    {error, Reason} -> {error, Reason};
    {ok, Item} ->
      Values =
        lists:map(fun(Attr) ->
                      Idx = csv_config:attr_idx(TableName, Attr),
                      %% io:format("~p ~p ~p ~n", [Idx, Item, Attr]),
                      erlang:element(Idx, Item)
                  end, AttrList),
      {ok, Values}
  end;

%% 获取单个属性.
get(TableName, TableKey, Attr) ->
  case exist(TableName, TableKey) of
    false -> {error, key_not_exist};
    true ->
      case csv_config:attr_idx(TableName, Attr) of
        {error, Reason} -> {error, Reason};
        Idx ->
          Data = ets:lookup_element(TableName, TableKey, Idx),
          {ok, Data}
      end
  end.

%% 获取最大的index.
max_index(TableName) ->
  case ets:lookup(csv_max_index, TableName) of
    [] -> {error, key_not_exist};
    [{_, Index}] -> {ok, Index}
  end.

%% 直接查找返回记录
%% find(gd_test,TheKey) -> {ok ,Value} | {ok,ValueList}.
find(TableName, Key) ->
  case ets:lookup(TableName, Key) of
    [] -> {error, key_not_exist};
    Data ->
      case length(Data)  of
        1 -> [Value] = Data, {ok, Value};
        _  -> {ok, Data}
      end
  end.
