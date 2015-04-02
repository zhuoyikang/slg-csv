%%% ==================================================================
%%% @author zhuoyikang
%%% @doc 通过指定csv文件和ets表的关系，将自动把csv文件中的内容加载到ets表中.
%%% @end
%%% ==================================================================

-module(csv_config).

-export([start/0,     %% 启动整个csv_config模块
         load/0,      %% 加载配置到内存.
         load/1,      %% 加载特定的配置到内存.
         maps_table/1,
         attr_idx/2   %% 根据TableName + AttrList查找其下标.
        ]).

-include_lib("stdlib/include/ms_transform.hrl").

%% 安全创建ets表,先使用ets:info判断是否已存在.
safe_ets_create(AtomName) ->
  case ets:info(AtomName) of
    undefined -> ets:new(AtomName, [named_table]);
    _Other -> do_nothing
  end,
  ok.

%% 安全创建ets表,先使用ets:info判断是否已存在.
safe_ets_create(AtomName, Options) ->
  %% io:format("ets:new(~p, ~p)~n", [AtomName, Options]),
  case ets:info(AtomName) of
    undefined -> ets:new(AtomName, Options);
    _Other -> do_nothing
  end,
  ok.

%% 检查一个tuple是否存在undefined的字段.
%% 这个函数主要用于gd数据解析后产生的元组检查
%% 如果出现undefined，极有可能是解析出错或者配置有错.
check_tuple(Tuple) ->
  List = tuple_to_list(Tuple),
  Result = lists:any(fun(X)-> X==undefined end, List),
  case Result of
    false -> do_nothing;
    true  -> io:format("tuple not valid ~p~n", [Tuple])
  end,
  Result.


%% 获取到用户指定映射关系
maps_table() ->
  O = ets:match(csv_map_table, {'$1','$2','$3'}),
  O1 = [ {T, R, L} || [T, R, L] <- O ],
  O1.

maps_table(Tags) ->
  O = ets:match(csv_map_table, {'$1','$2','$3'}),
  O1 = [ {T, R, L} || [T, R, L] <- O, lists:member(erlang:element(1, T), Tags) ],
  O1.

%% 启动整个csv_config配置
start() ->
  safe_ets_create(csv_map_table, [named_table, public]),  %% 用于记录用户配置
  safe_ets_create(csv_attr_map),
  safe_ets_create(csv_addtion),    %% 辅助表，可以做检查用.
  safe_ets_create(csv_max_index),  %% 辅助表，可以做检查用.
  ok.

%% 进行加载处理
load() ->
  lists:foreach(fun gen_ets_table/1, maps_table()).

load(Tags) ->
  lists:foreach(fun gen_ets_table/1, maps_table(Tags)).

-define(csv_attr_key(TableName, AttrName), {TableName, AttrName}).

%% 根据TableName + AttrList查找其下标.
attr_idx(TableName, Attr) ->
  case ets:lookup(csv_attr_map, ?csv_attr_key(TableName, Attr)) of
    [] -> {error, not_find_attr_key};
    [{_, Idx}] -> Idx
  end.

%% 记录ets-record属性下标,从2开始,1为record名字.
gen_parse_attr(TableName, RecordAttrs) ->
  lists:foldl(
    fun(Attr, Idx) ->
        ets:insert(csv_attr_map, {?csv_attr_key(TableName, Attr), Idx}),
        Idx + 1
    end, 2, RecordAttrs).

%% 处理单行数据.
gen_parse_item(undefined, _, Item) -> Item;
gen_parse_item(Fun, undefined, Item) when Fun /= undefined -> Fun(Item);
gen_parse_item(Fun, Args, Item) -> Fun(Item, Args).

gen_parse_max_index(TableName, Idx) ->
  MaxIdx = case ets:lookup(csv_max_index, TableName) of
             [] -> Idx;
             [{_, Index}] -> max(Index, Idx)
           end,
  ets:insert(csv_max_index, {TableName , MaxIdx}).

root() ->
  case ets:lookup(csv_map_table, root_path_of_csv_map_csv) of
    [{_, P}] -> P;
    R -> io:format("not root path !!! please use slg_csv:path(xx) to specify it !!!!~n~p~n", [R])
  end.

table_type(TableName) ->
  L = ets:info(TableName),
  [{_, T}] = [{type, Type} || {type, Type} <- L],
  T.

%% 处理整个csv文件数据.
gen_parse_file({TableName, RecordBase, RecordAttrs, Fun, Args, Path})  ->
  GenPath = root() ++ Path,
  List = csv_parser:parse_config_file(GenPath, RecordBase, RecordAttrs),
  ets:delete_all_objects(csv_addtion),  %% 清空此表
  lists:foldl(fun(Item, KeyIds) ->
                  NewItem = gen_parse_item(Fun, Args, Item),
                  Id = erlang:element(2, NewItem),
                  check_tuple(NewItem),
                  %%io:format("table ~p item ~p~n", [TableName, NewItem]),
                  ets:insert(TableName, NewItem),
                  gen_parse_max_index(TableName, Id),
                  case {orddict:is_key(Id, KeyIds), table_type(TableName)} of
                    {true, set} ->
                      io:format("[csv_config] primary key:~p exists on est table(~p)~n", [Id, TableName]), KeyIds;
                    _ ->
                      orddict:store(Id, 1, KeyIds)
                  end
              end, orddict:new(), List),
  ok.

%% 解析对路径和处理函数的配置.
gen_parse_path(Path) when is_list(Path) -> {undefined, Path, undefined};
gen_parse_path({Path}) -> {undefined, Path, undefined};
gen_parse_path({Fun, Path}) -> {Fun, Path, undefined};
gen_parse_path({Fun, Path, Args}) ->  {Fun, Path, Args}.

%% 产生ets表.
gen_parse_table(TableNameList) when is_list(TableNameList) ->
  [gen_parse_table(TableName) || TableName <- TableNameList],
  lists:nth(1, TableNameList);
gen_parse_table({TableName, Options}) ->
  X = [named_table | Options],
  safe_ets_create(TableName, X),
  TableName;
gen_parse_table(TableName) ->
  Options = [public, set, named_table, {keypos, 2}],
  safe_ets_create(TableName, Options),
  TableName.

%% 根据配置产生ets表.
gen_ets_table({ITableName, {RecordName, RecordAttrs}, PathList}) ->
  gen_ets_table({ITableName, {RecordName, RecordAttrs}, PathList, undefined});
gen_ets_table({ITableName, {RecordName, RecordAttrs}, PathList, LastFun}) ->
  TableName = gen_parse_table(ITableName),
  gen_parse_attr(TableName, RecordAttrs),
  RecordBase = list_to_tuple([RecordName | [undefined || _ <- RecordAttrs]]),
  FunList = lists:map(fun gen_parse_path/1, PathList),
  CallList = [{TableName, RecordBase, RecordAttrs, Fun, Args, Path}
              || {Fun, Path, Args} <- FunList],
  lists:foreach(fun gen_parse_file/1, CallList),
  case LastFun /= undefined of
    false -> do_nothing;
    true  -> csv_inject:LastFun()
  end,
  ok.
