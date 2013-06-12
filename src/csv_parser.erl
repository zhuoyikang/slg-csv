%%% ==================================================================
%%% @author zhuoyikang
%%% @doc 将csv文件中的数据解析为record记录
%%% @end
%%% ==================================================================
-module(csv_parser).
-compile([export_all]).


%%% -------------------------------------------------------------------
%%% 字符串处理操作
%%% -------------------------------------------------------------------
string_to_term(String) ->
  %% io:format("~p ~n", [String]),
  TmpString1 = string:strip(String, both, $.),
  TmpString = string:strip(TmpString1, both, $"),
  NewString = TmpString ++ ".",
  %% io:format("new ~p~n", [NewString]),
  {ok, Tokens,_EndLine} = erl_scan:string(NewString),
  {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
  {value, Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
  Value.


%% 执行解析操作.
parse_to_line_array(Path) ->
  Binary = case file:read_file(Path) of
             {ok, Data} ->  Data ;
             {error, Error} ->
               io:format("error ~p path ~p~n", [Error, Path]),
               <<>>
           end,
  String = binary_to_list(Binary),
  Array = parse_to_line_array(String, ["\r", "\n"]),
  List = lists:foldl(fun(Item, Result) ->
                         List = parse_line_to_list(Item, [";", ","]),
                         case length(List) of
                           0 -> Result;
                           _Length -> [List|Result]
                         end
                     end, [], Array),
  lists:reverse(List).

%% 将csv文件解析按行解析为数据记录.
parse_to_line_array(String, []) ->
  io:format("attention!!! parse line error !!!!!!!!!! ~p~n", [String]), [];
parse_to_line_array(String, [H|T]) ->
  Array = string:tokens(String, H),
  case length(Array) of
    1 -> parse_to_line_array(String, T);
    LineNo when LineNo > 1 -> Array
  end.

%%% -------------------------------------------------------------------
%%% 将csv行解析成为按csv分割符分割的数组.
%%% -------------------------------------------------------------------

parse_line_to_list(String, []) ->
  io:format("attention!!! parse line to tuple error !!! ~p~n", [String]), [];
parse_line_to_list(String, [H|T]) ->
  Array = string:tokens(String, H),
  case length(Array) of
    1 -> parse_line_to_list(String, T);
    LineNo when LineNo > 1 -> Array
  end.

%% 将csv字符串转换为数字
parse_string_to_integer(String) ->
  S = string:strip(String),
  try list_to_integer(S) of
      Value -> Value
  catch error:Error ->
      io:format("string_to_integer ~p error ~p~n", [String ,Error]), 0
  end.

%% 按record的@Title分析@data数据，更新@Database
%% 遍历csv文件中的title,将一行data数据填充到DataBase
parse_line_to_record(DataBase, TitleList, Data, Fields) ->
  lists:foldl(
    fun({FieldType, FieldName, Idx}, Record) ->
        case (length(Data) >= Idx) of
          false -> Record;
          true ->
            Value = lists:nth(Idx, Data),
            case FieldType of
              table -> set_field_val(Record, Fields, FieldName, Value);
              str -> set_field_val(Record, Fields, FieldName, Value);
              int -> NewValue = parse_string_to_integer(Value),
                     set_field_val(Record, Fields, FieldName, NewValue);
              term -> NewValue = string_to_term(Value),
                      set_field_val(Record, Fields, FieldName, NewValue);
              Other -> io:format("wrong type ~p~n", [Other]), Record
            end
        end
    end, DataBase, TitleList).

%%% -------------------------------------------------------------------
%%% 解析title
%%% -------------------------------------------------------------------

%% 解析单个title.
parse_record_title(CsvTitle, {Idx, RecordTitleList}) ->
  CsvTitleLower = string:to_lower(CsvTitle),
  CsvTitleSegmentArray = string:tokens(CsvTitleLower, "_"),
  [CsvTitleType | CsvTitleLeft] = CsvTitleSegmentArray,
  TitleName = list_to_atom(string:join(CsvTitleLeft, "_")),
  RecordTitle = case CsvTitleType of
                  "str" -> {list_to_atom(CsvTitleType), TitleName, Idx};
                  "table" -> {list_to_atom(CsvTitleType), TitleName, Idx};
                  "int" -> {list_to_atom(CsvTitleType), TitleName, Idx};
                  "term" -> {list_to_atom(CsvTitleType), TitleName, Idx};
                  _ -> {int, list_to_atom(CsvTitleLower), Idx}
                end,
  {Idx+1, [RecordTitle | RecordTitleList]}.

%% 解析整csv配置文件.
parse_config_file(CsvPath, RecordBase, RecordFields) ->
  case parse_to_line_array(CsvPath) of
    [] -> io:format("bad csv config file ~p fields ~p ~n",
                    [CsvPath, RecordFields]), error;
    [CsvTitle | CsvRecords] ->
      {_, TitleList} = lists:foldl(fun parse_record_title/2, {1, []}, CsvTitle),
      lists:foldl(fun(CsvRecord, RecordList) ->
                      case length(CsvRecord) > 0  of
                        true ->
                          Record = parse_line_to_record(RecordBase, TitleList,
                                                        CsvRecord, RecordFields),
                          [Record | RecordList];
                        false ->
                          io:format("wrong data ~p on ~p~n", [CsvRecord, CsvPath])
                      end
                  end, [], CsvRecords)
  end.

%%% -------------------------------------------------------------------
%%% 给record动态赋值的子函数
%%% -------------------------------------------------------------------

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

set_field_val(R, Fields, Field, Value) ->
  case index_of(Field, Fields) of
    not_found -> R;
    Idx -> erlang:setelement(Idx + 1, R, Value)
  end.
