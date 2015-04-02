-module(slg_csv).
-export([start/0, add/3, test/0, load/0, root/1, stop/0, load/1]).

-define(csv_record(Name), {Name, record_info(fields, Name)}).

%% vip经验配置,默认为0级.
-record(gd_vip_exp, {
          level,                    %% vip升级等级
          exp,                      %% 升级到level需要的经验.
          array
         }).

start() ->
  application:start(slg_csv).

stop() ->
  application:stop(slg_csv).

%% 添加配置项.
add(Table, Record, CsvList) ->
  ets:insert(csv_map_table, {Table, Record, CsvList}).

%% 设置配置文件的根目录
root(Path) ->
  ets:insert(csv_map_table, {root_path_of_csv_map_csv, Path}).

%% 加载配置
load() ->
  csv_proxy:reload().

load(Tags) ->
  csv_proxy:reload(Tags).

test() ->
  start(),
  root("data/"),
  add({gd_vip_exp, [public, duplicate_bag, {keypos,2}]}, ?csv_record(gd_vip_exp),
      [{fun csv_inject2:inject_vip_exp/2, "vip_exp.csv", 2}]),
  load(),
  ok.
