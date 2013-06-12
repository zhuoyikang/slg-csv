%%% ==================================================================
%%% @author zhuoyikang
%%% @doc 配置各个csv配置文件和ets表的关系.
%%% @end
%%% ==================================================================
-module(csv_map).
-export([maps/0]).

%% -include("csv_record.hrl").
%% -include("const.hrl").

-define(csv_record(Name), {Name, record_info(fields, Name)}).


%% vip经验配置,默认为0级.
-record(gd_vip_exp, {
          level,                    %% vip升级等级
          exp                       %% 升级到level需要的经验.
         }).


maps() ->
  [
   {gd_vip_exp, ?csv_record(gd_vip_exp), ["vip/vip_exp.csv"]}
  ].
