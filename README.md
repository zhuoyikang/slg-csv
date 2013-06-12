slg_csv提供csv文件到ets表的导入功能,你可以简单的指定配置关系来完成导入.

# 启动slg_csv

启动slg_csv使用以下代码:

    slg_csv:start()

关闭slg_csv使用以下代码:

    slg_csv:stop()

启动之后没有加载配置文件，你需要自定义你的data文件映射关系.

# 映射

  使用以下代码指定你的csv文件存放的路径.

    slg_csv:root("data/") %% 指定为项目路径下的`data/`文件夹

## ?csv_record

首先定义以下宏，下面会使用该宏

    -define(csv_record(Name), {Name, record_info(fields, Name)}).

# 简单映射

定义record为:

    %% vip经验配置,默认为0级.
    -record(gd_vip_exp, {
          level,                    %% vip升级等级
          exp                       %% 升级到level需要的经验.
         }).

其中对应的csv文件内容为:

    INT_level;INT_exp
    0;0
    1;500
    2;1000

配置条件为:

    add(gd_vip_exp, ?csv_record(gd_vip_exp), ["vip_exp.csv"]),

可以查看项目目录src/slg_csv.erl的test函数.

映射的record的字段需要和csv表的第一行字典一一对应。csv文件可以有其它字段，映射时会忽略；csv文件如果字段与record不够，启动时会报错 `tuple not valid`

# 字段类型

record里面不需要指定字段的类型，因为erlang是无类型的，但是csv文件里的第一行需要字典各列的类型，来帮助映射.

类型名与字段名通过下划线分割，比如：`INT_level;INT_exp`

在csv文件中不字段不区分大小写，现在支持以下类型：

* int:数字，也是默认类型，也就是不制定类型则为int
* str:字符串
* term:erlang Term.

# ets属性指定

默认ets属性为`[public, set, named_table, {keypos, 2}]`， 你也可以自己制定类型属性，比如.

    %% 指定`gd_vip_exp`为bag类型
    add({gd_vip_exp, [bag]}, ?csv_record(gd_vip_exp), ["vip_exp.csv"]),

也就是每个ets表可以通过 `atom | {atm, [attr|attr]}` 指定。


# 多ets表指定

可以在add的第一个参数指定一个列表，已达到创建多个ets表的目的，但只有第一个ets表为csv文件的导入表，其余的用户自定义为辅助表或结果表.

    add([gd_vip_exp, defined_but_not_use] , ?csv_record(gd_vip_exp), ["vip_exp.csv"]),

# 多csv文件

定义多csv文件将把多个csv文件导入一个相同的ets表:

    add(gd_vip_exp, ?csv_record(gd_vip_exp), ["vip_exp.csv", "vip_exp2.csv"]),

注意重复的id会给出错误提示，你可以选择忽略。

# inject函数.

大部分时候csv表到ets是可以直接映射过去，但是不有的时候也需要手工处理一下再导入到ets，这时候可以使用inject函数.

inject函数是一个普通函数，它接受一个record为参数，返回另一个处理之后的record，只有slg_csv会把这个record导入到ets.

比如，你可以把你的inject函数定义到一个单独的erlang模块:

    -module(csv_inject).
    -export([compile_all]).

    inject_vip_exp(Vip) ->
        io:format("vip ~p~n", [Vip]).


然后通过以下参数指定:

    add(gd_vip_exp, ?csv_record(gd_vip_exp), [ {fun csv_inject/inject_vip_exp/1, "vip_exp.csv"}]),

inject函数还可以定义一个参数，但是函数原型为:

    inject_vip_exp(Vip, P) ->
        io:format("vip ~p ~p~n", [Vip, P]),
        Vip.

    add(gd_vip_exp, ?csv_record(gd_vip_exp), [ {fun csv_inject/inject_vip_exp/2, "vip_exp.csv", 23}]),

可以slg_csv下代码:csv_inject2.erl

# 加载

    slg_csv:load()  %% 加载csv配置到ets.

# 总结

整个使用流程为:

    start(),
    root("data/"),
    add(gd_vip_exp, ?csv_record(gd_vip_exp), ["vip_exp.csv"]),
    load(),
    ok.
