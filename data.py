#!/usr/bin/env python
# -*- coding: utf-8 -*-

### 需要安装pyExcelerator
### http://sourceforge.net/projects/pyexcelerator/
### 解压后执行 python setup.py install
###
from pyExcelerator import *
import sys
import string

# 修改gd文件夹路径
gd_dir = "/Users/zhuoyikang/ge2/gd2/"
project_dir = "/Users/zhuoyikang/Project/galaxy-empire-server-2/data/"

if len(sys.argv) == 3 :
    gd_dir = sys.argv[1]
    project_dir = sys.argv[2]
def officer_times_parsed(data, fhandle):
    f.write("INT_OFFICER_ID;INT_TIMES;INT_PRICE\n")
    for row in data[1:]:
        csv_row = ";".join(['1', row[0], row[1]])
        f.write(csv_row+"\n")
    for row in data[1:]:
        csv_row = ";".join(['2', row[0], row[2]])
        f.write(csv_row+"\n")
    for row in data[1:]:
        csv_row = ";".join(['3', row[0], row[3]])
        f.write(csv_row+"\n")

# 导出文件配置
config = [
    # act配置
    {"path":"chapterdata_act1.xls", "export":{"KEY_Battles":"act/act_1.csv"}},
    {"path":"ChapterMonsterAct1.xls", "export":{"KEY_Monsters": "monster/monster_act1.csv"}},
    ]


for c in config:
    for sheet_name, values in parse_xls(gd_dir+c["path"], "cp1251"): # parse_xls(arg) -- default encoding
        key = sheet_name.encode("utf8", "backslashreplace")
        if not key in c["export"].keys():
            continue
        path = c["export"][key]
        print  "export ", key, "to", path
        matrix = [[]]
        for row_idx, col_idx in sorted(values.keys()):
            v = values[(row_idx, col_idx)]
            # print row_idx, col_idx, v
            if isinstance(v, unicode):
                v = v.encode("utf8", "backslashreplace")
            else:
                v = str(v)
            v = v.replace(" ", "").replace("\r", "").replace("\n", "")
            try:
                v = (int)(string.atof(v))
            except:
                v = str(v)
            v = str(v)
            last_row, last_col = len(matrix), len(matrix[-1])
            while last_row-1 < row_idx:
                matrix.extend([[]])
                last_row = len(matrix)
            while last_col < col_idx:
                matrix[-1].extend([''])
                last_col = len(matrix[-1])

            matrix[-1].extend([v])
        f = open(project_dir+path, "w")
        if c.has_key("handle"):
            c["handle"](matrix, f)
        else:
            for row in matrix:
                csv_row = ";".join(row)
                f.write(csv_row+"\n")
        f.close()


