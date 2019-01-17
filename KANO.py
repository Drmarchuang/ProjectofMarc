import pandas as pd
from numpy import *


def cross_table_num(better, worse):
    dic = ['我喜欢这样', '它必须是这样', '我无所谓', '我能忍受', '我讨厌这样']
    a = []
    for i in [5, 4, 3, 2, 1]:
        b = []
        for j in [5, 4, 3, 2, 1]:
            temp = sum(dot(((better == i) * 1), (worse == j) * 1))
            b.extend([temp])
        a.append(b)
    df = pd.DataFrame(data=a, columns=dic, index=dic, dtype=int8)

    index = ['我喜欢这样', '它必须是这样', '我无所谓', '我能忍受', '我讨厌这样', '合计']
    df = df.append(df.sum(), ignore_index=True)
    df['合计'] = df.sum(axis=1)
    df.index = index
    return df

def percent_convert(matrix,ppl):
    pent=matrix/ppl
    return pent


def KANO_attributes(matrix):
    dic = ['Q可疑结果', 'A魅力属性', 'O期望属性', 'R反向属性', 'I无差别属性',
           'M必备属性']
    col = ['百分比']
    Q = sum(matrix.iloc[4, 4])+sum(matrix.iloc[0,0])
    A = sum(matrix.iloc[0, 1:4])
    O = sum(matrix.iloc[0, 4])
    R = sum(matrix.iloc[1:4, 0]) + sum(matrix.iloc[4, 1:4])
    I = sum(sum(matrix.iloc[1:4, 1:4]))
    M = sum(sum(matrix.iloc[1:4, 4]))

    total = [[Q], [A], [O], [R], [I], [M]]
    df = pd.DataFrame(data=total, index=dic, columns=col, dtype=float32)
    df.sort_values(by=col, ascending=False, inplace=True)

    Better = (A + O) / (A + O + M + I)
    Worse = -1 * (M + O) / (A + O + M + I)
    return df, Better, Worse
