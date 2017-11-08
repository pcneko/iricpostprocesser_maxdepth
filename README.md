# 各格子点の最大水位を求めるプログラム
## 何が出来るの？
あいりっくの出力から各格子点の水位の最大値を抽出する。

## どうやって使うの？
### ディレクトリ構成
以下のようにする。
```zsh
% tree -L 1                                                                                                                                                                                        [4:49:04]
.
├── README.md
├── calc_result.csv
├── data/
├── input.cfg
└── main.f90
```
- main.f90        :コンパイル必要
- input.cfg       :後述
- data            :ディレクトリで入力csvファイルが格納。ファイル名は"Result_???.csv"。
- calc_result.csv :main.f90の出力結果
- README.md       :当ファイル

### input.cfgの中身
以下の通り
```sh
# file_num_min 
182
# file_num_max
360
# depth_threshold
0.1
```
- file_num_min    :入力ファイル番号の最小値
- file_num_max    :入力ファイル番号の最大値
- depth_threshold :カットする水深閾値

## 出力結果のみかた
ヘッダに必要な情報を記述した。参考にして欲しい。