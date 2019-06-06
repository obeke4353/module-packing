module-packing
====

## Description
module-packing.scm起動時にコマンドライン引数で引き渡したディレクトリの直下に存在する公開モジュールをすべてまとめ上げて、loadするファイルの作成を行います。

## Preparing
「git clone https://github.com/dko-n/module-packing」

## Usage

```
$ ls
>> module-packing.scm modules

$ gosh module-packing.scm modules

$ ls
>> module-packing.scm modules.scm modules
```