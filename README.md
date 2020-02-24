# 🧰module-packing
module-packing.scm起動時にコマンドライン引数で引き渡したディレクトリの直下に存在する公開モジュールをすべてまとめ上げて、loadするファイルの作成を行います。

# 🖥Dependency
* Gauche0.97 >=

# 👩‍💻Setup
```
$ git clone https://github.com/dko-n/module-packing
```

# 👍Usage
modulesディレクトリ内にはexport設定の記述がされたSchemeスクリプトが配置されているとします。  
その状態で、ディレクトリの名称をコマンドライン引数に指定して、module-packing.scmを実行すると、  
ディレクトリ内のすべてのSchemeスクリプトのexport設定の記述がまとめて記述されたSchemeスクリプトファイルが生成されます。
```
$ ls
>> module-packing.scm modules

$ gosh module-packing.scm --module modules

$ ls
>> module-packing.scm modules.scm modules
```
# 📝Author
おんてゃん(⋈◍＞◡＜◍)。✧♡

# 📖References
* ![Gauche ユーザリファレンス](http://practical-scheme.net/gauche/man/gauche-refj/index.html)
