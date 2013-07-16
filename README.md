#F2 -functional language 2-

関数型言語F2

##これは何？

ちょっとした思いつきと勉強のために作り始めた、関数型のプログラミング言語ですー＞ω＜
型推論をそなえていますです(｀・ω・´)

（ただし、あまり過剰な期待はしないほうがいいのです(　ﾟ,＿･･ﾟ)）

##インストール

Cabalを使ってのインストールに対応しましたですー＞ω＜

```
$ git clone git@github.com:kagamilove0707/F2.git && cd F2
$ cabal configure && cabal build
```

これで、依存関係に足りないものがあっても勝手にインストールされるはずですー＞ω＜

##実行

MLが七割、Haskellが三割入ったような変な仕様となっていますですー＞ω＜

現在は、残念ながらREPLしかないのです(｀・ω・´)  
というわけで、`./f2`を叩いてみるのです(｀・ω・´）

```
$ ./f2
hello. F2 v0.1.1.2 (2013/07/17)

(1)#
```

まずは`1 + 1`でも。

```
(1)# 1 + 1
  it = 1 + 1 = 2 : Int
```

`1 + 1`だけのわりに、たくさんの出力がされました(； ･\`д･´)　これは`変数名 = 入力した式 = 結果 : 型`となっていますです＞ω＜

次に関数の定義でも。`fun 引数 -> 本体`としますです＞ω＜

```
(2)# fun x y -> x + y
  it = fun x y -> x + y = <fun> : Int -> Int -> Int
(3)# (fun x y -> x + y) 1 2
  it = (func x y -> x + y) 1 2 = 3 : Int
```

多相型も使えるのです＞ω＜

```
(4)# fun x -> x
  it = fun x -> x = <fun> : 't0 -> 't0
(5)# (fun x -> x) 10
  it = (fun x -> x) 10 = 10 : Int
(6)# (fun x -> x) True
  it = (fun x -> x) True = True : Bool
```

分かってると思いますがこの`True`というのはブール代数の真のことです(｀・ω・´)

`def 名前 = 式`で変数を定義できますです＞ω＜　（変更できませんけど(´･ω･｀)）  
↑下の出力を見れば一目瞭然ですけれど、デフォルトは`it`という変数に代入されていて参照できますです(｀・ω・´)
ちなみに、演算子も`def (記号) = 式`や`let (記号) = 式`で定義できますです(｀・ω・´)　（しかし、左結合で結合順位は一定です(　ﾟ,_･･ﾟ)）
他にはlet式（`let 名前 = 式1 in 式2`）で束縛したり、if式（`if 条件 then 式1 else 式2`）で分岐したり、カリー化したり、型シグネチャを指定したりできますです＞ω＜

```
(7)# def const = fun x y -> x {- コメントはHaskell風ですー＞ω＜ -}
  const = fun x y -> x = <fun> : 't0 -> t1' -> t0'
(8)# let f = const 10 in f 20
  it = let f = const 10 in 20 = 10 : Int
(9)# let x = 10 in if x == 20 then 1 else 2
  it = let x = 10 in if x = 20 then 1 else 2 = 2 : Int
(10)# def (&&) = fun x y -> if x then y else False
  && = fun x y -> if x then y else False = <fun> : Bool -> Bool -> Bool
(11)# (fun x -> x : Int -> Int) True
  type error : no match
```

最後に、終了は`:q`で行えますです＞ω＜

```
(12)# :q

  See you!
```

##組み込み関数一覧

デフォルトで定義されている関数です(｀・ω・´)　現在はこれしかありません(´･ω･`)

  * `id : 'a -> 'a`
  * `(+) : Int -> Int -> Int`
  * `(==) : Int -> Int -> Bool`

`(==)`が`Int`にしか適用できないのは、型クラスを持たないせいです(´･ω･`)

##とぅどぅー

今後やっていきたいことです＞＜

  * 型を増やす。
  * 関数を増やす。
  * 再帰関数を実装する。
  * 代数的データ型を実装する。
  * パターンマッチを実装する。
  * 関数定義の糖衣構文（`let f x = x`のような）を実装する。
  * 型クラスを実装する。
  * エラーをもっと親切にする。
  * 仮想機械を作り、そこで実行するようにする。

他にもあるような気がしますが、今はこれぐらいにしておきますです＞＜

##ライセンス

MITライセンスにしましたです＞ω＜

##こんとりびゅーと

Fork & Pull Request大歓迎です＞ω＜　ここはこうしたほうがいいとか、こんな仕様があったら面白いとか。  
待ってますです＞ω＜

