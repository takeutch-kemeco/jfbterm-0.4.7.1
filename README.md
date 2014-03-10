## jfbterm-0.4.7.1 ----- J Framebuffer terminal -----

* jfbterm-0.4.7.1 は jfbterm-0.4.7 の個人的なフォーク版です。  
公式の jfbterm-0.4.7 との政治的つながりはありません。個人的な勝手なフォークにすぎません。

* jfbterm は Linux の framebuffer 上で漢字を表示するためのプログラムです。  
疑似端末を使ってコンソール出力をフックし、/dev/fb0 を用いて表示します。

* ライセンスはオリジナル同様に2条項BSDライセンスです。  
(詳しくは COPYING を参照してください)

### 注意
* commit f390323851f18f5d2d9a0d8378635e10ed443f84 以降では、一部をHaskellコードへ置き換えています。  
そのためビルドには gcc の他に ghc(version 7.6.3以降) も必要となります。  
また、Haskellライブラリーとして cabal によって適切にインストールされた unix-2.7.0.0, stm-2.4.2, rdtsc-1.3.0.0 ライブラリーが必要です。
* Haskellへの置き換え以前の最後のコミット位置は commit 6392e5934275b48e90880c4c019f6e159de50e51 です。

### 特徴
* フォントファイルとして PCF 形式のものを使用します。X で使われいる適当なフォントファイルを用意してください。
* JIS/EUC/UTF-8などに対応しています。iconv(3)を使ってSHIFT_JISにも対応できます。
* pty を使って出力をフックし、/dev/fb0 でテキスト画面をエミュレートしています。

### 公式の jfbterm-0.4.7 には無い追加部分
* 前提とする擬似端末を BSD 形式から UNIX98 形式へと変更しました。
* 長い ls -l 等の際のスクロール表示速度を著しく高速化しました。  
jfbterm --legacy として起動すると、従来の低速な表示を行います。
* 画面の回転機能を追加しました。（現状では32dppのみ）  
jfbterm --cw または -R として起動すると、画面表示を時計方向に回転します。（現状では32dppのみ）  
jfbterm --ccw または -L として起動すると、画面表示を反時計方向に回転します。（現状では32dppのみ）
* 文字を明るくしてあります。

### 公式の jfbterm-0.4.7 よりも劣化した部分
* フォントセットとして使用可能な文字コードは UTF-8 のみです。  
UTF-8 以外の文字コード(EUC-JP, ISO-2022-JP 等)への対応は全て廃止しました。

### 現状のバグ
* jfbterm 内で jfbterm を起動する場合は、--legacy オプションを付けて起動しなければ画面表示が固まります。  
(ただし画面表示以外は生きているので、 Alt+Ctrl+F? などは効く状態です)
* 公式版の jfbterm-0.4.7 から続く問題点（≠バグ）として、ncursesを用いた画面表示が崩れる場合があります。  
（Linuxカーネルの make menuconfig 等で問題となります）

### プログラムについて
JFBTERM を使ってハードウェアや他のソフトウェアに問題が発生しても、作者は責任をとることはできません。

バグや意見などがありましたら下記までご連絡ください。

		https://github.com/takeutch-kemeco/jfbterm-0.4.7.1
		E-Mail: takeutchkemeco@gmail.com

また、公式版への連絡先はこちらです。
（このフォークしたバージョン固有のバグ報告の場合は、迷惑になるので送らない方がよいと思います）

		http://sourceforge.jp/projects/jfbterm
		E-Mail: jfbterm-dev@linux.or.jp
		Current Maintainer: ukai@debian.or.jp
		Original Author: nmasu@ma3.justnet.ne.jp.

