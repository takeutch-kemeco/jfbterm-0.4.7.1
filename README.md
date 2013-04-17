## jfbterm-0.4.7.1 ----- J Framebuffer terminal -----

jfbterm-0.4.7.1 は jfbterm-0.4.7 の個人的なフォーク版です。
公式の jfbterm-0.4.7 との政治的つながりはありません。個人的な勝手なフォークにすぎません。

jfbterm は Linux の framebuffer 上で漢字を表示するためのプログラムです。
疑似端末を使ってコンソール出力をフックし、/dev/fb0 を用いて表示します。

### 特徴
* フォントファイルとして PCF 形式のものを使用します。X で使われいる適当なフォントファイルを用意してください。
* JIS/EUC/UTF-8などに対応しています。iconv(3)を使ってSHIFT_JISにも対応できます。
* pty を使って出力をフックし、/dev/fb0 でテキスト画面をエミュレートしています。

### 公式の jfbterm-0.4.7 には無い追加部分
* 前提とする擬似端末を BSD 形式から UNIX98 形式へと変更しました。

* 長い ls -l 等の際のスクロール表示速度を著しく高速化しました。
* jfbterm --legacy として起動すると、従来の低速な表示を行います。

* 画面の回転機能を追加しました。（現状では32dppのみ）
* jfbterm --cw または -R として起動すると、画面表示を時計方向に回転します。（現状では32dppのみ）
* jfbterm --ccw または -L として起動すると、画面表示を反時計方向に回転します。（現状では32dppのみ）

* 文字を明るくしてあります。

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

