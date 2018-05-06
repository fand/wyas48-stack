# メモ

- stack.yamlで `resolver: lts-9.21` にしとく
- `dist/` があるとghc-modが動かないので消しとく
- > 最後にアンダースコアの付いた名前は、Haskellにおける一般的な命名規約に沿ったもので、繰り返すが値を返さないモナド関数を表します
  - むずい……
- Control.Monad.Error 使ってるとこに `Use Control.Monad.Except instead` とメッセージ出るけど、こういうことっぽい
  - http://koba-e964.hatenablog.com/entry/2014/06/17/004011
- Hlintの声に従って変換していくとドンドン読みにくくなる気がする
- maybe関数の引数の順序難しい
  - maybe 失敗した場合に返す値 成功した場合に値を適用させる関数 Maybeモナド
