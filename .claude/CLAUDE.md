# screenshot プロジェクト

## check の生成物の後始末

- **`R CMD check` などで作られる `*.tar.gz` は，役割が終わったら削除する**．
  結果を確認し終えたら (CRAN へ出す場合は提出が済んだら) 消してよい．
  DESCRIPTION とソースから何度でも作り直せるため，残しておく理由がない．
- 同じ理由で，`*.Rcheck/` (check の作業ディレクトリ) も確認が済んだら消す．
- 補足: `*.tar.gz` を作るのは `R CMD build` / `devtools::build()` で，
  `devtools::check()` は既定で一時ディレクトリに作るためプロジェクト直下には残らない．
  プロジェクト直下に残るのは `R CMD build` を直接実行したときが多い．
  どちらの経路でできたものでも，見つけたら消す．

## ブランチ運用

- 開発は `develop`，公開は `main`．
  区切りごとに `develop` を `main` へマージ (`--no-ff`) して push する．

## 進捗状況

### 現在の状態

- 2026-08-19 06:42
  バージョン 0.9.2 (CRAN 提出済み，提出日 2025-08-27)．
  `reset_transparent()` の追加と `save_clipboard_image()` の `reset_transparent` 引数までが実装済み．
  リポジトリの後始末として次を実施:
  - `CRAN-SUBMISSION` の 0.9.2 提出記録をコミット
  - `.claude/CLAUDE.md` (プロジェクト規約) を追跡対象に追加，
    `.claude/settings.local.json` は `.gitignore` へ
  - `NEWS.md` の空見出し `0.9.1.9000` を削除し，0.9.2 に日付を追加

### 次にやること

- 次の開発を始めるときに `NEWS.md` へ `# screenshot 0.9.2.9000` の見出しを立てる．
- CRAN 側の 0.9.2 の反映状況を確認する．
