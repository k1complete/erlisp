# 開発時のgithub認証情報の設定方法
開発コンテナからGitHubへsshするときは、ssh-agentを活用する。
それにより、開発ホストの秘密鍵をコンテナ内にコピー(あるいはマウント)することなく、
安全かつパスワードなしで認証できる。
これはsshの「エージェント転送」機能を利用した方法。

## ホストでssh-agentを起動し、嗅ぎを登録

まだssh-agentを起動していない場合の取り急ぎでssh-agent起動しても
いいが、本来は.loginなで起動させておいたほうがいい。

SSH_AUTH_SOCK環境変数としてあるはず。

なので、やることは実質ssh-addのみ。

```bash
## eval "(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa

```


## Dockerコンテナにエージェントのソケットを渡す
```
docker run -it --rm \
  -v $SSH_AUTH_SOCK:/ssh-agent \
  -e SSH_AUTH_SOCK=/ssh-agent \
  alpine command args
```
(unixドメインソケットファイルをマウントしてわたしている）

## dockerコンテナの手動ビルド方法

.devcontainerにいるとして、

```
docker build ./ -t my-elisp-dev

```

## つくったイメージを動かす方法

.devcontainerにいるとして、

```
docker run -it -v $SSH_AUTH_SOCK:/ssh-agent \
  -e SSH_AUTH_SOCK=/ssh-agent \
  -v ../:/workspace \
  --name my-elisp-container
  -u erldev my-elisp-dev bash 
```
-vはマウントで、ssh-agentのソケットを/ssh-agentにマウントし、
親ディレクトリ（つまりプロジェクトディレクトリ）を/workspaceに
マウントしている。さらに -u でユーザ指定している。
(ワークススペースも、ユーザもDockerfileに記述されている）

これで、C-x C-f /docker:erldev@my-elisp-container:/workspace

でワークスペースがdiredできる。
shell-modeもdockerコンテナのなかから使える。
