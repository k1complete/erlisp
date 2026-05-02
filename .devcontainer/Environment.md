
# 開発時のgithub認証情報の設定方法

開発コンテナからGitHubへsshするときは、ssh-agentを活用する。
その方法はいくつかある。

## ssh-agentのSSH_AUTH_SOCKをコンテナにマウントして転送する

開発ホストの秘密鍵をコンテナ内にコピー(あるいはマウント)することなく、安全かつパスワードなしで認証できる。
これはsshの「ポート転送」機能を利用した方法。
しかし、Macのcolimaでは使えない。

### ホストでssh-agentを起動し、鍵を登録

まだssh-agentを起動していない場合の取り急ぎでssh-agent起動しても
いいが、本来は.loginなで起動させておいたほうがいい。

SSH_AUTH_SOCK環境変数としてあるはず。

なので、やることは実質ssh-addのみ。

```bash
## eval "(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa

```

### Dockerコンテナにエージェントのソケットを渡す
```
docker run -it --rm \
  -v $SSH_AUTH_SOCK:/ssh-agent \
  -e SSH_AUTH_SOCK=/ssh-agent \
  alpine command args
```
(unixドメインソケットファイルをマウントしてわたしている）

### つくったイメージを動かす方法

.devcontainerにいるとして、

```
docker run -it -v $SSH_AUTH_SOCK:/ssh-agent \
  -e SSH_AUTH_SOCK=/ssh-agent \
  -v ../:/workspace \
  --name my-elisp-container
  -u $(id -u -n) erlisp-app bash 
```
-vはマウントで、ssh-agentのソケットを/ssh-agentにマウントし、
親ディレクトリ（つまりプロジェクトディレクトリ）を/workspaceに
マウントしている。さらに -u でホスト側のユーザを指定している。
(ワークススペースも、ユーザもDockerfileに記述されている）


### unixsocket

じつはmac osとv間のファイル共有(VirtioFS/gRPS-FUSE)はunix socketの
転送をサポートしていない。

https://shinonono.net/amp/posts/devcontainer-docker-bitwarden-ssh-agent/

そのため、Docket Desktopではマジックパス
/run/host-services/ssh-auth.sock
を使ってデフォルトSSH agent を転送する（がDocket desktopがないとだめ).
しかしcolimaでも同様に上手くいかなかった（どうやってもsocketファイル
としてマウントできなった）。
そのため、鍵ファイルをroマウントしてssh-agentをコンテナ内で動かすことにした。

```
docker run -it -u $(id -u -n) \ 
  -v $HOME/.ssh/id_rsa:/home/$(id -u -n)/.ssh/id_rsa:ro \
  -v $HOME/.gitconfig:/home/$(id -u -n)/.gitconfig:ro \
  -v ../:/workspace --name my-elisp-dev erlisp-app bash

k-1@b0e0dc3b08a6:/workspace$ eval $(ssh-agent -s)
Agent pid 9
k-1@b0e0dc3b08a6:/workspace$ ssh-add /home/$(id -u -n)/.ssh/id_rsa 
Enter passphrase for /home/k-1/.ssh/id_rsa: 

```

---
## コンテナでの開発用ユーザについて

コンテナは各個人で使うが、同時に複数がアクセスすることはないため、固定のユーザを作成することが普通。

* 鍵ファイル id_rsaやgitconfigを置く場所としてそのユーザのホームディレクトリを使う。
* 鍵ファイルid_rsaは$HOME/.sshをイメージ側で作成しておき、コンテナ起動時にマウント(ro)する。
* .gitconfigも同様にコンテナ起動時にマウント(ro)する。

これらは、ホストユーザと同じにすることも可能だが、今回の開発ではあまり意味がない。


### Dockerfileのカスタマイズ箇所

```
diff --git a/.devcontainer/Dockerfile b/.devcontainer/Dockerfile
index a754602..31fb6ea 100644
--- a/.devcontainer/Dockerfile
+++ b/.devcontainer/Dockerfile
@@ -16,6 +16,10 @@ RUN curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 &
     chmod +x /usr/local/bin/rebar3
 
 # Create a non-root user for development (optional, adjust if needed)
-RUN useradd -m -s /bin/bash erldev
+#RUN useradd -m -s /bin/bash erldev
+ARG USERNAME=devuser
+RUN useradd -m -s /bin/bash $USERNAME && \
+     mkdir /home/$USERNAME/.ssh && \
+     chown $USERNAME /home/$USERNAME/.ssh
 
```

ユーザ作成ではARGでUSERNAMEを定義して、変更可能にしているが、デフォルトで'devuser'としている。
（どうせ、ユーザ固有情報は概ねgitconfigや鍵を使うので）


### docker imageビルド方法

単にタグを指定してカレントディレクトリのDockerfileに基づいてビルドすればいい。
必要であれば、--build-arg USERNAME=$(id -u -n)などとしてUSERAME変数にホストの現在のユーザを指定してビルド
する。その場合、イメージ自体の可搬性はない。

```
docker rmi erlisp-app
docker build -t erlisp-app ./
```

### dockerコンテナ起動方法

イメージ内に作っている開発用ユーザ(devuser)を指定して~/.ssh/id_rsa, ~/.gitconfigをroでマウントしている。
加えてプロジェクトディレクトリを/workspaceにマウントしている。

```
docker run -it -u $(id -u -n) \ 
  -v $HOME/.ssh/id_rsa:/home/devuser/.ssh/id_rsa:ro \
  -v $HOME/.gitconfig:/home/devuser/.gitconfig:ro \
  -v ../:/workspace \
  --name my-elisp-dev erlisp-app bash
```

### コンテナ内部でのssh-agentの初期化処理
```
$ eval $(ssh-agent -s)
$ ssh-add $HOME/.ssh/id_rsa
```

つまりあとは、このマウントしてssh-agentするところをスクリプトにしておけばOK。
これを.devcontainer/ssh-agent-start.shとして作っているので、
```
. ./ssh-agent-start.sh
```
とすればいい。（パスフレーズは入力する）

```
ssh -T git@github.com
```
してテストできる。このとき、.ssh/known_hostsにキーを追加していいか聞かれるのでyesとする。
（コンテナ内のものなので、イメージを起動するたびにすること）


これで、C-x C-f /docker:$ID@my-elisp-container:/workspace

でワークスペースがdiredできる。
shell-modeもdockerコンテナのなかから使える。


