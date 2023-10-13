** 関数定義における引数の渡しかた

*** &optional opt : オプション引数optとする
例
    > (defun foo (a b &optional c d)
        (format t "a=~a b=~a c=~a d=~a" a b c d))
    FOO
    > (foo 1 2 3)
    a=1 b=2 c=3 d=NILNIL
    
オプショナル引数にはデフォルト値を設定することもできる
例
    > (defun foo (a b &optional (c 99) (d c))
        (format t "a=~a b=~a c=~a d=~a" a b c d))
    FOO
    > (foo 1 2)
    a=1 b=2 c=99 d=99NIL
    

*** &rest rest : 残りをリストとして受け取る。
例

    > (defun foo (&rest values)
        (format t "a=~a" values))
    FOO
    > (foo 1 2 3 4)
    a=(1 2 3 4)NIL
    
*** &key a : キーワードリストを受け取る

例
    > (defun foo (&key a b c d)
        (format t "a=~a b=~a c=~a d=~a" a b c d))
    FOO
    > (foo :a 1 :b 2 :c 3)
    a=1 b=2 c=3 d=NILNIL

デフォルト値を設定することもできる。

例
    > (defun foo (&key a b (c a) (d c))
        (format t "a=~a b=~a c=~a d=~a" a b c d))
    FOO
    > (foo :a 1 :b 2)
    a=1 b=2 c=1 d=1NIL

例
    > (defun foo (&key ((:param-a a)) ((:param-b b) 1))
        (format t "a=~a b=~a" a b))
    FOO
    > (foo :param-a 1 :param-b 2)
    a=1 b=2 c=1 d=1NIL
    
     
オプショナル引数とキーワード引数の場合は、パラメータ名-supplied-pとい
う変数を定義しておくと、明示的に変数に値が設定されたらTとなる。


例
    > (defun foo (&key a b (c a) (d c d-suppliied-p))
        (format t "a=~a b=~a c=~a d=~a" a b c d))
    FOO
    > (foo :a 1 :b 2)
    a=1 b=2 c=1 d=1NIL

    
(defun f (&key a b c) body)
--> (defun f (keylist) body)
--> (f (maps k b c d)) 
となるので&keyは不要。
