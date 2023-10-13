変更内容
 make-symbolを呼ばなくても、よいようにする。
 具体的には、リストの先頭がベアatomの場合、
 make-symbolするような、walk関数を呼ぶ。
 
 
