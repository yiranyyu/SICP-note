# Building Abstractions with Procedures

1. define
   过程定义的一般形式
    ```Scheme
    (define (<name> <formal parameters>) <body1>                                           <body2>
                                           ...
                                         <bodyN>))
    ```
   解释器将顺序求值各个body，并以最后一个表达式的值作为整个过程的值
2. cond
    条件表达式的一般性形式：
    ```Scheme
    (cond (<p1> <e1>)
          (<p2> <e2>)
          .
          .
          .
          (<pn> <en>))
    ```
    `<ei>` 可以是多个表达式，以最后一个作为其值
    `else`是一个特殊符号，可以用在`cond`的最后一个子句中`<p>`的位置，这样做时，如果该`cond`前面的所有子句全部被跳过，它就会返回最后子句中`<e>`的值。事实上所有永远都求出真值的表达式都可以用在这个`<p>`的位置上
3. if
    ```Scheme
    (if <predicate> <consequent> <alternative>)
    ```
    解释器从求值`predicate`部分开始，如果得到真就求值`consequent`并返回，否则求值`alternative`并返回。`consequent` 和 `alternative`都只能是单个表达式
4. 当解释器检查一个谓词的值时，它将`#f`解释为假，而将其他所有的值都作为真。
5. `and`，`or`是短路求值
6. lambda
    ```Scheme
    (lambda (<formal-parameters) <body>)
    ```
7. let
    ```Scheme
    (let ((<var1> <exp1>)
          (<var2> <exp2>)
             ………………
          (<varn> <expn>) )
        <body>)
    ```
    用`let`替代以`lambda`实现的另一种表达:
    ```Scheme
    ((lambda (<var1> … <varn>)
             (body)
      <exp1>
       ………
      <expn>))
    ```
    这样，解释器里就不需要为局部变量增加任何新机制。`let`只是作为其基础的`lambda`表达式的语法外衣罢了
* `let`使人能在尽可能接近其使用的地方建立局部变量约束。例如如果`x`的值是5, 下面表达式
    ```Scheme
    (+ (let ((x 3))
        (+ x (* x 10)))
        x)
    ```
    的值就是38.`let`体内的`x`是3，因此这一`let`表达式的值是33.另一方面作为最外层的`+`的第二个参数的`x`仍然是5
* 变量的值是在`let`之外计算的。在为局部变量提供值的表达式依赖于某些与局部变量同名的变量时，这一规定就起作用    了。例如，如果`x`的值是2，表达式:
    ```Scheme
    (let ((x 3)
          (y (+ x 2)))
        (* x y))
    ```
将具有值12，因为在`let`的体内,`x`将是3而`y`将是4(其值是外面的`x`加2)
