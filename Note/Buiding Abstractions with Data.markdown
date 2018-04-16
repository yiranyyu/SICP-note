# Building Abstractions With Data

复合数据

* 形成符合数据的关键在于, 程序设计语言里应该提供了某种*黏合剂*
* 复合数据中的一个关键性思想是**闭包**的概念——也就是说, 用于组合数据对象的粘合剂不但能够用于组合基本的数据对象, 同样也可以用于符合的数据对象
* 另一关键思想是, 符合数据对象能够成为以混合与匹配方式组合程序模块的方便界面

## 数据抽象引导

### 序对

* `cons`, 为了在具体层面上实现这一数据抽象，我们所用的语言提供了一种称为**序对**的符合结构，这种结构可以通过基本过程`cons`构造出来。`cons`取两个参数，返回一个包含这两个参数作为其成分的复合数据对象。从序对构造起来的数据对象称为**表结构**数据

```Scheme
(define data (cons 1 2))

(car data);; get 1
(cdr data);; get 2
;; cons stands for "construct"
;; car and cdr derive from the addressing scheme of the original implementation of the Lisp on the IBM 704 machine.
```

### 数据意味着什么

一般而言，我们总可以将数据定义为一组适当的选择函数和构造函数，以及为使这些过程成为一套合法表示，它们就必须满足的一组特定条件。
这一观点不仅可以服务于"高层"数据对象的定义，同样也适用于低层对象。请考虑序对的概念，我们从来没有说过序对究竟是什么，只说所用的语言为序对的操作提供了三个过程`cons`,`car`,`cdr`。有关这三个操作，我们需要知道的全部东西就是，如果用con将两个对象粘接到一起，那么就可以借助于car和cdr提取出这两个对象。也就是说，这些操作满足的条件是：对任何对象x、y,如果z是(cons x y)，那么(car z)就是x，(cdr z)就是y。任何能满足上述条件的过程都可以成为实现序对的基础。我们完全能够不用任何数据结构，只使用过程就实现序对:

```Scheme
(define (cons x y)
    (define (dispatch i)
        (cond ((= i 0) x)
              ((= i 1) y)
              (else (error "Argument not 0 or 1 -- CONS" m)))
    dispatch)   ;; return a procedure

(define (car z) (z 0))
(define (cdr z) (z 1))
```

## 层次性数据和闭包性质

一般说，某种组合数据对象的操作满足闭包性质，那就是说，通过它组合起数据对象得到的结果本身还可以通过同样的操作再进行组合

### 序对的表示

```scheme
(cons 1
      (cons (2
            (cons (3
                   (cons 4 null))))))

;; same to

(list 1 2 3 4)
```

* The procedures +, *, and list take arbitrary numbers of arguments. One way to define such procedures is to use define with dotted-tail notation.

```scheme
(define (f x . z) <body>) ;; you can call f with one or more arguments

(f 40)  ;; x is 40 z is ()
(f 3 2) ;; x is 3 z is (2)
```

### 对表的映射

```Scheme
(define (map proc items)
    (if (null? items)
        null
        (cons (car items)
              (map proc (cdr items)))))
```

Scheme standardly provides a map procedure that is more general than the one described here. This more general map takes a procedure of n arguments, together with n lists, and applies the procedure to all the first elements of the lists, all the second elements of the lists, and so on, returning a list of the results. For example:

```Scheme
(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
(741 852 963)

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))
(9 12 15)
```

### 层次性结构

Scheme 提供了基本过程`pair?`，它检查其参数是否为序对.

## 符号数据

在Scheme中一个单引号的意义就是引用下一个对象

```Scheme
(define a 1)
(define b 2)

(list a b)
; (1 2)

(list 'a 'b)
; (a b)

(list 'a b)
; (a 2)
```

引号也用于复合对象:

```Scheme
(car '(a b c))
; a

(cdr '(a  b c))
; (b c)
```

* 引号只不过是一种将下一个完整表达式用`(quote <expression>)`形式包裹的单字符缩写形式
* 可以通过求值 `'()`得到空表
* 为了能够对符号做各种操作，我们还需要另一个基本过程`eq?`.这个过程以两个符号作为参数，检查它们是否为相同的符号
