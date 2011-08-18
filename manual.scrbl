#lang scribble/manual
@(require planet/scribble
          (for-label (this-package-in main)
                     racket/base))

@title{browser-evaluate: evaluate JavaScript expressions in the browser}

@section{Introduction}
This library allows you to evaluate JavaScript expressions from Racket;
the result of those expressions can then be fed back into Racket.


For example, evaluating the following:
@codeblock|{
#lang racket/base
(require (planet dyoo/browser-evaluate))
(simple-js-evaluate "alert('hello world!');")
            }|
should bring up a browser window in which an alert dialog should display.


Values can be returned back by using @racket[js-evaluate] and its implicit @litchar{$SUCC}
success function.  For example:
@codeblock|{
#lang racket/base
(require (planet dyoo/browser-evaluate))
(define result (js-evaluate #<<EOF
var f = function(x) {
    if (x == 0) { return 1; }
    else { return x * f(x-1); }
}
$SUCC(f(10));
EOF
))
            }|






@defmodule/this-package[main]

@defproc[(simple-js-evaluate [str string?]) evaluated?]{                                                       
Evaluates a JavaScript string.  Returns an @racket[evaluated] structure that records
how long the evaluation took, and on what browser the evaluation happened.
                                                        }


@defproc[(js-evaluate [str string?]) evaluated?]{
                                                 }





@defproc[(make-evaluate [program-transformer
                         (any/c output-port . -> . void)])
         (any/c -> (values string number))]{
Produces a JavaScript evaluator that cooperates with a browser.
The JavaScript-compiler is expected to write out a thunk.  When invoked,
the thunk should return a function that consumes three values, corresponding
to success, failure, and other parameters to evaluation.

For example:

@racketblock[(make-evaluate (lambda (program op)
                          (fprintf op "(function() {
                                            return function(success, fail, params) {
                                                       success('ok');
                                            }})")))]

is an evaluator that will always give back 'ok' to the caller.
}