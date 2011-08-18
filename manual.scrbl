#lang scribble/manual
@(require planet/scribble
          (for-label (this-package-in main)
                     racket/base))
@author+email["Danny Yoo" "dyoo@hashcollision.org"]

@title{browser-evaluate: evaluate JavaScript expressions in the browser}



@section{Introduction}
This library allows you to send JavaScript expressions from Racket into 
a web browser.  Furthermore, the library provides hooks to send string values
from that evaluation back into Racket.



For example, evaluating the following:
@codeblock|{
#lang racket/base
(require (planet dyoo/browser-evaluate))
(simple-js-evaluate "alert('hello world!');")
            }|
should bring up a browser window in which an alert dialog should display.


For more fine-grained control over evaluation, you can use @racket[js-evaluate], which
allows string values to be returned back from JavaScript back to Racket.
The JavaScript
code should call the @litchar{$SUCC} success function to send the value back
into Racket.
For example:
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
When this program executes, @racket[result] will be bound to an @racket[evaluated]
stucture whose @racket[evaluate-value] will be @racket[3628800].





@section{API}
@defmodule/this-package[main]

The return value for the evaluation functions in this libarary
are instances of the @racket[evaluated] structure.
@defstruct[evaluated ([stdout string]
                      [value string?]
                      [t number]
                      [browser string?])]{
@racket[value] represents the value returned by a use of @litchar{$SUCC}.
@racket[t] represents the amount of time in milliseconds that evaluation took.
@racket[browser] represents the browser string.

The @racket[stdout] field is currently underdocumented.
    }


If the JavaScript code signals an error (with a use of @litchar{$FAIL}), then
an @racket[error-happened] structure will be raised to the Racket caller.
@defstruct[(error-happened exn:fail) ([message string?]
                                      [continuation-marks continuation-mark-set?]
                                      [t number?])]{}


                                                   

                                                   

@defproc[(simple-js-evaluate [str string?]) evaluated?]{                                                       
Evaluates a JavaScript string.  Returns an @racket[evaluated] structure that records
how long the evaluation took, and on what browser the evaluation happened.
                                                        }


Each of the evaluation functions run in a isolated lexically scoped context.  For example:
@codeblock|{
#lang racket/base
(require (planet dyoo/browser-evaluate))
(simple-js-evaluate "var x = 3;")
(simple-js-evaluate "var x = x + 1;")
(simple-js-evaluate "alert(x);")
}|
should raise a error, since @racket[x] will not be bound on the third use of
@racket[simple-js-evaluate].



@defproc[(js-evaluate [str string?]) evaluated?]{
Evaluates a JavaScript string, where the user is responsible for calling
@litchar{$SUCC}.  The call to @racket[js-evaluate] blocks until the JavaScript
function calls @litchar{$SUCC} with a value.

For example:
@codeblock|{
#lang racket/base
(require (planet dyoo/browser-evaluate))
(js-evaluate "$SUCC(3 * 4);")
}|
will return an @racket[evaluated] structure whose @racket[evaluated-value]
will be the string @racket["12"].


In order to signal an exception back to Racket, use the implicit @litchar{$FAIL}
function.  For example.
@codeblock|{
#lang racket/base
(require (planet dyoo/browser-evaluate))
(js-evaluate "$FAIL('oh no!');")           
            }|
will raise an @racket[error-happened] whose @racket[exn-message] field
will be the string @racket["oh no!"].
}





@defproc[(make-evaluate [program-transformer
                         (any/c output-port . -> . void)])
         (any/c -> (values string number))]{
(Low-level function that's used to implement @racket[simple-js-evaluate]
and @racket[js-evaluate].)

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