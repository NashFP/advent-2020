# Advent of Code 2020 - Common Lisp

These were written using SBCL (Steel Bank Common Lisp) on Linux, but
should work with other lisps. You need to make sure you have
[Quicklisp](https://www.quicklisp.org/beta/) installed.

Rather than printing out results, I usually just make functions that
return the result, usually named day*n*a and day*n*b. A quick way to
run one would be:

    sbcl --load day1.lisp

This would load the day1.lisp file in SBCL, and at the SBCL prompt
you could enter (day1a) to run the day1a function.

I typically use [Emacs SLIME](https://common-lisp.net/project/slime/)
to develop, as it gives me an interactive environment. One thing,
though, is that if you use control-c-control-k to compile the buffer
and it uses Quicklisp to load a package, the compile may fail the first
time you try to load it because the compiler doesn't invoke the
ql:quickload at the beginning of the file. Instead, the first time you
open a file with SLIME, if you do control-c-control-l to load the file
instead of compile it, it will run the ql:quickload invocations at the
beginning of the file.

I do occasionally use the Common Lisp loop macro, it's very powerful,
but not necessarily functional. I am mostly trying to make things
functional. The only time I have used setf was to store items in a
hashmap. I often make use of tail-recursive functions.

If you have questions/commands e-mail me at <mark@wutka.com> or
on the NashFP mailing list or Slack.
