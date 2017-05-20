Breadthw: breadth-first file enumerator
=======================================

Like `find(1)`, with less features :)

Breadth-first listing work well with fuzzy files finders and similar tools (see
`scripts/tmux-menu` for an example).

Why rewrite something already done so many times?
-------------------------------------------------

I wanted to try to compare a dynamically shrinking zipper tree with a simple
queue and see if it could performs with a smaller memory usage on real workloads.
That's because I have the habit of exploring the whole `$HOME` directory and
often run lows on memory.

Try with:

    ./bench_mem <DIR>

For now, ZipTree is behing, probably because of the structure overhead. It's not
that bad though! Maybe it could win with collapsing straight branches.

Also, an excuse to play with QuickCheck and criterion!

TODO
----

* option to limit depth?
