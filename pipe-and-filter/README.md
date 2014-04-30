Pipe and filter
---------------

Erlang sample implementation of a pipe-and-filter architecture.

It features the following modules:

* `gen_pipe`, behaviour which defines the basic skeleton of a compatible pipe.
* `gen_filter`, behaviour which defines the basic skeleton of a compatible filter.
* `pipe_and_filter`, generic interface to start, use, and stop a pipe-and-filter implementation which uses the previous behaviours.
* `sample_pipe`, sample implementation of a `gen_pipe` behaviour.
* `sample_filter`, sample implementation of a `gen_filter` behaviour.

It includes a EUnit test file under the `test` folder.

