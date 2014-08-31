riak_json_http
==============

HTTP interface for [RiakJson](https://github.com/basho-labs/riak_json)

## Unit Testing

Easiest way to link an existing Riak instance to a local ```riak_json_http``` repo under development is to 
use the included ```make_riak.sh``` script. For example:

```
./bin/make_riak.sh -m partial -r /Users/dz/riak/riak-2.0/ -j /Users/dz/code/riak_json_http/ -s
```

### Running Unit Tests

```
# Build
make

# Unit Test
make test
```

### Running Integration Tests
These assume the default localhost and ports.

```
make itest
```