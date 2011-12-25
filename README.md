# ESpec

Behaviour driven development for Erlang.

## Overview

ESpec is a behaviour driven development framework for Erlang. If you've ever used RSpec you'll feel at home, as this is basically the same.

It provides a structure for writing executable examples for how your code should behave. It uses the words "describe" and "it" so we can express concepts like a conversation:

    "Describe an order."
    "It sums the prices of its line items."

## Basic Structure

    describe("#generate_headers", fun() ->
        it("should generate Host and User-Agent headers", fun() ->
            [
                {"Host", "api.twitter.com"},
                {"User-Agent", "Twerl"}
            ] = stream_client_util:generate_headers()
        end)
    end).

The `describe` function is used to express a entity, for example a module or function, in your system whose behaviour you want to define. The body of this contain `it` functions which are used to express the behaviour.

## Pending tests

`it` functions do not need a body, in this case they are called 'pending'. This means you can write out the high level behaviour, and later define it in more detail.

    describe("#connect", fun() ->
        it("should return http errors"),

        it("should use http post method"),

        it("should use the correct url")
    end).

## Nesting

`describe` functions can be nested, to help you better isolate tests of different parts of the system.

    describe("http client", fun() ->
        describe("when connected", fun() ->
            it("should pass data to the callback"),

            it("should return ok and the pid when the stream terminates")
        end),

        describe("when not connected", fun() ->
            it("should return an error when trying to make a request")
        end)
    end).

## Filters

When describing different conditions for similar functionality of an entity you will find that there is a lot of repeated setup and tear down code, for example mocking. You can use filters which let you repeat code for each example:

    describe("connection handler", fun() ->
        before_each(fun() ->
            catch meck:new(httpc, [unstick]),
            catch meck:new(stream_client, [passthrough])
        end),

        after_each(fun() ->
            ?assertEqual(true, meck:validate(httpc)),
            ?assertEqual(true, meck:validate(stream_client)),
            meck:unload(stream_client),
            meck:unload(httpc)
        end),

        it("should have the client passed when connected")
    end)

The filters you can use are:

* `before_each` - Run before each example
* `after_each` - Run after each example
* `before_all` - Run before all examples
* `after_all` - Run after all examples

## the `espec` command

The `espec` executable runs spec files and provides pretty output from the test results. You can pass it files or directories through which it will recursively look for spec files.

    $ espec spec
    http client
      when connected
        should pass data to the callback
        should return ok and the pid when the stream terminates (PENDING)
      when not connected
        should return an error when trying to make a request (FAILED):
          error {badmatch, false}

## Development

Start an Erlang shell with reloader support:

    make dev

Compile new code:

    make compile

Run specs:

    make spec

## Contributing

* Fork the project.
* Make your feature addition or bug fix.
* Add specs. Pull requests without tests will be ignored.
* Send me a pull request. Bonus points for topic branches.

## Authors

* [Luca Spiller](http://github.com/lucaspiller)
* [Ben Murphy](http://github.com/benmmurphy)

## License

<a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/3.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" href="http://purl.org/dc/dcmitype/InteractiveResource" property="dct:title" rel="dct:type">ESpec</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/lucaspiller/espec" property="cc:attributionName" rel="cc:attributionURL">Luca Spiller</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.<br />Based on a work at <a xmlns:dct="http://purl.org/dc/terms/" href="https://github.com/lucaspiller/espec" rel="dct:source">github.com</a>.<br />Permissions beyond the scope of this license may be available at <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/lucaspiller/espec" rel="cc:morePermissions">https://github.com/lucaspiller/espec</a>.
