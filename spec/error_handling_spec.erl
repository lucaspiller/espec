-module(error_handling_spec).
-include("espec.hrl").

spec() ->
  describe("badmatch errors", fun() ->
        it("should be caught and returned", fun() ->
              State = espec:run_spec(error_example_spec, badmatch_error_example_spec(), espec_null_listener:new(), espec_null_listener),
              {error, {error, {badmatch,false}, _}} = proplists:get_value("should fail", State)
          end)
    end),

  describe("throws", fun() ->
        it("should be caught and returned", fun() ->
              State = espec:run_spec(error_example_spec, throw_error_example_spec(), espec_null_listener:new(), espec_null_listener),
              {error, {throw, something_went_wrong, _}} = proplists:get_value("should fail", State)
          end)
    end),

  describe("exits", fun() ->
        it("should be caught and returned", fun() ->
              State = espec:run_spec(error_example_spec, exit_error_example_spec(), espec_null_listener:new(), espec_null_listener),
              {error, {exit, goodbye, _}} = proplists:get_value("should fail", State)
        end),

        it("should not propagate error to next spec", fun() ->
              State = espec:run_spec(error_should_not_propagate_to_next_example_spec, error_should_not_propagate_to_next_example_spec(),
                 espec_null_listener:new(), espec_null_listener),
              {error, {exit, goodbye, _}} = proplists:get_value("should fail", State),
              ok =  proplists:get_value("should not fail", State)
        end)
    end),

  describe("before all filter errors", fun() ->
        it("should treat the tests as failing if a before all fails", fun() ->
              State = espec:run_spec(before_all_handling_spec, before_all_handling_spec(), espec_null_listener:new(), espec_null_listener),
              [before_all] = get(before_all_handling_spec),
              {error,{throw,something_went_wrong, _}} =  proplists:get_value("should do stuff", State)
        end),

        it("should not break tests in an outer context if a before all fails", fun() ->
              _State = espec:run_spec(before_all_nested_handling_spec, before_all_nested_handling_spec(), espec_null_listener:new(), espec_null_listener),
              [example1, before_all, example2] = get(before_all_nested_handling_spec)
        end)
  end),

  describe("after all filter errors", fun() ->
        it("should treat the test as succeeded", fun() ->
              _State = espec:run_spec(after_all_handling_spec, after_all_handling_spec(), espec_null_listener:new(), espec_null_listener),
              [should_do_stuff, after_all] = get(after_all_handling_spec)
        end),

        it("should flag the after all as failing")
  end),

  describe("before each filter errors", fun() ->
        it("should treat the test as failed if a before each fails", fun() ->
              State = espec:run_spec(before_each_handling_spec, before_each_handling_spec(), espec_null_listener:new(), espec_null_listener),
              [before_each] = get(before_each_handling_spec),
              {error, {throw, something_went_wrong, _}} =  proplists:get_value("should do stuff", State)
        end)
  end),

  describe("after each filter errors", fun() ->
        it("should treat the test as failed if an after each fails", fun() ->
              State = espec:run_spec(after_each_handling_spec, after_each_handling_spec(), espec_null_listener:new(), espec_null_listener),
              [should_do_stuff, after_each] = get(after_each_handling_spec),
              {error, {throw, something_went_wrong, _}} = proplists:get_value("should do stuff", State)
        end)
  end).

%
% Example specs hould be caught and returnedor testing
%

after_each_handling_spec() ->
  describe("after each handling", fun() ->
      it("should do stuff", fun() ->
            spec_helper:append(should_do_stuff, after_each_handling_spec)
      end),

      after_each(fun() ->
          spec_helper:append(after_each, after_each_handling_spec),
          throw(something_went_wrong)
      end)
  end).


before_each_handling_spec() ->
  describe("before each handling", fun() ->
      it("should do stuff", fun() ->
          spec_helper:append(should_do_stuff, before_each_handling_spec)
      end),

      before_each(fun() ->
          spec_helper:append(before_each, before_each_handling_spec),
          throw(something_went_wrong)
      end)
  end).

before_all_handling_spec() ->
  describe("before all handling", fun() ->
      it("should do stuff", fun() ->
          spec_helper:append(should_do_stuff, before_all_handling_spec)
      end),

      describe("nested stuff", fun() ->
          it("should do nested stuff", fun() ->
              spec_helper:append(should_do_nested_stuff, before_all_handling_spec)
          end)
      end),

      before_all(fun() ->
          spec_helper:append(before_all, before_all_handling_spec),
          throw(something_went_wrong)
      end)
  end).

before_all_nested_handling_spec() ->
  describe("before all nested handling", fun() ->
      it("should do stuff", fun() ->
          spec_helper:append(example1, before_all_nested_handling_spec)
      end),

      describe("nested stuff", fun() ->
          it("should do nested stuff", fun() ->
              spec_helper:append(should_do_nested_stuff, before_all_nested_handling_spec)
          end),

          it("should do other nested stuff", fun() ->
              spec_helper:append(should_do_other_nested_stuff, before_all_nested_handling_spec)
          end),
          
          before_all(fun() ->
              spec_helper:append(before_all, before_all_nested_handling_spec),
              throw(something_went_wrong)
          end)

      end),
     
      it("should do other stuff", fun() ->
         spec_helper:append(example2, before_all_nested_handling_spec)
      end)

  end).



after_all_handling_spec() ->
  describe("after all handling", fun() ->
      it("should do stuff", fun() ->
          spec_helper:append(should_do_stuff, after_all_handling_spec)
      end),

      after_all(fun() ->
          spec_helper:append(after_all, after_all_handling_spec),
          throw(something_went_wrong)
      end)
  end).

badmatch_error_example_spec() ->
  describe("error spec", fun() ->
      it("should fail", fun() ->
            A = true,
            B = false,
            A = B
      end)
  end).

throw_error_example_spec() ->
  describe("error spec", fun() ->
      it("should fail", fun() ->
            throw(something_went_wrong)
      end)
  end).

exit_error_example_spec() ->
  describe("error spec", fun() ->
      it("should fail", fun() ->
            exit(goodbye)
      end)
  end).

error_should_not_propagate_to_next_example_spec() ->
  describe("no propagation", fun() ->
     it("should fail", fun() ->
       exit(goodbye)
     end),
   
     it("should not fail", fun() ->
       ok
     end)
  end).
