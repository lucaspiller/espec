-module(line_number_spec).
-include("espec.hrl").


%
% Line numbers in this file are important :(
%

line_number_spec() ->
  describe("line number example spec", fun() ->
      
      % at the top
     
      before_each(fun() ->
          spec_helper:append(before_each, line_number_spec)
      end),

      it("should do first stuff", fun() ->
          % inside first stuff
          spec_helper:append(it_should_do_first_stuff, line_number_spec)
      end),
      
      % between
      
      it("should do second stuff", fun() ->
          % inside second stuff
          spec_helper:append(it_should_do_second_stuff, line_number_spec)
      end),

      describe("nested stuff", fun() ->
          before_each(fun() ->
              spec_helper:append(before_each_nested, line_number_spec)
          end),

          it("should do first nested", fun() ->
              spec_helper:append(it_should_do_first_nested, line_number_spec)
          end)

      end)
  end).
  
-define(SHOULD_DO_SECOND_STUFF, 27).
-define(OUTER_DESCRIBE_BLOCK, 11).
-define(SHOULD_DO_FIRST_NESTED_STUFF, 36).
-define(INNER_DESCRIBE_BLOCK, 32).


spec() ->
  describe("filtering by line number", fun() ->
        before_each(fun() ->
            put(line_number_spec, undefined)
        end),

        it("should run a single example if the line is inside that example", fun() ->
              espec:run_spec(line_number_spec, espec:filter_groups_by_line(?SHOULD_DO_SECOND_STUFF, line_number_spec()),
                                     espec_null_listener:new(), espec_null_listener),
              ?assertEqual([before_each, it_should_do_second_stuff], get(line_number_spec))
        end),

        it("should run a single example in a nested describe block", fun() ->
              espec:run_spec(line_number_spec, espec:filter_groups_by_line(?SHOULD_DO_FIRST_NESTED_STUFF, line_number_spec()),
                                                    espec_null_listener:new(), espec_null_listener),
              ?assertEqual([before_each, before_each_nested, it_should_do_first_nested], get(line_number_spec))
        end),

        it("should run everything in describe block if line is inside but not in an example", fun() ->
              %% this currently only works in the space at the top of the file because we don't 
              %% include the end line for blocks yet. so we assume the end point is the start of
              %% the next block :(
              espec:run_spec(line_number_spec, espec:filter_groups_by_line(?OUTER_DESCRIBE_BLOCK, line_number_spec()),
                                     espec_null_listener:new(), espec_null_listener),
              ?assertEqual([before_each, it_should_do_first_stuff, before_each, it_should_do_second_stuff,
                  before_each, before_each_nested, it_should_do_first_nested], get(line_number_spec))
        end),

        it("should run everything in a nested describe block if line is inside but not in an example", fun() ->
              espec:run_spec(line_number_spec, espec:filter_groups_by_line(?INNER_DESCRIBE_BLOCK, line_number_spec()),
                                     espec_null_listener:new(), espec_null_listener),
              ?assertEqual([before_each, before_each_nested, it_should_do_first_nested], get(line_number_spec))
        end)
    end).
