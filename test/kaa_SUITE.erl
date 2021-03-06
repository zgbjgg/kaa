-module(kaa_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("kaa.hrl").
-include("kaa_worker.hrl").
-include("kaa_error.hrl").
-include("kaa_result.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_kaa_worker/1,
    test_kaa_worker_call/1,
    test_kaa_worker_cast/1,
    test_kaa_worker_stop/1,
    test_kaa_get_worker/1,
    test_kaa_proto_in_path/1,
    test_kaa_proto_in_int/1,
    test_kaa_proto_in_double/1,
    test_kaa_proto_in_string/1,
    test_kaa_proto_in_binary/1,
    test_kaa_proto_in_columns/1,
    test_kaa_get_worker_non_pid/1,
    test_kaa_exec_no_binary/1,
    test_kaa_exec_no_proto_msg/1,
    test_kaa_proto_in_error/1,
    test_kaa_proto_plotting/1,
    test_kaa_proto_raw_plotting/1,
    test_kaa_proto_selection/1,
    test_kaa_proto_groupby/1,
    test_kaa_proto_sort/1,
    test_kaa_proto_sort_index/1,
    test_kaa_proto_iplot/1,
    test_kaa_proto_pplot/1,
    test_kaa_proto_seaborn/1,
    test_kaa_proto_in_series/1,
    test_kaa_proto_legacy_assignment/1,
    test_kaa_proto_to_datetime/1]).

all() ->
    [test_kaa_worker,
     test_kaa_worker_call,
     test_kaa_worker_cast,
     test_kaa_worker_stop,
     test_kaa_get_worker,
     test_kaa_proto_in_path,
     test_kaa_proto_in_int,
     test_kaa_proto_in_double,
     test_kaa_proto_in_string,
     test_kaa_proto_in_binary,
     test_kaa_proto_in_columns,
     test_kaa_get_worker_non_pid,
     test_kaa_exec_no_binary,
     test_kaa_exec_no_proto_msg,
     test_kaa_proto_in_error,
     test_kaa_proto_plotting,
     test_kaa_proto_raw_plotting,
     test_kaa_proto_selection,
     test_kaa_proto_groupby,
     test_kaa_proto_sort,
     test_kaa_proto_sort_index,
     test_kaa_proto_iplot,
     % test_kaa_proto_pplot,
     test_kaa_proto_seaborn,
     test_kaa_proto_in_series,
     test_kaa_proto_legacy_assignment,
     test_kaa_proto_to_datetime].

init_per_testcase(Testcase, _Config) ->
    ok = application:start(mnesia),
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(erlport),
    ok = application:start(syn),
    ok = application:start(jun),
    ok = application:start(kaa),
    % for each case start a new worker
    {ok, Key} = kaa_main_worker:start_link(),
    {ok, Pb} = kaa_main_worker:get_worker(Key),
    #'KaaWorker'{jun_worker = Worker} = kaa_worker:decode_msg(Pb, 'KaaWorker'),
    {ok, Cwd} = file:get_cwd(),
    % use the helper read_csv_instruction from kaa_proto
    Path = case Testcase of
        test_kaa_proto_seaborn -> "/../../lib/kaa/test/files/csv2.txt";
        _                      -> "/../../lib/kaa/test/files/csv.txt"
    end,
    Ins = read_csv_instruction(Worker, Cwd ++ Path),
    [{kaa_worker, Key}, {worker, Worker}, {ins, Ins}].

end_per_testcase(_, Config) ->
    % @todo stop the worker
    Key = proplists:get_value(kaa_worker, Config),
    % this is to skip the stop link process for the worker stop test
    ok = case syn:find_by_key(Key) of
        undefined -> ok;
        _Pid      ->
          kaa_main_worker:stop_link(Key)
    end,
    ok = application:stop(mnesia),
    ok = application:stop(compiler),
    ok = application:stop(syntax_tools),
    ok = application:stop(goldrush),
    ok = application:stop(lager),
    ok = application:stop(erlport),
    ok = application:stop(syn),
    ok = application:stop(jun),
    ok = application:stop(kaa),
    ok.

test_kaa_worker([{kaa_worker, Key}, _, _]) ->
    Pid = syn:find_by_key(Key),
    ?assertEqual(is_pid(Pid), true).

test_kaa_worker_stop([{kaa_worker, Key}, _, _]) ->
    Pid = syn:find_by_key(Key),
    ok = kaa_main_worker:stop_link(Key),
    ?assertEqual(erlang:is_process_alive(Pid), false).

test_kaa_get_worker([{kaa_worker, Key}, _, _]) ->
    {ok, Pb} = kaa_main_worker:get_worker(Key),
    #'KaaWorker'{jun_worker = Worker} = kaa_worker:decode_msg(Pb, 'KaaWorker'),
    ?assertEqual(true, is_pid(list_to_pid(Worker))).

test_kaa_proto_in_path([{kaa_worker, Key}, _, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    ?assertMatch({dataframe, _}, Result).

test_kaa_proto_in_int([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    SumIns = common_instruction(Worker, DataFrame, sum, "age", []),
    {ok, PbOutSum} = kaa_main_worker:kaa_proto_in(Key, SumIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutSum, 'KaaResult'),
    ?assertEqual({inumber, 198}, Result).

test_kaa_proto_in_double([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    MedianIns = common_instruction(Worker, DataFrame, median, "age", []),
    {ok, PbOutMedian} = kaa_main_worker:kaa_proto_in(Key, MedianIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutMedian, 'KaaResult'),
    ?assertEqual({dnumber, 30.0}, Result).

test_kaa_proto_in_string([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    ToCsvIns = common_instruction(Worker, DataFrame, to_csv, none, []),
    {ok, PbOutToCsv} = kaa_main_worker:kaa_proto_in(Key, ToCsvIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutToCsv, 'KaaResult'),
    ?assertMatch({string, _}, Result).

test_kaa_proto_in_binary([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    ToJsonIns = common_instruction(Worker, DataFrame, to_json, none, [#'Keywords'{key = "orient", value = {svalue, "records"}}]),
    {ok, PbOutToJson} = kaa_main_worker:kaa_proto_in(Key, ToJsonIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutToJson, 'KaaResult'),
    ?assertMatch({string, _}, Result).

test_kaa_proto_in_error([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    SumIns = common_instruction(Worker, DataFrame, sum, "unknown", []),
    {ok, PbOutSum} = kaa_main_worker:kaa_proto_in(Key, SumIns),
    #'KaaError'{error = Error} = kaa_error:decode_msg(PbOutSum, 'KaaError'),
    [_, KeyError] = string:tokens(Error, "."),
    ?assertMatch("KeyError", KeyError).

test_kaa_proto_in_columns([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    ColumnsIns = common_instruction(Worker, DataFrame, columns, none, []),
    {ok, PbOutColumns} = kaa_main_worker:kaa_proto_in(Key, ColumnsIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutColumns, 'KaaResult'),
    ?assertMatch({string, _}, Result).

test_kaa_proto_plotting([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    PlotIns = common_instruction(Worker, DataFrame, plot, "fig.png", [#'Keywords'{key = "x", value = {svalue, "name"}},
        #'Keywords'{key = "y", value = {svalue, "age"}},
        #'Keywords'{key = "kind", value = {svalue, "bar"}}]),
    {ok, PbOutPlot} = kaa_main_worker:kaa_proto_in(Key, PlotIns),
     #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutPlot, 'KaaResult'),
    ?assertMatch({string, _}, Result).

test_kaa_proto_raw_plotting([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    PlotIns = common_instruction(Worker, DataFrame, plot, "None", [#'Keywords'{key = "x", value = {svalue, "name"}},
        #'Keywords'{key = "y", value = {svalue, "age"}},
        #'Keywords'{key = "kind", value = {svalue, "bar"}}]),
    {ok, PbOutPlot} = kaa_main_worker:kaa_proto_in(Key, PlotIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutPlot, 'KaaResult'),
    ?assertMatch({axesplot, _}, Result).

test_kaa_proto_selection([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    SelectionIns = common_instruction(Worker, DataFrame, selection, "age", []),
    {ok, PbOutSelection} = kaa_main_worker:kaa_proto_in(Key, SelectionIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutSelection, 'KaaResult'),
    ?assertMatch({dataframe, _}, Result).

test_kaa_proto_groupby([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    GroupByIns = common_instruction(Worker, DataFrame, groupby, "age", []),
    {ok, PbOutGroupBy} = kaa_main_worker:kaa_proto_in(Key, GroupByIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutGroupBy, 'KaaResult'),
    ?assertMatch({groupby, _}, Result).

test_kaa_proto_sort([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    SortIns = common_instruction(Worker, DataFrame, sort_values, "None", [#'Keywords'{key = "by", value = {svalue, "age"}},
        #'Keywords'{key = "ascending", value = {svalue, "True"}}]),
    {ok, PbOutSort} = kaa_main_worker:kaa_proto_in(Key, SortIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutSort, 'KaaResult'),
    ?assertMatch({dataframe, _}, Result).

test_kaa_proto_sort_index([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    SortIns = common_instruction(Worker, DataFrame, sort_index, "None", [#'Keywords'{key = "ascending", value = {svalue, "True"}}]),
    {ok, PbOutSort} = kaa_main_worker:kaa_proto_in(Key, SortIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutSort, 'KaaResult'),
    ?assertMatch({dataframe, _}, Result).

test_kaa_proto_iplot([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    PlotIns = common_instruction(Worker, DataFrame, iplot, "key001", [#'Keywords'{key = "x", value = {svalue, "name"}},
        #'Keywords'{key = "y", value = {svalue, "age"}},
        #'Keywords'{key = "kind", value = {svalue, "bar"}},
        #'Keywords'{key = "asFigure", value = {svalue, "True"}}]),
    {ok, PbOutPlot} = kaa_main_worker:kaa_proto_in(Key, PlotIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutPlot, 'KaaResult'),
    ?assertMatch({iplot, _}, Result).

test_kaa_proto_pplot([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    PlotIns = common_instruction(Worker, DataFrame, iplot, "key001", [#'Keywords'{key = "x", value = {svalue, "name"}},
        #'Keywords'{key = "y", value = {svalue, "age"}},
        #'Keywords'{key = "kind", value = {svalue, "bar"}},
        #'Keywords'{key = "asFigure", value = {svalue, "True"}}]),
    {ok, PbOutPlot} = kaa_main_worker:kaa_proto_in(Key, PlotIns),
    #'KaaResult'{ok = "ok", result = {iplot, "key001"}} = kaa_result:decode_msg(PbOutPlot, 'KaaResult'),
    PlotIns1 = common_instruction(Worker, "key001", pplot, "pplot", []),
    {ok, PbOutPlot1} = kaa_main_worker:kaa_proto_in(Key, PlotIns1),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutPlot1, 'KaaResult'),
    ?assertMatch({string, _}, Result).

test_kaa_proto_in_series([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    ApplyIns = common_instruction(Worker, DataFrame, apply, "None", [#'Keywords'{key = "axis", value = {ivalue, 1}},
        #'Keywords'{key = "lambda", value = {svalue, "lambda row : row['age'] + 7"}}]),
    {ok, PbOutApply} = kaa_main_worker:kaa_proto_in(Key, ApplyIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutApply, 'KaaResult'),
    ?assertMatch({series, _}, Result).

test_kaa_proto_legacy_assignment([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    ApplyIns = common_instruction(Worker, DataFrame, apply, "None", [#'Keywords'{key = "axis", value = {ivalue, 1}},
        #'Keywords'{key = "lambda", value = {svalue, "lambda row : row['age'] + 7"}}]),
    {ok, PbOutApply} = kaa_main_worker:kaa_proto_in(Key, ApplyIns),
    #'KaaResult'{ok = "ok", result = {series, Series}} = kaa_result:decode_msg(PbOutApply, 'KaaResult'),
    LegacyAssignmentIns = common_instruction(Worker, DataFrame, legacy_assignment, Series, [#'Keywords'{key = "column", value = {svalue, "cl"}}]),
    {ok, PbOutLegacyAssignment} = kaa_main_worker:kaa_proto_in(Key, LegacyAssignmentIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutLegacyAssignment, 'KaaResult'),
    ?assertMatch({dataframe, _}, Result).

test_kaa_proto_to_datetime([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    SelectionIns = common_instruction(Worker, DataFrame, single_selection, "age", []),
    {ok, PbOutSelection} = kaa_main_worker:kaa_proto_in(Key, SelectionIns),
    #'KaaResult'{ok = "ok",result = {series, Series}} = kaa_result:decode_msg(PbOutSelection, 'KaaResult'),
    ToDateTimeIns =  core_instruction(Worker, to_datetime, Series, []),
    {ok, PbOutToDateTime} = kaa_main_worker:kaa_proto_in(Key, ToDateTimeIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutToDateTime, 'KaaResult'),
    ?assertMatch({series, _}, Result).

%% @TODO: maybe more tests for seaborn?

test_kaa_proto_seaborn([{kaa_worker, Key}, {worker, Worker}, {ins, Ins}]) ->
    {ok, PbOut} = kaa_main_worker:kaa_proto_in(Key, Ins),
    #'KaaResult'{ok = "ok", result = R} = kaa_result:decode_msg(PbOut, 'KaaResult'),
    {dataframe, DataFrame} = R,
    LmplotIns = common_instruction(Worker, DataFrame, seaborn, lmplot,
        "fig.png", [#'Keywords'{key = "x", value = {svalue, "month"}},
            #'Keywords'{key = "y", value = {svalue, "users"}},
            #'Keywords'{key = "hue", value = {svalue, "smoker"}}]),
    {ok, PbOutPlot} = kaa_main_worker:kaa_proto_in(Key, LmplotIns),
    #'KaaResult'{ok = "ok", result = Result} = kaa_result:decode_msg(PbOutPlot, 'KaaResult'),
    ?assertMatch({string, _}, Result).

%% other errors directly to kaa_proto

test_kaa_get_worker_non_pid([_, _, _]) ->
    Pb = kaa_proto:worker(i_am_not_a_pid),
    #'KaaError'{error = Err} = kaa_error:decode_msg(Pb, 'KaaError'),
    ?assertEqual("no_jun_worker", Err).

test_kaa_exec_no_binary([_, _, _]) ->
    Pb = kaa_proto:exec(i_am_not_a_pb_msg),
    #'KaaError'{error = Err} = kaa_error:decode_msg(Pb, 'KaaError'),
    ?assertEqual("invalid_kaa_proto_message", Err).

test_kaa_exec_no_proto_msg([_, _, _]) ->
    Pb = kaa_proto:exec(<<"i_am_not_a_proto_msg">>),
    #'KaaError'{error = Err} = kaa_error:decode_msg(Pb, 'KaaError'),
    ?assertEqual("invalid_kaa_proto_message", Err).

% just to increment % of coverage

test_kaa_worker_call([{kaa_worker, Key}, _, _]) ->
    Pid = syn:find_by_key(Key),
    R = gen_server:call(Pid, hello),
    ?assertEqual(ok, R).

test_kaa_worker_cast([{kaa_worker, Key}, _, _]) ->
    Pid = syn:find_by_key(Key),
    R = gen_server:cast(Pid, hello),
    ?assertEqual(ok, R).

% helpers

%% Sample of usage for kaa proto encoder
%% This should be implemented from client side, but we put here
%% as a sample and test how works kaa modules. Also clients are
%% responsible for implement result and error proto files. 
read_csv_instruction(JunWorker, PathToCsv) ->
    Kaa = #'Kaa'{module = 'jun_pandas',
        'function' = 'read_csv',
        jun_worker = JunWorker,
        arguments = {core, #m_core{argument = PathToCsv, keywords = []}}},
    kaa:encode_msg(Kaa).

core_instruction(JunWorker, Fn, Argument, Keywords) ->
    Kaa = #'Kaa'{module = 'jun_pandas',
        'function' = Fn,
         jun_worker = JunWorker,
         arguments = {core, #m_core{argument = Argument, keywords = Keywords}}},
    kaa:encode_msg(Kaa).

common_instruction(JunWorker, DataFrame, seaborn, Fn, Axis, Keywords) ->
    Kaa = #'Kaa'{module = 'jun_seaborn',
        'function' = Fn,
        jun_worker = JunWorker,
        arguments = {frame, #m_frame{dataframe = DataFrame, axis = Axis,
            keywords = Keywords}}},
    kaa:encode_msg(Kaa).

common_instruction(JunWorker, Key, pplot, Axis, Keywords)      ->
    Kaa = #'Kaa'{module = 'jun_plotly',
       'function' = plot,
       jun_worker = JunWorker,
       arguments = {frame, #m_frame{dataframe = Key, axis = Axis,
           keywords = Keywords}}},
    kaa:encode_msg(Kaa);
common_instruction(JunWorker, DataFrame, iplot, Axis, Keywords) ->
    Kaa = #'Kaa'{module = 'jun_plotly',
        'function' = iplot,
        jun_worker = JunWorker,
        arguments = {frame, #m_frame{dataframe = DataFrame, axis = Axis,
            keywords = Keywords}}},
    kaa:encode_msg(Kaa);
common_instruction(JunWorker, DataFrame, Fn, none, Keywords) ->
    Kaa = #'Kaa'{module = 'jun_pandas',
        'function' = Fn,
        jun_worker = JunWorker,
        arguments = {frame, #m_frame{dataframe = DataFrame,
            keywords = Keywords}}},
    kaa:encode_msg(Kaa);
common_instruction(JunWorker, DataFrame, Fn, Axis, Keywords) ->
    Kaa = #'Kaa'{module = 'jun_pandas',
        'function' = Fn,
        jun_worker = JunWorker,
        arguments = {frame, #m_frame{dataframe = DataFrame, axis = Axis,
            keywords = Keywords}}},
    kaa:encode_msg(Kaa).
