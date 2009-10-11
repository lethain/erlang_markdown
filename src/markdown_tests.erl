-module(markdown_tests).
-author("Will Larson <lethain@gmail.com>").
-version("0.0.2").
-compile(export_all).

%%
%% Tests
%%

testcases() ->
    [{"*test*",  <<"<p><em>test</em></p>">>},
     {"**test**", <<"<p><strong>test</strong></p>">>},
     {"``test``", <<"<p><code>test</code></p>">>},
     {"`test`", <<"<p><code>test</code></p>">>},
     {"[test](http://test.com/)",  <<"<p><a href=\"http://test.com/\">test</a></p>">>},
     {"[test](http://test.com/ \"test2\")", <<"<p><a href=\"http://test.com/\" title=\"test2\">test</a></p>">>},
     {"[test]: http://test.com/\n[test][test]", <<"<p><a href=\"http://test.com/\">test</a></p>">>},
     {"[test]: http://test.com/ \"test2\"\n[test][test]", <<"<p><a href=\"http://test.com/\" title=\"test2\">test</a></p>">>},
     {"out", <<"<p>out</p>">>},
     {"out\n", <<"<p>out</p>">>},
     {"out\nwoot\n", <<"<p>out woot</p>">>},
     {"out\nwoot", <<"<p>out woot</p>">>},
     {">> this is\n>> a test\n", <<"<blockquote>this is a test</blockquote>">>},
     {"    this is\n    a test\n", <<"<pre>this is\na test</pre>">>},
     {"    this is a\n    test\n    yep\n>> and this\n>> is too", <<"<pre>this is a\ntest\nyep</pre><blockquote>and this is too</blockquote>">>},
     {"this is a test\nand another\nand *another*\n", <<"<p>this is a test and another and <em>another</em></p>">>},
     {"    test\n    this\n    out\n\n **yep**", <<"<pre>test\nthis\nout</pre><p> <strong>yep</strong></p>">>},
     {"this is *a test*\n\n    import test\n    import test2\nstill a **test**\n", <<"<p>this is <em>a test</em></p><pre>import test\nimport test2</pre><p>still a <strong>test</strong></p>">>},
     {"test\nthis\n\n>> out", <<"<p>test this</p><blockquote>out</blockquote>">>},
     {"test\nthis\n\n>> out\n>> again\n", <<"<p>test this</p><blockquote>out again</blockquote>">>},
     {"test this\n**out**\n\n>> and this\n>> as well\n\n    woo\n    hoo\n", <<"<p>test this <strong>out</strong></p><blockquote>and this as well</blockquote><pre>woo\nhoo</pre>">>},
     {"    this is a test\n    of pre\n\n* test this\n* out\n\n woohoo", <<"<pre>this is a test\nof pre</pre><ul><li>test this</li><li>out</li></ul><p> woohoo</p>">>},
     {" *test this ``out``*", <<"<p> <em>test this <code>out</code></em></p>">>},
     {" *test this **out***", <<"<p> <em>test this <strong>out</strong></em></p>">>},
     {"* this is a test\n* so is this\n* and this\n", <<"<ul><li>this is a test</li><li>so is this</li><li>and this</li></ul>">>},
     {"* *test this **out***\n", <<"<ul><li><em>test this <strong>out</strong></em></li></ul>">>},
     {"1. test\n2. test", <<"<ol><li>test</li><li>test</li></ol>">>},
     {"1. this is a test\n2. so is this\n\n* and so on\n* and further\n\na paragraph\n", <<"<ol><li>this is a test</li><li>so is this</li></ol><ul><li>and so on</li><li>and further</li></ul><p>a paragraph</p>">>},
     {"* *test this **out***", <<"<ul><li><em>test this <strong>out</strong></em></li></ul>">>},
     {"this is really just a test\nis that okay?\n\nand what about\nthis\n", <<"<p>this is really just a test is that okay?</p><p>and what about this</p>">>},
     {"------------\n", <<"<hr>">>},
     {"hello\n------------\ngoodbye\n", <<"<p>hello</p><hr><p>goodbye</p>">>},
     {"trying  \nthis  \nout\n", <<"<p>trying<br> this<br> out</p>">>},
     {"* test\n    * this\n", <<"<ul><li>test<ul><li>this</li></ul></li></ul>">>},
     {"* test\n    * *test*\n* hi\n\n what about now?\n", <<"<ul><li>test<ul><li><em>test</em></li></ul></li><li>hi</li></ul><p> what about now?</p>">>},
     {"* test\n    * test2\n        * test3\n", <<"<ul><li>test<ul><li>test2<ul><li>test3</li></ul></li></ul></li></ul>">>},
     {"* test\n    * test2\n        * test3\n            * test4\n", <<"<ul><li>test<ul><li>test2<ul><li>test3<ul><li>test4</li></ul></li></ul></li></ul></li></ul>">>},

     {"1. test\n    2. this\n", <<"<ol><li>test<ol><li>this</li></ol></li></ol>">>},
     {"1. test\n    2. *test*\n3. hi\n\n what about now?\n", <<"<ol><li>test<ol><li><em>test</em></li></ol></li><li>hi</li></ol><p> what about now?</p>">>},
     {"* a\n    * b\n    * c\n* d\n", <<"<ul><li>a<ul><li>b</li><li>c</li></ul></li><li>d</li></ul>">>}
    ].

gen_string(N) ->
    Str1 = "* **this is a test** *and so is this*\n* another line\n\n1. a line\n2. a line2\n3. another line\n\n",
    Str2 = ">> blockquote\n>> blockquote2\n\n    pre block1\n    same pre block\n\n",
    Str3 = "[test](http://test.com \"this\")\ this out\n ``code block``\n\n",
    Str4 = "1. This is a test\n    2. so is this\n    3. yep...n4. yep\n\n* hi\n    * there\n    * ayep..\n * the end\n\n",
    Str = lists:append([Str1, Str2, Str3, Str4]),
    lists:append(lists:map(fun(_n) -> Str end, lists:seq(0,N))).

test_performance(N) ->
    test_performance(N, 10).

test_performance(N, Runs) ->
    LongStr = gen_string(N),
    LongBinary = list_to_binary(LongStr),
    Times = lists:map(fun(_X) ->
			      timer:tc(markdown, markdown, [LongBinary])
		      end, lists:seq(1,Runs)),
    Time = lists:foldr(fun({Micros, _}, Acc) ->
			       Micros + Acc
		       end, 0, Times),
    Seconds = (Time / N) / 1000000.0,
    Length = length(LongStr),
    io:format("~p chars, ~p seconds, ~p us/char~n", [Length, Seconds, (Time/N)/Length]).

test() ->
    io:format("Running tests: ~n"),
    Tests = testcases(),
    FailedTests = lists:foldr(fun({Input, Output}, Failed) ->
				      case markdown:markdown(Input) of
					  Output ->
					      io:format("."),
					      Failed;
					  Other ->
					      io:format("E"),
					      [{Input, Output, Other} | Failed]
				      end end, [], Tests),
    io:format("~n"),
    case length(FailedTests) > 0 of
	true ->
	    lists:foreach(fun({In, Expected, Got}) ->
				  io:format("Failed: ~p != ~p (input: ~p)~n", [Got, Expected, In])
			  end,FailedTests);
	false ->
	    ok
    end,
    TestsLen = length(Tests),
    FailedLen = length(FailedTests),
    PassedLen = TestsLen - FailedLen,
    io:format("~p Tests Passed / ~p Tests Failed / ~p Total Tests~n", [PassedLen, FailedLen, TestsLen]).

