%% Copyright (c) 2009 Will Larson <lethain@gmail.com>
%% <insert MIT License here>
%% @todo support for horizontal rule
%% @todo support for breaklines
%% @todo support for secondary title syntax "Title\n====="
-module(markdown).
-author("Will Larson <lethain@gmail.com>").
-version("0.0.1").
-export([markdown/1]).
-export([line_start/5, single_line/5]).
-export([trim_whitespace/2, preserve_line/5, start_of_next_line/1, starts_with_number/1, remove_top_tag/3]).
-export([identify_line_type/1]).
-export([toggle_tag/3, exclusive_insert_tag/3]).
-export([parse_link/2, parse_link_text/2, parse_link_remainder/2]).
-export([test/0, test_performance/1]).

-define(DEBUG_LOGGER, fun(_X,_Y) -> ok end).
%-define(DEBUG_LOGGER, fun(X,Y) -> io:format(X,Y) end).			      

%%
%% Primary Interface
%%

markdown(Text) when is_list(Text) ->
    markdown(list_to_binary(Text));
markdown(Binary) when is_binary(Binary) ->
    line_start(Binary, [], [], [], []).

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
     {"this is really just a test\nis that okay?\n\nand what about\nthis\n", <<"<p>this is really just a test is that okay?</p><p>and what about this</p>">>}
    ].

test_performance(N) ->
    Str1 = "* **this is a test** *and so is this*\n* another line\n\n1. a line\n2. a line2\n3. another line\n\n",
    Str2 = ">> blockquote\n>> blockquote2\n\n    pre block1\n    same pre block\n\n",
    Str3 = "[test](http://test.com \"this\")\ this out\n ``code block``\n\n",
    Str = lists:append([Str1, Str2, Str3]),
    LongStr = lists:append(lists:map(fun(_n) -> Str end, lists:seq(0,N))),
    {Time, _} = timer:tc(markdown, markdown, [LongStr]),
    Seconds = Time / 1000000.0,
    io:format("~p chars took ~p seconds~n", [length(LongStr), Seconds]).

test() ->
    io:format("Running tests: ~n"),
    Tests = testcases(),
    FailedTests = lists:foldr(fun({Input, Output}, Failed) ->
				      case markdown(Input) of
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

%%
%% Multi-line Entities
%%

identify_line_type(<<"">>) -> {empty_line, <<"">>};
identify_line_type(<<"\n", Binary/binary>>) -> {empty_line, Binary};
identify_line_type(<<"- ", Binary/binary>>) -> {ul, Binary};
identify_line_type(<<"    - ", Binary/binary>>) -> {ul, Binary};
%identify_line_type(<<"    - ", Binary/binary>>) -> {deep_ul, Binary};
identify_line_type(<<"* ", Binary/binary>>) -> {ul, Binary};
identify_line_type(<<"    * ", Binary/binary>>) -> {ul, Binary};
%identify_line_type(<<"    * ", Binary/binary>>) -> {deep_ul, Binary};
identify_line_type(<<"    ", Binary/binary>>) -> {pre, Binary};
identify_line_type(<<">> ", Binary/binary>>) -> {blockquote, Binary};
identify_line_type(<<"> ", Binary/binary>>) -> {blockquote, Binary};
identify_line_type(<<Binary/binary>>) ->
    case starts_with_number(Binary) of
	{true, Binary2} ->
	    {ol, Binary2};
	false ->
	    {Binary3, Offset} = trim_whitespace(Binary, 4),
	    case {Offset==4, starts_with_number(Binary3)} of
		{true, {true, Binary4}} ->
		    %{deep_ol, Binary4};
		    {deep_ol, Binary4};
		_ ->
		    {p, Binary}
	    end
    end.

%% Manages closing multi-line entities.
line_start(<<Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->   
    ?DEBUG_LOGGER("line_start: ~p~n",[Binary]),
    % calculate the expected indentation depth based on current stack
    IndentDepth0 = lists:foldr(fun(Elem, Depth) ->
				       case Elem of
					   <<"ol">> -> Depth + 1;
					   <<"ul">> -> Depth + 1;
					   _ -> Depth
				       end end, 0, MultiContext),
    IndentDepth = erlang:max(IndentDepth0-1,0),
    % trim whitespace based on indent depth, 4 spaces per indentation depth
    % restrict trimming to avoid capturing pre blocks and signifigant whitespace
    % from within pre blocks
    {Binary2, Offset} = trim_whitespace(Binary, 4*IndentDepth),
    % close an appropriate number of ol/ul blocks when
    % the amount of trimmed whitespace is inadequate for
    % the current indentation depth
    CloseDepthBy = IndentDepth - trunc(Offset/4),
    {MultiContext2, Acc3} = lists:foldr(fun(_, {Tags, Acc0}) ->
						{Tags2, Acc2} = remove_top_tag([<<"ul">>, <<"ol">>], Tags, Acc0),
						remove_top_tag([<<"li">>], Tags2, Acc2)
					end, {MultiContext, Acc}, lists:seq(1,CloseDepthBy)),
    {Type, Binary3} = identify_line_type(Binary2),
    ?DEBUG_LOGGER("type (~p) and stack (~p) for ~p~n", [Type, MultiContext2, Binary]),
    {MultiContext3, Acc4} = case {Type, MultiContext2} of
				{empty_line, [<<"p">> | RestTags]} ->
				    {RestTags, [<<"</p>">> | Acc3]};
				{empty_line, [<<"pre">> | RestTags]} ->
				    {RestTags, [<<"</pre>">> | Acc3]};
				{empty_line, [<<"blockquote">> | RestTags]} ->
				    {RestTags, [<<"</blockquote>">> | Acc3]};
				{empty_line, [<<"li">>, <<"ol">> | RestTags]} ->
				    {RestTags, [<<"</ol>">>, <<"</li>">> | Acc3]};
				{empty_line, [<<"li">>, <<"ul">> | RestTags]} ->
				    {RestTags, [<<"</ul>">>, <<"</li>">> | Acc3]};
				{p, []} ->
				    {[<<"p">>], [<<"<p>">> | Acc3]};
				{p, [<<"p">> | RestTags]} ->
				    {[<<"p">> | RestTags], [<<" ">> | Acc3]};
				{p, [<<"li">> | RestTags]} ->
				    {[<<"li">> | RestTags], [<<" ">> | Acc3]};
				{p, [Tag | RestTags]} ->
				    case lists:member(Tag, [<<"pre">>, <<"blockquote">>]) of
					true ->
					    {[<<"p">> | RestTags], [<<"<p>">>,<<"</",Tag/binary,">">> | Acc3]};
					false ->
					    {[Tag | RestTags], Acc3}
				    end;
				{pre, []} ->
				    {[<<"pre">>], [<<"<pre>">> | Acc3]};
				{pre, [<<"pre">> | RestTags]} ->
				    {[<<"pre">> | RestTags], [<<"\n">> | Acc3]};
				{pre, [Tag | RestTags]} ->
				    case lists:member(Tag, [<<"p">>, <<"blockquote">>]) of
					true ->
					    {[<<"pre">> | RestTags], [<<"<pre>">>,<<"</",Tag/binary,">">> | Acc3]};
					false ->
					    {[Tag | RestTags], Acc3}
				    end;
				{blockquote, []} ->
				    {[<<"blockquote">>], [<<"<blockquote>">> | Acc3]};
				{blockquote, [<<"blockquote">> | RestTags]} ->
				    {[<<"blockquote">> | RestTags], [<<" ">> | Acc3]};
				{blockquote, [Tag | RestTags]} ->
				    case lists:member(Tag, [<<"pre">>, <<"p">>]) of
					true ->
					    {[<<"blockquote">> | RestTags], [<<"<blockquote>">>,<<"</",Tag/binary,">">> | Acc3]};
					false ->
					    {[Tag | RestTags], Acc3}
				    end;
				{ol, [<<"li">> | RestTags]} ->
				    {[<<"li">> | RestTags], [<<"<li>">>, <<"</li>">> | Acc3]};
				{ol, RestTags} ->
				    {[<<"li">>, <<"ol">> | RestTags], [<<"<li>">>, <<"<ol>">> | Acc3]};
				{ul, [<<"li">> | RestTags]} ->
				    {[<<"li">> | RestTags], [<<"<li>">>, <<"</li>">> | Acc3]};
				{ul, RestTags} ->
				    {[<<"li">>, <<"ul">> | RestTags], [<<"<li>">>, <<"<ul>">> | Acc3]};
				_ ->
				    {MultiContext2, Acc3}
			    end,
    case {Type, Binary3} of
	{empty_line, <<"">>} ->
	    single_line(Binary3, OpenTags, Acc4, LinkContext, MultiContext3);
	{empty_line, _} ->
	    line_start(Binary3, OpenTags, Acc4, LinkContext, MultiContext3);
	_ ->
	    single_line(Binary3, OpenTags, Acc4, LinkContext, MultiContext3)
    end.

%%
%% Single Line Entities (headers, em, strong, links, code)
%%

%% Wrapup function, called at end of document.
single_line(<<"">>, OpenTags, Acc, _LinkContext, MultiContext) ->
    Open = lists:reverse(lists:append([OpenTags, MultiContext])),
    ?DEBUG_LOGGER("remaining_tags: ~p~n", [Open]),

    ClosedTags = lists:foldr(fun(Tag, Acc2) ->
				     [<<"</",Tag/binary,">">> | Acc2]
			     end, Acc, Open),
    % markdown is gathered in reverse order
    Reversed = lists:reverse(ClosedTags),
    list_to_binary(lists:append(lists:map(fun(X) -> binary_to_list(X) end, Reversed)));


%% Pass control to multi-line entity handler when
%% encountering new-line.
single_line(<<"\n", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    line_start(Rest, OpenTags, Acc, LinkContext, MultiContext);

single_line(<<"#####", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = exclusive_insert_tag(<<"h5">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"####", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = exclusive_insert_tag(<<"h4">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"###", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = exclusive_insert_tag(<<"h3">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"##", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = exclusive_insert_tag(<<"h2">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"#", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = exclusive_insert_tag(<<"h1">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"**", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = toggle_tag(<<"strong">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"*", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = toggle_tag(<<"em">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"``", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = toggle_tag(<<"code">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"`", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    {OpenTags2, Acc2} = toggle_tag(<<"code">>, OpenTags, Acc),
    single_line(Rest, OpenTags2, Acc2, LinkContext, MultiContext);
single_line(<<"![", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    case parse_link(<<"[", Rest/binary>>, LinkContext) of
	{link, Rest2, Href, Text, []} ->
	    Img = <<"<img src=\"", Href/binary, "\" alt=\"", Text/binary, "\">">>,
	    single_line(Rest2, OpenTags, [Img | Acc], LinkContext, MultiContext);
	{link, Rest2, Href, Text, Title} ->
	    Img = <<"<img src=\"", Href/binary, "\" alt=\"", Text/binary, "\" title=\"", Title/binary, "\">">>,
	    single_line(Rest2, OpenTags, [Img | Acc], LinkContext, MultiContext)
    end;
single_line(<<"[", Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    case parse_link(<<"[" , Rest/binary>>, LinkContext) of
	{link, Rest2, Href, Text, <<"">>} ->
	    Link = <<"<a href=\"", Href/binary, "\">", Text/binary, "</a>">>,
	    single_line(Rest2, OpenTags, [Link | Acc], LinkContext, MultiContext);
	{link, Rest2, Href, Text, Title} ->
	    Link = <<"<a href=\"", Href/binary, "\" title=\"", Title/binary, "\">", Text/binary, "</a>">>,
	    single_line(Rest2, OpenTags, [Link | Acc], LinkContext, MultiContext);
	{context, Rest2, LinkContext2} ->
	    single_line(Rest2, OpenTags, Acc, LinkContext2, MultiContext)
    end;
single_line(<<B:1/binary, Rest/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    single_line(Rest, OpenTags, [B | Acc], LinkContext, MultiContext).

%%
%% Utility functions (parsing links, managing tags, etc)
%%


%% @doc sub-parser for handling links.
%%      Handles formats:
%%        [This is a test](http://test.com/ "The title")
%%        [This is a test][test]
%%
%%     For the second format, you'll need to have previously
%%     specified the link using the format
%%        [test]: http://test.com/ "Test Title"
%%
%% @spec parse_link() -> link_components() | new_context()
%%       new_context = {context, binary(), proplist()}
%%       link_components = {link, binary(), href(), text(), title()}
%%       href = string()
%%       text = string()
%%       title = string() | undefined
%%       proplist = [{binary(), binary()}]
parse_link(Binary, LinkContext) ->
    {Binary2, Text} = parse_link_text(Binary,[]),
    case Binary2 of
	<<"(", Binary3/binary>> ->
	    {Binary4, Link, Title} =  parse_link_remainder(Binary3, <<")">>),
	    {link, Binary4, Link, Text, Title};
	<<"[",Binary3/binary>> ->
	    {Binary4, Reference} = parse_link_text(<<"[",Binary3/binary>>, []),
	    case proplists:get_value(Reference, LinkContext) of
		{Link, Title} ->	    
		    {link, Binary4, Link, Text, Title};
		_ ->
		    {syntax_error, reference_to_undeclared_link_definition}
	    end;
	<<":",Binary3/binary>> -> 
	    {Binary4, Link, Title} =  parse_link_remainder(Binary3, <<"\n">>),
	    {context, Binary4, [{Text, {Link, Title}} | LinkContext]};
	<<_:1/binary, _Binary3/binary>> ->
	    {syntax_error, expected_paren_bracket_or_colon}
    end.

%% @doc parse the text portion of a link.
%%      For example, parse "test" from [test][this].
parse_link_text(<<"\n", _Binary/binary>>, _Acc) ->
    {syntax_error, unexpected_newline_in_link};
parse_link_text(<<"[",Binary/binary>>, Acc) ->
    parse_link_text(Binary, Acc);
parse_link_text(<<"]",Binary/binary>>, Acc) ->
    Reversed = lists:reverse(Acc),
    Text = lists:append(lists:map(fun(X) -> binary_to_list(X) end, Reversed)),				     
    {Binary, list_to_binary(Text)};
parse_link_text(<<Char:1/binary, Binary/binary>>, Acc) ->
    parse_link_text(Binary, [Char | Acc]).

%% @doc Parse the link and title out of Markdown link remainder.
%%      'http:/test/ "this"' has link of "http:/test/" and title of "this"
%%
%%      Example:
%%	    {Binary4, Link, Title} =  parse_link_remainder(Binary3, <<")">>),
parse_link_remainder(<<Binary/binary>>, <<EndChar:1/binary>>) ->
    parse_link_remainder(Binary, EndChar, [], [], link).
parse_link_remainder(<<EndChar:1/binary>>, EndChar, LinkAcc, TitleAcc, _) ->
    Link = list_to_binary(lists:append(lists:map(fun(X) -> binary_to_list(X) end, lists:reverse(LinkAcc)))),
    Title = list_to_binary(lists:append(lists:map(fun(X) -> binary_to_list(X) end, lists:reverse(TitleAcc)))),
    {<<"">>, Link, Title};
parse_link_remainder(<<EndChar:1/binary, Binary/binary>>, EndChar, LinkAcc, TitleAcc, _) ->
    Link = list_to_binary(lists:append(lists:map(fun(X) -> binary_to_list(X) end, lists:reverse(LinkAcc)))),
    Title = list_to_binary(lists:append(lists:map(fun(X) -> binary_to_list(X) end, lists:reverse(TitleAcc)))),
    {Binary, Link, Title};
parse_link_remainder(<<"\"", Binary/binary>>, EndChar, LinkAcc, TitleAcc, title) ->
    parse_link_remainder(<<Binary/binary>>, EndChar, LinkAcc, TitleAcc, done);
parse_link_remainder(<<"\"", Binary/binary>>, EndChar, LinkAcc, [], link) ->
    parse_link_remainder(Binary, EndChar, LinkAcc, [], title);
parse_link_remainder(<<" ", Binary/binary>>, EndChar, LinkAcc, [], link) ->
    parse_link_remainder(Binary, EndChar, LinkAcc, [], link);
parse_link_remainder(<<Char:1/binary, Binary/binary>>, EndChar, LinkAcc, [], link) ->
    parse_link_remainder(Binary, EndChar, [Char | LinkAcc], [], link);
parse_link_remainder(<<"\"", Binary/binary>>, EndChar, LinkAcc, TitleAcc, title) ->
    parse_link_remainder(Binary, EndChar, LinkAcc, TitleAcc, done);
parse_link_remainder(<<Char:1/binary, Binary/binary>>, EndChar, LinkAcc, TitleAcc, title) ->
    parse_link_remainder(Binary, EndChar, LinkAcc, [Char | TitleAcc], title).

%% @doc remove all whitespace from a newline, returns count
%%      of whitespace and trimmed binary.
trim_whitespace(<<Binary/binary>>, Max) ->
    trim_whitespace(Binary, 0, Max).
trim_whitespace(<<Binary/binary>>, Max, Max) ->
    {Binary, Max};
trim_whitespace(<<" ", Binary/binary>>, Offset, Max) ->
    trim_whitespace(Binary, Offset+1, Max);
trim_whitespace(<<Binary/binary>>, Offset, _Max) ->
    {Binary, Offset}.

%% @doc close a tag if it is in the open tags stack,
%%      otherwise open it.
%% @spec toggle_tag(tag(), tag_stack(), html()) -> {tag_stack(), html()}
%%       tag = binary()
%%       tag_stack = [tag()]
%%       html = [binary()]
toggle_tag(Tag, OpenTags, Acc) ->
    case lists:member(Tag, OpenTags) of
	true ->
	    {lists:delete(Tag, OpenTags), [<<"</",Tag/binary,">">> | Acc]};
	false ->
	    {[Tag | OpenTags], [<<"<",Tag/binary,">">> | Acc]}
    end.

%% @doc insert tag IFF it isn't already on the
%%      stack of open tags.
%% @spec exclusive_insert_tag(tag(), tag_stack(), html()) -> {tag_stack(), html()}
%%       tag = binary()
%%       tag_stack = [tag()]
%%       html = [binary()]
exclusive_insert_tag(Tag, OpenTags, Acc) ->
    case lists:member(Tag,OpenTags) of
	true ->
	    {OpenTags, Acc};
	false ->
	    {[Tag | OpenTags], [<<"<",Tag/binary,">">> | Acc]}
    end.

%% @doc consume an entire line as is without modification
preserve_line(<<"">>, OpenTags, Acc, LinkContext, MultiContext) ->
    line_start(<<"">>, OpenTags, Acc, LinkContext, MultiContext);
preserve_line(<<"\n",Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    line_start(Binary, OpenTags, [<<"\n">> | Acc], LinkContext, MultiContext);
preserve_line(<<Char:1/binary,Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    preserve_line(Binary, OpenTags, [Char | Acc], LinkContext, MultiContext).

%% @doc skip remainder of line
start_of_next_line(<<"">>) ->
    <<"">>;
start_of_next_line(<<"\n", Binary/binary>>) ->
    Binary;
start_of_next_line(<<_Char:1/binary, Binary/binary>>) ->
    start_of_next_line(Binary).

%% @doc determine if line starts with a number
starts_with_number(<<Binary/binary>>) ->
    starts_with_number(Binary, []).
starts_with_number(<<"">>, []) ->
    false;
starts_with_number(<<".", _Binary/binary>>, []) ->
    false;
starts_with_number(<<". ", Binary/binary>>, _Acc) ->
    {true, Binary};
starts_with_number(<<Char:1/binary, Binary/binary>>, Acc) ->
    try 
	_Integer = list_to_integer(binary_to_list(Char)),
	starts_with_number(Binary, [Char | Acc])
    catch 
	_:_ ->
	    false 
    end.

%% @doc remove the first occurance of any of the tags in ToRemove.
remove_top_tag(ToRemove, Tags, Html) ->
    {_, Tags3, Html3} = lists:foldr(fun(X, {Done, Acc, Html2}) ->
			case {Done, lists:member(X, ToRemove)} of
			    {true, _} ->
				{true, [X | Acc], Html2};
			    {false, true} ->
				{true, Acc, [<<"</",X/binary,">">> | Html2]};
			    {false, false} ->
				{false, [X | Acc], Html2}
			end
		end, {false,[],Html}, Tags),
    {Tags3, Html3}.

