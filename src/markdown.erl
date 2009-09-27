%% Copyright (c) 2009 Will Larson <lethain@gmail.com>
%% <insert MIT License here>
%% @todo support for horizontal rule
%% @todo support for breaklines
%% @todo support for secondary title syntax "Title\n====="
-module(markdown).
-author("Will Larson <lethain@gmail.com>").
-version("0.0.1").
-export([markdown/1]).
-export([line_start/5, single_line/5, multi_line/5]).
-export([trim_whitespace/2, preserve_line/5, start_of_next_line/1]).
-export([toggle_tag/3, exclusive_insert_tag/3]).
-export([parse_link/2, parse_link_text/2, parse_link_remainder/2]).
-export([test/0]).

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
     {"    this is a\n    test\n    yep\n>> and this\n>> is too", <<"<pre>this is a\ntestyep</pre><blockquote>and this is too</blockquote>">>},
     {"this is a test\nand another\nand *another*\n", <<"<p>this is a test and another and <em>another</em></p>">>},
     {"    test\n    this\n    out\n\n **yep**", <<"<pre>test\nthisout</pre><p> <strong>yep</strong></p>">>},
     {"this is *a test*\n\n    import test\n    import test2\nstill a **test**\n", <<"<p>this is <em>a test</em></p><pre>import test\nimport test2</pre><p>still a <strong>test</strong></p>">>},
     {"test\nthis\n\n>> out", <<"<p>test this</p><blockquote>out</blockquote>">>},
     {"test\nthis\n\n>> out\n>> again\n", <<"<p>test this</p><blockquote>out again</blockquote>">>},
     {"test this\n**out**\n\n>> and this\n>> as well\n\n    woo\n    hoo\n", <<"<p>test this <strong>out</strong></p><blockquote>and this as well</blockquote><pre>woo\nhoo</pre>">>}
    ].



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

tag_modifier(_Binary, []) ->
    0;
tag_modifier(Binary, [Element | _MultiContext]) ->
    Start = start_of_next_line(Binary),
    ?DEBUG_LOGGER("start_of_next_line: ~p~nelement: ~p~n", [Start, Element]),
    case {start_of_next_line(Binary), Element} of
	{<<">> ", _/binary>>, <<"blockquote">>} ->
	    ?DEBUG_LOGGER(">> ~n",[]),
	    -1;
	{<<"> ", _/binary>>, <<"blockquote">>} ->
	    ?DEBUG_LOGGER("> ~n",[]),
	    -1;
	{<<"    ", _/binary>>, <<"blockquote">>} ->
	    ?DEBUG_LOGGER("pre ~n",[]),
	    1;
	{<<"\n", _/binary>>, <<"blockquote">>} ->
	    ?DEBUG_LOGGER("newline~n",[]),
	    -1;
	{<<_Char:1/binary,_/binary>>, <<"blockquote">>} ->
	    ?DEBUG_LOGGER("_char~n",[]),
	    -1;
	{<<"">>, <<"blockquote">>} ->
	    -1;
	{<<">> ", _/binary>>, <<"p">>} ->
	    1;
	{<<"    ", _/binary>>, <<"p">>} ->
	    1;
	{<<"* ", _/binary>>, <<"p">>} ->
	    1;
	{<<"- ", _/binary>>, <<"p">>} ->
	    1;
	{<<_/binary>>, <<"p">>} ->
	    -1;
	_ ->
	    0
    end.

%% Manages closing multi-line entities.
line_start(<<Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    ?DEBUG_LOGGER("line_start: ~p~n",[Binary]),
    {Trimmed, Offset} = trim_whitespace(Binary, 4 * length(MultiContext)),
    IndentLevel = erlang:trunc(Offset / 4),   
    TagsModifier = tag_modifier(Binary, MultiContext),
    QtyTagsToClose = erlang:max(length(MultiContext) - IndentLevel + TagsModifier,0),
    ?DEBUG_LOGGER("TagsToClose = (multicontext) ~p - (indentlevel) ~p + (tagsmodifier) ~p~n", [length(MultiContext), IndentLevel, TagsModifier]),
    Acc3 = lists:foldr(fun(Tag, Acc2) ->
			       [<<"</",Tag/binary,">">> | Acc2]
		       end, Acc, lists:sublist(MultiContext, QtyTagsToClose)),
    ?DEBUG_LOGGER("multi_context ~p => close ~p~n",[MultiContext, lists:sublist(MultiContext, QtyTagsToClose)]),
    MultiContext2 = case (QtyTagsToClose > length(MultiContext)) of
			false ->
			    lists:nthtail(QtyTagsToClose, MultiContext);
			true ->
			    []
		    end,
    ?DEBUG_LOGGER("~n",[]),
    multi_line(Trimmed, OpenTags, Acc3, LinkContext, MultiContext2).

%% Manage pre blocks
multi_line(<<"    ", Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    % if 4 extra spaces, and not in pre, start a pre
    case lists:member(<<"pre">>, MultiContext) of
	true ->	    
	    preserve_line(Binary, OpenTags, Acc, LinkContext, MultiContext);
	false ->
	    preserve_line(Binary, OpenTags, [<<"<pre>">> | Acc], LinkContext, [<<"pre">> | MultiContext])
    end;

%% Manage unordered blocks
multi_line(<<"* ", Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    single_line(Binary, OpenTags, [<<"<li>">> | Acc], LinkContext, [<<"li">> | MultiContext]);

%% Manage unordered blocks
multi_line(<<"- ", Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    single_line(Binary, OpenTags, [<<"<li>">> | Acc], LinkContext, [<<"li">> | MultiContext]);

%% Manage quote blocks
multi_line(<<">> ", Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    case lists:member(<<"blockquote">>, MultiContext) of
	true ->
	    single_line(Binary, OpenTags, [<<" ">> | Acc], LinkContext, MultiContext);
	false ->
	    single_line(Binary, OpenTags, [<<"<blockquote>">> | Acc], LinkContext, [<<"blockquote">> | MultiContext])
    end;

%% Manage quote blocks
multi_line(<<"> ", Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    case lists:member(<<"blockquote">>, MultiContext) of
	true ->
	    single_line(Binary, OpenTags, [<<" ">> | Acc], LinkContext, MultiContext);
	false ->
	    single_line(Binary, OpenTags, [<<"<blockquote>">> | Acc], LinkContext, [<<"blockquote">> | MultiContext])
    end;

%% Manage paragraphs. Special case to avoid empty paragraph on final newline.
multi_line(<<"">>, OpenTags, Acc, LinkContext, []) ->
    single_line(<<"">>, OpenTags, Acc, LinkContext, []);
multi_line(<<"\n", Binary/binary>>, OpenTags, Acc, LinkContext, []) ->
    single_line(<<"\n", Binary/binary>>, OpenTags, Acc, LinkContext, []);
%% Manage paragraphs.
multi_line(<<Binary/binary>>, OpenTags, Acc, LinkContext, []) ->
    single_line(Binary, OpenTags, [<<"<p>">> | Acc], LinkContext, [<<"p">>]);

multi_line(<<"">>, OpenTags, Acc, LinkContext, MultiContext) ->
    single_line(<<"">>, OpenTags, Acc, LinkContext, MultiContext);

%% Manage ordered lists and default..
multi_line(<<Binary/binary>>, OpenTags, Acc, LinkContext, MultiContext) ->
    % insert space for multi-line paragraphs
    Acc2 = case lists:member(<<"p">>, MultiContext) of
	       true ->
		   [<<" ">> | Acc];
	       false ->
		   Acc
	   end,

    %single_line(Binary, OpenTags, [<<"<li>">> | Acc], LinkContext, [<<"li">>, MultiContext]);
    % ordered lists, paragraphs..
    single_line(Binary, OpenTags, Acc2, LinkContext, MultiContext).



%%
%% Single Line Entities (headers, em, strong, links, code)
%%

%% Wrapup function, called at end of document.
single_line(<<"">>, OpenTags, Acc, _LinkContext, MultiContext) ->
    ClosedTags = lists:foldr(fun(Tag, Acc2) ->
				     [<<"</",Tag/binary,">">> | Acc2]
			     end, Acc, lists:append([OpenTags, MultiContext])),
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
