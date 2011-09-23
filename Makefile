RUN := +P 99999999 +Bc +K true -smp enable -pa ebin deps/*/ebin -s crypto -s inets -s ssl -s elog ${ERLARGS} 

all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	dialyzer -pa deps/*/ebin --apps erts kernel stdlib inets --output_plt ~/.match_stream_dialyzer_plt --build_plt

analyze: all
	dialyzer -pa deps/*/ebin --plt ~/.match_stream_dialyzer_plt -Wunmatched_returns -Werror_handling -Wrace_conditions ebin

update-deps:
	rebar update-deps

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref
	
run: all
	erl  -boot start_sasl ${RUN} -s match_stream

shell: all
	erl  -boot start_sasl ${RUN}

test: all
	erl -noshell -noinput ${RUN} -run match_stream_tests main
