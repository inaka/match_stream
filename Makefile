RUN := +Bc +K true -smp enable -pa ebin deps/*/ebin -s crypto -s inets -s ssl -s elog

all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	dialyzer -pa deps/*/ebin --apps erts kernel stdlib inets --output_plt ~/.match_stream_dialyzer_plt --build_plt

analyze: all
	dialyzer -pa deps/*/ebin --plt ~/.match_stream_dialyzer_plt -Wunmatched_returns -Werror_handling -Wbehaviours ebin

update-deps:
	rebar update-deps

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref
	
run: all
	if [ -f app.config ]; then\
		erl  -name match_stream -config app -boot start_sasl ${RUN} -s match_stream;\
	else\
		erl  -name match_stream -boot start_sasl ${RUN} -s match_stream;\
	fi

shell: all
	if [ -f app.config ]; then\
		erl  -config app -boot start_sasl ${RUN};\
	else\
		erl  -boot start_sasl ${RUN};\
	fi

test: all
	if [ -f app.config ]; then\
		erl -noshell -noinput -config app ${RUN} -run match_stream_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run match_stream_tests main;\
	fi
