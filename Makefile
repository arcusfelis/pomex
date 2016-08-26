all: compile

compile: ./rebar deps
	./rebar compile

clean: ./rebar
	./rebar clean

deps: ./rebar
	./rebar get-deps

./rebar:
	rm -rf rebar-dist rebar-dist.tar.gz
	wget https://github.com/rebar/rebar/archive/2.6.1.tar.gz -O rebar-dist.tar.gz || \
		curl -L https://github.com/rebar/rebar/archive/2.6.1.tar.gz -o rebar-dist.tar.gz
	tar zxf rebar-dist.tar.gz
	mv rebar-2.6.1 rebar-dist
	cd rebar-dist && make
	cp rebar-dist/rebar .
	rm -rf rebar-dist rebar-dist.tar.gz
