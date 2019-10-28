all: stack.build stack.gen npm.build

prod: stack.prod stack.gen npm.build

build: stack.haddock

clean:
	stack clean
	rm -rf web/build

debs: stack.install npm.install




npm.build:
	cd web && npm run build

npm.dev: 
	cd web && npm run start

npm.install: 
	cd web && npm install

stack.install:
	stack build --dry-run

stack.prod:
	stack build --test --copy-bins

stack.build:
	stack build

stack.haddock:
	stack build  --haddock

stack.gen: stack.build
	stack exec nitta-api-gen

stack.web: stack.build
	if [ -z "${sim}" ]; \
	then \
		stack exec nitta -- --web examples/fibonacci.lua \
	else \
		stack exec nitta -- --web examples/${sim}.lua \
	fi;
