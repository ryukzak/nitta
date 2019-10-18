all: stack.build stack.gen npm.build

dev: stack.build stack.gen stack.web npm.dev

prod: stack.prod stack.gen npm.build

build: stack.buildNdocs

clean: stack.kill
	rm -rf .stack-work
	rm -rf web/build




npm.build:
	cd web && npm run build

npm.dev: 
	cd web && npm run start-dev

stack.prod:
	stack build --test --haddock --copy-bins

stack.build:
	stack build

stack.buildNdocs:
	stack build  --haddock

stack.gen: stack.build
	stack exec nitta-api-gen

stack.web: stack.build
	if [ -z "${sim}" ]; \
	then \
		stack exec nitta -- --web examples/fibonacci.lua & \
	else \
		stack exec nitta -- --web examples/${sim}.lua & \
	fi;

stack.kill:
	pkill -f nitta 
