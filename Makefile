HS_SRC_DIR = app src test
HS_O = --fast

POETRYPATH = ml/synthesis
PYTHONPATH = ml/synthesis/src
POETRY = poetry -C $(POETRYPATH)
PYTHON = PYTHONPATH=$(PYTHONPATH) $(POETRY) run python3

ML_CRAWL_DATA_PATH = ml/synthesis/data
ML_MODEL_PATH = ml/synthesis/models
ML_MODEL = $(shell ls -t $(ML_MODEL_PATH) | grep model | head -n 1)

PLATFORM := $(shell uname -s)

.PHONY: all build run test format clean

all: format build test lint

############################################################
## nitta
############################################################

build:
	stack build --fast --keep-going \
		--haddock --no-haddock-deps \
		--copy-bins --coverage \
		--test --no-run-tests

build-prod:
	stack build --ghc-options="-O2"

test:
	stack build --coverage --test $(HS_O)
	find src -name '*.hs' -exec grep -l '>>>' {} \; | xargs -t -L 1 -P 4 stack exec doctest

format:
	fourmolu -m inplace $(HS_SRC_DIR)

format-check:
	fourmolu -m check $(HS_SRC_DIR)

lint:
	hlint $(HS_SRC_DIR)
	stack exec weeder

clean:
	stack clean

benchmark:
	$(PYTHON) $(PYTHONPATH)/scripts/evaluate_nitta_synthesis.py \
		ml/synthesis/src/scripts/evaluation_configs/demo.json

benchmark-report:
	$(PYTHON) $(PYTHONPATH)/scripts/compare_evaluations.py \
		evaluation/*.csv

############################################################
## nitta ui
############################################################

ui-build:
	stack exec nitta-api-gen -- -v
	yarn --cwd web run build

ui-run:
	stack exec nitta-api-gen -- -v
	yarn --cwd web start

ui-format:
	yarn --cwd web exec -s prettier -- --write 'web/src/**/*.{ts,tsx}' --ignore-path web/.gitignore

ui-format-check:
	yarn --cwd web exec -s prettier -- --check 'web/src/**/*.{ts,tsx}' --ignore-path web/.gitignore

############################################################
## nitta ml
############################################################

ml-crawl-data:
	$(PYTHON) -m scripts.crawl_data_by_tree_sampling_many

ml-train-model:
	$(PYTHON) -m scripts.train_model

ml-format:
	$(POETRY) run black $(PYTHONPATH)

ml-format-check:
	$(POETRY) run black --check --diff $(PYTHONPATH)

ml-lint:
	$(POETRY) run ruff $(PYTHONPATH)
	$(POETRY) run mypy --config-file ml/synthesis/pyproject.toml $(PYTHONPATH)
	cd $(POETRYPATH) && poetry run vulture

ml-nitta:
	echo 'Model for Synthesis: ' $(ML_MODEL)
	$(POETRY) shell
	MODELS_DIR=$(ML_MODEL_PATH) PYTHONPATH=$(PYTHONPATH) stack exec nitta -- examples/teacup.lua -s ml_$(ML_MODEL) -p=8080

ml-clean:
	rm -rfv $(ML_CRAWL_DATA_PATH) $(ML_MODEL_PATH)

############################################################
## docker development image
############################################################


docker-dev-build-for-linux-win:
	docker build \
		--target development \
		-f ml/synthesis/Dockerfile \
		--build-arg HOST_UID=$(id -u) \
		--build-arg HOST_GID=$(id -g) \
		-t nitta-dev \
		.

docker-dev-build-for-mac:
	docker build \
		--target development \
		-f ml/synthesis/Dockerfile \
		-t nitta-dev \
		.

docker-dev-build:
	echo Platform: $(PLATFORM)
ifeq ($(PLATFORM),Darwin)
	make docker-dev-build-for-mac
else
	make docker-dev-build-for-linux-win
endif


docker-dev-build-with-gpu-for-linux-win:
	docker build \
		--target development-gpu \
		-f ml/synthesis/Dockerfile \
		--build-arg HOST_UID=$(shell id -u) \
		--build-arg HOST_GID=$(shell id -g) \
		-t nitta-dev \
		.

docker-dev-build-with-gpu:
	echo Platform: $(PLATFORM)
ifeq ($(PLATFORM),Darwin)
	echo "GPU is not supported on Mac, use `docker-dev-build`"
else
	make docker-dev-build-with-gpu-for-linux-win
endif

docker-dev-run:
	docker run \
		--name=nitta-dev-container \
		-p 31032:22 \
		-v="$(PWD):/app" \
		-v="nitta-devuser-home:/home/devuser" \
		-it \
		nitta-dev

docker-dev-run-with-gpu:
	docker run \
		--name=nitta-dev-container \
		--gpus=all \
		-p 31032:22 \
		-v="$(PWD):/app" \
		-v="nitta-devuser-home:/home/devuser" \
		-it \
		nitta-dev
