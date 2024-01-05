HS_SRC_DIR = app src test
HS_O = --fast

POETRYPATH = ml/synthesis
PYTHONPATH = ml/synthesis/src
POETRY = poetry -C $(POETRYPATH)
PYTHON = PYTHONPATH=$(PYTHONPATH) $(POETRY) run python3
ML_MODEL_PATH = ml/synthesis/models
ML_MODEL = $(shell ls ml/synthesis/models -t | grep model | head -n 1)

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
	stack build --ghc-options="-O2" nitta:nitta

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
	$(PYTHON) $(PYTHONPATH)/scripts/crawl_data_by_tree_sampling_many.py

ml-train-model:
	$(PYTHON) $(PYTHONPATH)/scripts/train_model.py

ml-format:
	$(POETRY) run black $(PYTHONPATH)

ml-format-check:
	$(POETRY) run black --check --diff $(PYTHONPATH)

ml-lint:
	$(POETRY) run ruff $(PYTHONPATH)
	$(POETRY) run mypy --config-file ml/synthesis/pyproject.toml $(PYTHONPATH)
	cd $(POETRYPATH) && poetry run vulture

ml-nitta:
	echo $(ML_MODEL)
	$(POETRY) shell
	MODELS_DIR=$(ML_MODEL_PATH) PYTHONPATH=$(PYTHONPATH) stack exec nitta -- examples/teacup.lua -s $(ML_MODEL) -p=8080 -d=1.2
