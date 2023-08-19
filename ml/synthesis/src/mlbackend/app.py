from __future__ import annotations

from http import HTTPStatus

import pandas as pd
from fastapi import FastAPI, HTTPException
from fastapi.exception_handlers import http_exception_handler
from starlette.responses import HTMLResponse

from components.data_crawling.node_processing import nitta_node_to_df_dict
from components.data_processing.feature_engineering import (
    df_to_model_columns,
    preprocess_train_data_df,
)
from mlbackend.dtos import (
    ModelInfo,
    PostScoreRequestBody,
    PostScoreResponseData,
    Response,
)
from mlbackend.models_store import ModelNotFoundError, models

app = FastAPI(
    title="NITTA ML Backend",
    version="0.0.0",
    # stock doc UIs are replaced by rapidoc below (it's arguably better)
    docs_url=None,
    redoc_url=None,
)


@app.get("/models/{model_name}")
def get_model_info(model_name: str) -> Response[ModelInfo]:
    model, meta = models[model_name]
    return Response(
        data=ModelInfo(
            name=model_name,
            train_mae=meta.train_mae,
            validation_mae=meta.validation_mae,
        )
    )


@app.post("/models/{model_name}/score")
def score_with_model(model_name: str, body: PostScoreRequestBody) -> Response[PostScoreResponseData]:
    """Runs score prediction with model of given name for each input in a given list of inputs."""
    model, meta = models[model_name]

    scores = []

    for inp in body.inputs:
        all_siblings = tuple(inp.nodes)
        siblings, target_nodes = [], []
        for node in all_siblings:
            if inp.scoring_target == "all" or inp.scoring_target == node.sid:
                target_nodes.append(node)
            else:
                siblings.append(node)

        if not target_nodes:
            raise HTTPException(
                status_code=HTTPStatus.BAD_REQUEST,
                detail="No target node(s) were found",
            )

        df = pd.DataFrame([nitta_node_to_df_dict(target_node, siblings=all_siblings) for target_node in target_nodes])
        df = preprocess_train_data_df(df)
        df = df_to_model_columns(df, model_columns=meta.input_columns)
        scores.append(model.predict(df.values).reshape(-1).tolist())

    return Response(
        data=PostScoreResponseData(
            scores=scores,
        )
    )


@app.exception_handler(ModelNotFoundError)
async def model_not_found_exception_handler(request, exc):
    return await http_exception_handler(request, HTTPException(status_code=HTTPStatus.NOT_FOUND, detail=str(exc)))


@app.get("/docs", response_class=HTMLResponse, include_in_schema=False)
def get_rapidoc_docs():
    return f"""
        <!doctype html>
        <html>
            <head>
                <meta charset="utf-8">
                <script
                    type="module"
                    src="https://unpkg.com/rapidoc/dist/rapidoc-min.js"
                ></script>
            </head>
            <body>
                <rapi-doc
                    spec-url="{app.openapi_url}"
                    render-style="read"
                    show-header="false"
                    default-schema-tab="schema"
                    theme="light"
                ></rapi-doc>
            </body>
        </html>
    """
