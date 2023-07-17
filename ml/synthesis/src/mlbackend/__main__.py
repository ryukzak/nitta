import uvicorn

from components.common.logging import configure_logging, get_logger
from consts import ML_BACKEND_BASE_URL_FILEPATH
from mlbackend.backend_base_url_file import BackendBaseUrlFile

logger = get_logger(__name__)
configure_logging()

with BackendBaseUrlFile(
    filepath=ML_BACKEND_BASE_URL_FILEPATH, base_url_fmt="http://127.0.0.1:{port}"
) as base_url_file:
    assert base_url_file.port is not None
    logger.info(f"Starting ML backend server on port {base_url_file.port}")
    uvicorn.run(
        "mlbackend.app:app", host="127.0.0.1", port=base_url_file.port, workers=4
    )
