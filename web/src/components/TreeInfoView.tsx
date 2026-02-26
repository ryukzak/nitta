import { JsonView } from "components/JsonView";
import { MapHistogram } from "components/utils/MapHistogram";
import { RequestResult } from "components/utils/RequestResult";
import { useApiRequest } from "hooks/useApiRequest";
import { type FC, useCallback } from "react";
import { Col, Container, Row } from "react-bootstrap";
import { api } from "services/HaskellApiService";
import { CHART_COLOR_PALLETE } from "utils/color";
import "components/Graphviz.scss";

export type ITreeInfoViewProps = {};

export const TreeInfoView: FC<ITreeInfoViewProps> = (_props) => {
  const treeInfoRequest = useApiRequest({
    requester: useCallback(() => {
      return api.getTreeInfo();
      // getTreeInfo result depends on selectedSid on server side, thus need to re-request the result when it's changed
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, []),
  });

  return (
    <RequestResult
      result={treeInfoRequest.response}
      resultRenderer={(result) => (
        <Container fluid>
          <Row>
            <Col sm={7} md={6} lg={5}>
              <JsonView value={result.data} />
            </Col>
            <Col sm={5} md={5} lg={4}>
              <Row>
                <MapHistogram
                  data={result.data.targetProcessDuration}
                  color={CHART_COLOR_PALLETE.blue}
                  name="success nodes with duration"
                />
              </Row>
              <Row>
                <MapHistogram
                  data={result.data.synthesisStepsForSuccess}
                  color={CHART_COLOR_PALLETE.orange}
                  name="success nodes with steps"
                />
              </Row>
            </Col>
          </Row>
        </Container>
      )}
      noResultRenderer={treeInfoRequest}
    />
  );
};
