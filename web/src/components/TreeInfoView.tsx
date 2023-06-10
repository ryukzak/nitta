import React, { useContext, FC, useCallback } from "react";
import { Col, Container, Row } from "react-bootstrap";

import { AppContext, IAppContext } from "app/AppContext";
import { api } from "services/HaskellApiService";
import { useApiRequest } from "hooks/useApiRequest";
import { RequestResult } from "components/utils/RequestResult";
import { JsonView } from "components/JsonView";
import { MapHistogram } from "components/utils/MapHistogram";
import { CHART_COLOR_PALLETE } from "utils/color";

import "components/Graphviz.scss";
import "react-table/react-table.css";

export interface ITreeInfoViewProps {}

export const TreeInfoView: FC<ITreeInfoViewProps> = (props) => {
  const { selectedSid } = useContext(AppContext) as IAppContext;

  const treeInfoRequest = useApiRequest({
    requester: useCallback(() => {
      return api.getTreeInfo();
      // getTreeInfo result depends on selectedSid on server side, thus need to re-request the result when it's changed
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [selectedSid]),
  });

  return (
    <RequestResult
      result={treeInfoRequest.response}
      resultRenderer={(result) => (
        <Container fluid>
          <Row>
            <Col md={4}>
              <JsonView src={result.data} />
            </Col>
            <Col md={4} lg={3}>
              <MapHistogram
                data={result.data.targetProcessDuration}
                color={CHART_COLOR_PALLETE.blue}
                name="success nodes with duration"
              />
            </Col>
            <Col md={4} lg={3}>
              <MapHistogram
                data={result.data.synthesisStepsForSuccess}
                color={CHART_COLOR_PALLETE.orange}
                name="success nodes with steps"
              />
            </Col>
          </Row>
        </Container>
      )}
      noResultRenderer={treeInfoRequest}
    />
  );
};
