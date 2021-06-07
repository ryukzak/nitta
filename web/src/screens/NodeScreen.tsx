import React, { FC, useCallback, useContext } from "react";
import "react-table/react-table.css";

import { AppContext, IAppContext } from "app/AppContext";
import { IntermediateView } from "components/IntermediateView";
import { MicroarchitectureView } from "components/MicroarchitectureView";
import { api } from "services/HaskellApiService";
import { JsonView } from "components/JsonView";
import { useApiRequest } from "hooks/useApiRequest";
import { RequestResult } from "components/utils/RequestResult";
import { Col, Container, Row } from "react-bootstrap";
import { SimpleNittaBarChart } from "components/utils/SimpleNittaBarChart";
import { CHART_COLOR_PALLETE } from "utils/color";

export interface INodeScreenProps {}

export const NodeScreen: FC<INodeScreenProps> = (props) => {
  const { selectedSID } = useContext(AppContext) as IAppContext;
  const treeInfoRequest = useApiRequest({
    requester: useCallback(() => {
      return api.getTreeInfo();
      // getTreeInfo result depends on selectedSID on server side, thus need to re-request the result when it's changed
      // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [selectedSID]),
  });

  if (!selectedSID) return <pre> synthesis is not selected </pre>;

  return (
    <div className="m-3">
      <h3>sid:</h3>
      <pre>{selectedSID}</pre>

      <h3>Tree info:</h3>
      <RequestResult
        result={treeInfoRequest.response}
        resultRenderer={(result) => (
          <Container fluid>
            <Row>
              <Col md={4}>
                <JsonView src={result.data} />
              </Col>
              <Col md={6} lg={4}>
                <SimpleNittaBarChart
                  data={result.data.durationSuccess}
                  color={CHART_COLOR_PALLETE.blue}
                  name="success nodes with duration"
                />
                <div className="mt-3">
                  <SimpleNittaBarChart
                    data={result.data.stepsSuccess}
                    color={CHART_COLOR_PALLETE.orange}
                    name="success nodes with steps"
                  />
                </div>
              </Col>
            </Row>
          </Container>
        )}
        noResultRenderer={treeInfoRequest}
      />

      <h3 className="mt-3">Dataflow graph:</h3>
      <IntermediateView />

      <MicroarchitectureView />
    </div>
  );
};
