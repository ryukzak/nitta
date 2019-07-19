import * as React from "react";
import { haskellAPI, Timelines } from "../middleware/haskell-api";

interface ProcessViewProps {
    nId: string;
}

interface ProcessViewState {
    timelines: any;
    nId: string;
}

export class ProcessView extends React.Component<ProcessViewProps, ProcessViewState> {
    state: ProcessViewState = { nId: null, timelines: null }

    static getDerivedStateFromProps(props: ProcessViewProps, state: ProcessViewState) {
        console.log('> ProcessView.getDerivedStateFromProps', props.nId && props.nId != state.nId)
        if (props.nId && props.nId != state.nId) {
            console.log('> ProcessView.getDerivedStateFromProps - new state')
            return { nId: props.nId, timelines: null } as ProcessViewState
        }
        return null
    }

    componentDidMount() {
        console.log('> ProcessView.componentDidMount', this.state.nId)
        this.requestTimelines(this.state.nId)
    }

    componentDidUpdate(prevProps: ProcessViewProps, prevState: ProcessViewState, snapshot: any) {
        console.log('> ProcessView.componentDidUpdate')
        if (prevState.nId != this.state.nId) {
            this.requestTimelines(this.state.nId)
        }
    }

    requestTimelines(nId: string) {
        console.log('> ProcessView.requestTimelines')
        haskellAPI.getTimelines(nId)
            .then((response: Timelines) => {
                console.log('> ProcessView.requestTimelines - done')
                this.setState({
                    timelines: response.data
                });
            })
            .catch((err: any) => console.log(err));
    }

    render() {
        return <pre>{JSON.stringify(this.state.timelines, null, 2)}</pre>;
    }
}
