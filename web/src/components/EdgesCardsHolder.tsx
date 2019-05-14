import * as React from "react";
import { haskellAPI } from "../middleware/haskell-api";
import { EdgesCard } from "./EdgeCard";

/**
 * Component to display EdjeCards of selected nId and it's chidren. 
 * Takes one argument: 
 * nid - id of selected node;
 */

interface EdgesCardsHolderProps {
    nid: string;
}

interface EdgesCardsHolderState {
    selectedNId: string;
    maxECharacteristic: number;
    idsForCard: string[];
    edgesForCard: JSON[];
    isLoad: boolean;
}

export class EdgesCardsHolder extends React.Component<EdgesCardsHolderProps, EdgesCardsHolderState> {
    constructor (props: EdgesCardsHolderProps) {
        super(props);
        this.state = {
            selectedNId: props.nid,
            maxECharacteristic: 0,
            idsForCard: [],
            edgesForCard: [],
            isLoad: false
        };
        this.loadIds(props.nid);
    }

    componentWillReceiveProps(props: EdgesCardsHolderProps) {
        if (this.state.selectedNId !== props.nid) {
            this.setState({
                selectedNId: props.nid,
                isLoad: false,
                maxECharacteristic: 0,
                idsForCard: [],
                edgesForCard: []
            });
            this.loadIds(props.nid);
        }
    }

    loadIds(nid: string) {
        if (nid === undefined || nid === null || nid === "") return;
        let curIds: string[] = [];
        let nIds = {};
        var index = 0;
        let reLastNidStep = /:[^:]*$/;
        let childNid = new RegExp( "^" + nid + ":[0-9]*$");
        haskellAPI.getSynthesis()
            .then((response: JSON) => {
                let buildHolder = (gNode: JSON, dNode: JSON) => {
                    gNode.name = reLastNidStep.exec(dNode[0].svNnid)[0];
                    gNode.nid = dNode[0].svNnid;
                    if (gNode.nid === nid || childNid.test(gNode.nid)) {
                        // curIds[index] = gNode.nid;
                        curIds.push(gNode.nid);
                        index++;
                    }
                    nIds[dNode[0].svNnid] = gNode;
                    gNode.children = [];
                    dNode[1].forEach((e: any) => {
                        let tmp = {};
                        gNode.children.push(tmp);
                        buildHolder(tmp, e);
                    });
                };
                buildHolder({}, response.data);
                this.setState({
                    idsForCard: curIds,
                });
                this.loadEdge(index);
            })
            .catch((err: any) => console.log(err));
    }

    loadEdge(ind: number) {
        let length = this.state.idsForCard.length;
        let maxEChar = 0;
        let curEdges: JSON[] = [];
        let act = this;
        this.state.idsForCard.map(function(id, index){
            haskellAPI.getEdge(id)
            .then((response: JSON) => {
                curEdges.push(response.data);
                if (maxEChar < response.data.eObjectiveFunctionValue) {
                    maxEChar = response.data.eObjectiveFunctionValue;
                }
                if (index === length - 1) {
                        act.setState({
                            edgesForCard: curEdges,
                            maxECharacteristic: maxEChar,
                            isLoad: true,
                        });
                    }
            })
            .catch(err => {console.log(err);} );
        });
    }

    render() {
        let act = this;
        return(
            <div className = "grid-x" >
                { this.state.isLoad && 
                    this.state.edgesForCard.map(function(edge, index) {
                        return (
                            <div className="edgeCardContainer" >
                                { <EdgesCard nid = {act.state.idsForCard[index]} edge = {edge} maxValue={act.state.maxECharacteristic}/> }
                            </div>
                        );
                })}
            </div>
        );
    }
}