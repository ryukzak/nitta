from dataclasses import dataclass, field
from typing import Optional, Any, List

from cachetools import cached
from dataclasses_json import LetterCase, dataclass_json

nitta_dataclass_params = dict(letter_case=LetterCase.CAMEL)


@dataclass_json(**nitta_dataclass_params)
@dataclass
class NittaNodeDecision:
    tag: str


@dataclass_json(**nitta_dataclass_params)
@dataclass
class NittaNode:
    score: Optional[int]
    is_terminal: bool
    is_finish: bool
    parameters: Any
    decision: NittaNodeDecision
    duration: Optional[int]
    sid: str

    children: Optional[List['NittaNode']] = field(default=None, repr=False)
    parent: Optional['NittaNode'] = field(default=None, repr=False)

    def __hash__(self):
        return hash(self.sid)
