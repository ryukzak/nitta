import * as React from 'react';
import * as ReactDOM from 'react-dom';
import * as TestUtils from 'react-dom/test-utils';
import App from '../src/components/App';

it('renders without crashing', () => {
    const div = document.createElement('div');
    ReactDOM.render(<App />, div);
    ReactDOM.unmountComponentAtNode(div);
});