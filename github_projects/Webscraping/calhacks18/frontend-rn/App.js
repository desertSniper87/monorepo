import React from 'react';
import { createStore } from 'redux';
import { Provider } from 'react-redux';

import { reducer } from './reducer.js';
import Main from './Main.js';

const store = createStore(reducer);

export default class App extends React.Component {
    render() {
        return (
            <Provider store={store}>
                <Main />
            </Provider>
        );
    }
}
