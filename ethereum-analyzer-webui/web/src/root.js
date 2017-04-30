'use strict';

import React from 'react';
import ReactDOM from 'react-dom';

class App extends React.Component {
    constructor(props) {
        super(props);
        this.state = {ea: props.ea};
    }
    render() {
        return (
            <div id="ea-app">
              <MenuBar />
              <LineNavBar />
              <MainBody />
              <CmdWindow />
            </div>
        );
    }
}

class MenuBar extends React.Component {
    render() {
        return <div id="ea-menu-bar">There will be a menu here</div>;
    }
}

class LineNavBar extends React.Component {
    render() {
        return <div id="ea-line-nav-bar">||CCCCCCCCCC|DDDD||</div>;
    }
}

class MainBody extends React.Component {
    render() {
        return (
            <div id="ea-main-body">
              <SideNav />
              <MainWindow />
            </div>
        );
    }
}

class CmdWindow extends React.Component {
    render() {
        return (
            <div id="ea-cmd-window">
              <CmdOutput />
              <CmdInput />
            </div>
        );
    }
}

class CmdOutput extends React.Component {
    render() {
        return (
            <div id="ea-cmd-output">
              command input and output history <br />
              command input and output history <br />
              command input and output history <br />
              command input and output history <br />
              command input and output history <br />
              command input and output history <br />
            </div>
        );
    }
}

class CmdInput extends React.Component {
    render() {
        return (
            <div id="ea-cmd-input">cmd input</div>
        );
    }
}

class SideNav extends React.Component {
    render() {
        return (
            <div id="ea-side-nav">
              sidenav
            </div>
        );
    }
}

class MainWindow extends React.Component {
    render() {
        return (
            <div id="ea-main-window">
              main window
            </div>
        );
    }
}

var EA_STATE = {
    name: 'mock state',
};

ReactDOM.render(
    <App ea={EA_STATE} />,
    document.getElementById('root')
);
