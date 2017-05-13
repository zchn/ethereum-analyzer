'use strict';

import React from 'react';
import ReactDOM from 'react-dom';
// import EAClient from './ea-client.js';

class App extends React.Component {
    constructor(props) {
        super(props);
        this.state = {ea: props.ea};
    }
    callDotCfg: (hexstring, onSuccess, onError) => {
        fetch("/ea/dotcfg2?code="+hexstring).then((resp) => {
            resp.json().then((jsonn) => {
                onSuccess(jsonn);
            }, (reason) => {
                onError(reason);
            });
        }, (reason) => {
            onError(reason);
        });
    }
    handleNewCmd: (cmd) => {
        if(cmd.startsWith("disasm ")) {
            var parts = cmd.split(" ");
            console.log(parts);
            for(var i = 1; i < parts.length; i++) {
                if(parts[i].length > 0) {
                    this.callDotCfg(
                        parts[i],
                        (cfgs) => {
                            console.log(cfgs);
                            this.setState({ea: { cfgs: cfgs }});
                        },
                        (error) => {
                            console.log(error);
                        });
                    break;
                }
            }
        } else {
            console.log("Unknown command: " + cmd);
        }
    }
    render() {
        return (
            <div id="ea-app">
              <MenuBar />
              <LineNavBar />
              <MainBody cfgs={this.state.ea.cfgs} />
              <CmdWindow onNewCmd={this.handleNewCmd} />
            </div>
        );
    }
}

class MenuBar extends React.Component {
    render() {
        return (
            <div id="ea-menu-bar">There will be a menu here</div>
        );
    }
}

class LineNavBar extends React.Component {
    render() {
        return (
            <div id="ea-line-nav-bar">||CCCC-----=========================</div>
        );
    }
}

class MainBody extends React.Component {
    render() {
        return (
            <div id="ea-main-body">
              <SideNav />
              <MainWindow cfgs={this.props.cfgs} />
            </div>
        );
    }
}

class CmdWindow extends React.Component {
    constructor(props) {
        super(props);

        this.state = { cmdHistory: [] };

        this.handleNewCmd = this.handleNewCmd.bind(this);
    }

    handleNewCmd(cmd) {
        this.props.onNewCmd(cmd);
        this.setState({ cmdHistory: this.state.cmdHistory.concat([ cmd ])});
    }

    render() {
        return (
            <div id="ea-cmd-window">
              <CmdOutput cmdHistory={this.state.cmdHistory} />
              <CmdInput onNewCmd={this.handleNewCmd} />
            </div>
        );
    }
}

class CmdOutput extends React.Component {
    render() {
        // console.log(this.props.cmdHistory);
        var rows = [];
        for(var i = 0; i < this.props.cmdHistory.length; i++) {
            rows.push(<li key={i}>{this.props.cmdHistory[i]}</li>);
        }
        return (
            <ul id="ea-cmd-output" >
              {rows}
            </ul>
        );
    }
}

class CmdInput extends React.Component {
    constructor(props) {
        super(props);

        this.state = { cmd: "" };

        this.handleChange = this.handleChange.bind(this);
        this.handleEnter = this.handleEnter.bind(this);
    }

    handleChange(e) {
        this.setState({ cmd: e.target.value });
    }

    handleEnter(e) {
        if (e.charCode == 13 || e.keyCode == 13) {
            e.preventDefault();
            this.props.onNewCmd(this.state.cmd);
            this.setState({ cmd: "" });
        }
    }

    render() {
        return (
            <input type="text"
                   id="ea-cmd-input"
                   value={this.state.cmd}
                   onChange={this.handleChange}
                   onKeyPress={this.handleEnter} />
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
        console.log(this.props.cfgs);
        var ctorDot = this.props.cfgs._ctorDot;
        var ctorCfg = ctorDot.length > 20 ? Viz(ctorDot) : ctorDot;

        var disDot = this.props.cfgs._dispatcherDot;
        var disCfg = disDot.length > 20 ? Viz(disCfg) : disDot;
        return (
            <div id="ea-main-window">
              {ctorCfg}
              {disCfg}
            </div>
        );
    }
}

var EA_STATE = {
    cfgs: {
        _ctorDot: "",
        _dispatcherDot: ""
    }
};

ReactDOM.render(
    <App ea={EA_STATE} />,
    document.getElementById('root')
);
