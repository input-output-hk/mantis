/** @jsx React.DOM */

web3 = new Web3(new Web3.providers.HttpProvider("http://127.0.0.1:8546"));

var abi = [ { "constant": false, "inputs": [ { "name": "thread", "type": "uint256" }, { "name": "content", "type": "string" } ], "name": "newMessage", "outputs": [], "payable": false, "type": "function" }, { "anonymous": false, "inputs": [ { "indexed": true, "name": "thread", "type": "uint256" }, { "indexed": false, "name": "content", "type": "string" } ], "name": "NewMessage", "type": "event" } ];

var contract = web3.eth.contract(abi).at("0xB351AE07713E41f437Dfed42C8B7648c717c8914");

const Message = React.createClass({
  getInitialState: function() {
    return {
      block: null,
      transaction: null
    }
  },
  componentDidMount: function() {
    web3.eth.getBlock(this.props.event.blockHash, (function(error, block) {
      if (error) {
        alert("Error: " + error);
      } else {
        this.setState({
          block: block
        });
      }
    }).bind(this));

    web3.eth.getTransaction(this.props.event.transactionHash, (function(error, transaction) {
      if (error) {
        alert("Error: " + error);
      } else {
        this.setState({
          transaction: transaction
        });
      }
    }).bind(this));
  },
  render: function() {
    var ev = this.props.event,
        block = this.state.block,
        tx = this.state.transaction;
    if (!block || !tx) {
      return (<li>Loading...</li>);
    } else {
      return (
        <li>
          <em>{ev.args.content}</em><br />
          <small>Thread: {ev.args.thread.toString()}</small><br />
          <small>Timestamp: {block.timestamp}</small><br />
          <small>Author: {tx.from}</small><br /><br />
        </li>
      );
    }
  }
});

const Container = React.createClass({
  getInitialState: function() {
    return {
      events: [],
      filterInstance: null
    };
  },
  componentDidMount: function() {
    this.restartWatch(null);
  },
  restartWatch: function(threadFilter) {
    if (this.state.filterInstance) {
      this.state.filterInstance.stopWatching();
    }

    var opts = threadFilter ? {'thread': threadFilter} : {};
    var filterInstance = contract.NewMessage(opts, {fromBlock: 0}).watch((error, result) => {
      if (error) {
        alert('Error: ' + error);
      } else {
        this.setState({
          events: this.state.events.concat([result])
        });
      }
    });

    this.setState({
      filterInstance: filterInstance,
      events: []
    });
  },
  handleFilterChange: function(event) {
    this.restartWatch(event.target.value);
  },
  render: function() {
    return (
      <div>
        <h3>Messages:</h3><br />
        <span>Thread: <input type="number" onChange={this.handleFilterChange} /></span>
        {this.state.events.length == 0 ? (
          <p>No messages to show</p>
        ) : (
          <ul>
            {this.state.events.map((ev) => <Message event={ev} />)}
          </ul>
        )}
      </div>
    );
  }
});

ReactDOM.render(
  <Container />,
  document.getElementById('container'));
