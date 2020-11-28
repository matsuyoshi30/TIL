import React from 'react'
import ReactDOM from 'react-dom'
import Form from './form'

export default class App extends React.Component {
  render() {
    return (
      <div>
        <Form />
      </div>
    );
  }
}

ReactDOM.render(<App />, document.getElementById('app'))
