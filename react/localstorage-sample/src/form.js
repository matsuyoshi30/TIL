import React from 'react'

export default class Form extends React.Component {
  constructor(props) {
    super(props)

    const initList = []
    for (let key in localStorage) {
      if (localStorage.hasOwnProperty(key)) {
        const item = {
          key: key,
          val: localStorage.getItem(key)
        }
        initList.push(item)
      }
    }

    this.state = {
      key: '',
      val: '',
      list: initList
    }

    this.handleSubmit = this.handleSubmit.bind(this)
    this.handleKeyChange = this.handleKeyChange.bind(this)
    this.handleValChange = this.handleValChange.bind(this)
  }

  handleSubmit = event => {
    if (this.state.key && this.state.val) {
      localStorage.setItem(this.state.key, this.state.val)

      const item = {
        key: this.state.key,
        val: this.state.val
      }
      this.setState({list: [...this.state.list, item]})
    }

    this.setState({key: ''})
    this.setState({val: ''})
    event.preventDefault()
  }

  handleKeyChange = event => {
    this.setState({key: event.target.value})
  }

  handleValChange = event => {
    this.setState({val: event.target.value})
  }

  render() {
    return (
      <div>
        <form onSubmit={this.handleSubmit}>
          <label>
            <input type="text" value={this.state.key} onChange={this.handleKeyChange} />
            <input type="text" value={this.state.val} onChange={this.handleValChange} />
          </label>
          <input type="submit" value="Submit" />
        </form>

        <ul>
          {this.state.list.map((elem, i) => (<li key={i}>{elem.key}: {elem.val}</li>))}
        </ul>
      </div>
    );
  }
}
