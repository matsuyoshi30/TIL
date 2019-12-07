function App() {
  return <div><h1>Hello React!</h1></div>;
}
const target = document.querySelector('#app');
ReactDOM.render(<App/>, target);

function tick() {
  const element = (
    <div>
      <h2>It is {new Date().toLocaleTimeString()}.</h2>
    </div>
  );
  ReactDOM.render(element, document.getElementById('root'));
}
setInterval(tick, 1000);

function Welcome(props) {
  return <h1>Hello, {props.name}</h1>;
}
const element = <Welcome name="Sara" />;
ReactDOM.render(
  element,
  document.getElementById('propsample')
);

function Welcome2(props) {
  return <h1>Hello, {props.name}</h1>;
}
function App2() {
  return (
    <div>
      <Welcome2 name="Sara" />
      <Welcome2 name="Cahal" />
      <Welcome2 name="Edite" />
    </div>
  );
}
ReactDOM.render(
  <App2 />,
  document.getElementById('propsample2')
);