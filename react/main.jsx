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