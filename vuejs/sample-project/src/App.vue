<template>
    <div id="app">
        <img alt="Vue logo" src="./assets/logo.png">
        <HelloWorld msg="Welcome to Your Vue.js App"/>
        <Counter name="Counter 1" :initCount="5" @emitUp="getEvent" />
        <Counter name="Counter 2" :initCount="10" @emitUp="getEvent" />
        <p>
            EventStack: {{ stack }}
        </p>
    </div>
</template>

<script>
import HelloWorld from './components/HelloWorld.vue'
import Counter from './components/Counter.vue'

export default {
    name: 'App',
    components: {
        HelloWorld,
        Counter
    },
    data() {
        return {
            stack: []
        }
    },
    methods: {
        getEvent(payload) {
            this.stack.push(payload)
        }
    }
}
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
