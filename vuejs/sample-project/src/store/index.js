import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
    globalCount: 0
  },
  mutations: {
    globalIncrement(state) {
      state.globalCount++
    }
  },
  actions: {
  },
  modules: {
  }
})
