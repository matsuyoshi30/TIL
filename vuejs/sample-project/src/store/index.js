import Vue from 'vue'
import Vuex from 'vuex'

Vue.use(Vuex)

export default new Vuex.Store({
  state: {
    profile: null,
    globalCount: 0,
  },
  mutations: {
    globalIncrement(state) {
      state.globalCount++
    },
    setGitHubProfile(state, payload) {
      state.profile = payload;
    },
  },
  actions: {
    async fetchGitHubProfile(store, payload) {
      if (store.state.profile !== null) return;

      const response = await fetch(
        `https://api.github.com/users/${payload.user_id}`
      ).then((res) => {
        return res.json();
      });
      store.commit("setGitHubProfile", response);
    },
  },
  modules: {
  }
})
