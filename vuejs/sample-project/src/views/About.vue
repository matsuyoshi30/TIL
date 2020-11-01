<template>
  <div class="about">
    <h1>This is an about page</h1>
    <p>{{ profile }}</p>
  </div>
</template>

<script>
import { mapState } from "vuex"

export default {
    mounted() {
        this.$store.dispatch("fetchGitHubProfile", {user_id: "octcat"})
    },
    computed: {
        ...mapState(["profile"])
    }
}
</script>
