<template>
    <div class="counter">
        <p>{{ name }}</p>
        <p>Count: {{ count }}</p>
        <button @click="increment">+1</button>
    </div>
</template>

<script>

export default {
    props: {
        name: String,
        initCount: Number
    },
    data() {
        return {
            count: this.initCount
        }
    },
    method: {
        increment() {
            this.count++;
        }
    }
}

</script>

<style scoped>
.counter {
    width: 200px;
}
</style>
