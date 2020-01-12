package main

import (
	"syscall/js"
)

func sendMessage(this js.Value, i []js.Value) interface{} {
	message := js.Global().Get("document").Call("getElementById", "message").Get("value")
	js.Global().Get("document").Call("getElementById", "quote").Set("textContent", message)
	return nil
}

func registerCallbacks() {
	js.Global().Set("sendMessage", js.FuncOf(sendMessage))
}

func main() {
	c := make(chan struct{}, 0)
	registerCallbacks()
	<-c
}
