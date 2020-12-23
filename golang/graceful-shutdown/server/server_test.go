package server_test

import (
	"net/http"
	"os"
	"syscall"
	"testing"
	"time"

	"github.com/matsuyoshi30/graceful-shutdown/server"
)

func TestRun(t *testing.T) {
	var waitSignal bool
	waitStart := make(chan struct{}, 1)
	sigDone := make(chan struct{}, 1)

	s := server.New(":3000", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if waitSignal {
			waitStart <- struct{}{}
			<-sigDone
		}
	}))

	go s.Run()

	// first request
	_, err := http.Get("http://localhost" + s.Addr)
	if err != nil {
		t.Fatal(err)
	}

	// second request (server close after response)
	waitSignal = true
	go func() {
		_, err := http.Get("http://localhost" + s.Addr)
		if err != nil {
			t.Fatal(err)
		}
	}()

	// shutdown
	<-waitStart

	p, err := os.FindProcess(os.Getpid())
	if err != nil {
		t.Fatal(err)
	}
	err = p.Signal(syscall.SIGTERM)
	if err != nil {
		t.Fatal(err)
	}
	time.Sleep(1 * time.Second)

	sigDone <- struct{}{}

	// third request (should be unconnected)
	_, err = http.Get("http://localhost" + s.Addr)
	if err == nil {
		t.Errorf("expected error but got nil")
	}
}
