package server

import (
	"context"
	"errors"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
)

type MyServer struct {
	http.Server
}

func New(addr string, handler http.HandlerFunc) *MyServer {
	return &MyServer{
		http.Server{
			Addr:    addr,
			Handler: handler,
		},
	}
}

func (s *MyServer) Run() {
	go func() {
		quit := make(chan os.Signal, 1)
		signal.Notify(quit, syscall.SIGTERM, os.Interrupt)
		<-quit

		if err := s.Shutdown(context.Background()); err != nil {
			log.Println("Shutdown error")
			return
		}
	}()

	if err := s.ListenAndServe(); !errors.Is(err, http.ErrServerClosed) {
		log.Println("Listen and Serve error")
		return
	}
}
