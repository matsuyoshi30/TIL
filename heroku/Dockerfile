FROM golang:latest as builder
WORKDIR /go/src/github.com/matsuyoshi30/go-heroku-sample
COPY . .
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build main.go

FROM alpine:latest
WORKDIR /root/
COPY --from=builder /go/src/github.com/matsuyoshi30/go-heroku-sample /app
CMD /app/main $PORT