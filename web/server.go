package main

import ("fmt"; "net/http")

type Server struct {
  // State
}

func (h *Server) ServeHTTP(
    w http.ResponseWriter,
    r *http.Request) {
  fmt.Fprintf(w, "Hello!")
}

func main() {
  const address = "localhost:4000"
  fmt.Printf("Server started at %s\n", address)
  var srv = new(Server)
  http.ListenAndServe(address, srv)
}
