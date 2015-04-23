#include "base.h"
#include "mongoose.h"

namespace {
bool process_request(mg_connection *conn, std::stringstream &buf) {
  string uri(conn->uri);
  if (uri == "/hello") {
    buf << "hello from me!!!";
    return true;
  }
  cerr << "URI not found: " << uri << endl;
  return false;
}

int event_handler(mg_connection *conn, mg_event ev) {
  if (ev == MG_AUTH) {
    // All is authorized.
    return MG_TRUE;
  }
  if (ev == MG_REQUEST) {
    std::stringstream buf;
    if (process_request(conn, buf)) {
      auto s = buf.str();
      mg_write(conn, s.data(), s.size());
      return MG_TRUE;
    }
  }
  return MG_FALSE;
}
}  // namespace

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);
  auto server = mg_create_server(nullptr, event_handler);
  mg_set_option(server, "document_root", ".");
  mg_set_option(server, "listening_port", "8060");

  cout << "Waiting for requests..." << endl;
  for (;;) {
    mg_poll_server(server, 1000);
  }
  mg_destroy_server(&server);
  return 0;
}
