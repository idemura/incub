#include "base.h"
#include "mongoose.h"

namespace {
bool trim_trailing(string &uri) {
  if (!uri.empty() && uri[uri.size() - 1] == '/' && uri != "/") {
    auto n = uri.size() - 1;
    for (; uri[n] == '/' && n > 0; n--);
    uri.resize(n + 1);
    return true;
  }
  return false;
}

void respond_redirect(mg_connection *conn, const string &uri) {
  mg_printf(conn,
      "HTTP/1.1 302 Found\r\n"
      "Location: %s\r\n"
      "\r\n", uri.data());
}

bool process_request(mg_connection *conn, std::stringstream &buf) {
  string uri(conn->uri);
  if (trim_trailing(uri)) {
    respond_redirect(conn, uri);
    return true;
  }

  if (uri == "/hello") {
    buf << "hello from the server";
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
      mg_send_data(conn, s.data(), s.size());
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
