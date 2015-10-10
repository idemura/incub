#include "iped.hxx"
#include "mongoose.h"

namespace iped {
std::unique_ptr<Flags> flags;

bool trim_trailing_slashes(string &uri) {
  if (!uri.empty() && uri.back() == '/' && uri.size() > 1) {
    auto n = uri.size() - 1;
    for (; uri[n] == '/' && n > 0; n--);
    uri.resize(n + 1);
    return true;
  }
  return false;
}

void response_redirect(mg_connection *conn, const string &uri) {
  mg_printf(conn,
      "HTTP/1.1 302 Found\r\n"
      "Location: %s\r\n"
      "\r\n", uri.data());
}

bool process_request(mg_connection *conn, string *buf) {
  string uri(conn->uri);
  if (trim_trailing_slashes(uri)) {
    response_redirect(conn, uri);
    return true;
  }

  mg_send_header(conn, "Content-Type", "text/html; charset=utf-8");
  if (uri == "/") {
    *buf = "<html>\n"
             "<body>\n"
               "<pre>\n"
                 "hello <a href=\"/\">from</a> the server\n"
               "</pre>\n"
             "</body>\n"
           "</html>\n";
    return true;
  }
  cerr<<"URI not found: "<<uri<<endl;
  return false;
}

int event_handler(mg_connection *conn, mg_event ev) {
  if (ev == MG_AUTH) {
    // All is authorized.
    return MG_TRUE;
  }
  if (ev == MG_REQUEST) {
    string buf;
    if (process_request(conn, &buf)) {
      mg_send_data(conn, buf.data(), buf.size());
      return MG_TRUE;
    }
  }
  return MG_FALSE;
}

int main(int argc, char **argv) {
  flags.reset(new Flags());
  if (!flags->parse(&argc, argv)) {
    return -1;
  }
  auto server = mg_create_server(nullptr, &event_handler);
  mg_set_option(server, "document_root", flags->document_root.c_str());
  mg_set_option(server, "listening_port", std::to_string(flags->port).c_str());

  cout<<"Waiting for requests..."<<endl;
  for (;;) {
    mg_poll_server(server, flags->poll_ms);
  }
  mg_destroy_server(&server);
  flags.reset();
  return 0;
}
}  // namespace

int main(int argc, char **argv) {
  std::ios_base::sync_with_stdio(false);
  return iped::main(argc, argv);
}
