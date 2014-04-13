#include <algorithm>
#include <map>
#include <string>
#include <vector>
#include <utility>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

typedef long long int lli;

class StringPrinter {
  NON_COPYABLE(StringPrinter);
public:
  StringPrinter() {}
  void print(const char *fmt, ...);
  void flush(FILE *f);

private:
  std::string buf_;
};

void StringPrinter::print(const char *fmt, ...)
{
  char buf[80] = {};
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(buf, sizeof buf, fmt, ap);
  va_end(ap);
  buf_.append(buf);
}

void StringPrinter::flush(FILE *f)
{
  fputs(buf_.c_str(), f);
  buf_.clear();
}

class Timer {
  NON_COPYABLE(Timer);
public:
  Timer(): total_(), count_() {}
  lli getTotal() const { return total_; }
  int getCount() const { return count_; }
  int getAverage() const {
    return count_? static_cast<int>(total_ / count_): -1;
  }

  class Scope {
    NON_COPYABLE(Scope);
  public:
    explicit Scope(Timer &timer): timer_(timer), start_(getAccurateTimer()) {}
    ~Scope() {
      timer_.Add(getAccurateTimer() - start_);
    }

  private:
    Timer &timer_;
    lli start_;
  };

private:
  // Thread timer in microseconds.
  static lli getAccurateTimer();
  void Add(lli t);

  lli total_, count_;
};

lli Timer::getAccurateTimer()
{
  struct timeval tv = {};
  gettimeofday(&tv, NULL);
  return static_cast<lli>(tv.tv_sec) * 1000000 + tv.tv_usec;
}

void Timer::Add(lli t)
{
  total_ += t;
  count_++;
}

int main(int argc, char **argv)
{
// #ifndef ONLINE_JUDGE
//   freopen("in", "r", stdin);
// #endif
  return 0;
}
