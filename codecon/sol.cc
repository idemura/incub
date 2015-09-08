#include "base.h"

using SysTime=chrono::time_point<chrono::system_clock>;
using Seconds=chrono::duration<double>;

SysTime system_time() {
  return chrono::system_clock::now();
}

// In seconds.
double time_diff(SysTime start, SysTime end) {
  return Seconds(end-start).count();
}

struct PRange {
  int *p=nullptr, n=0;
  PRange() {}
  PRange(int *p, int n): p(p), n(n) {}
};

struct IntPool {
  static constexpr int kBlockSize=2*1024<<10;
  vector<unique_ptr<int[]>> blocks;
  int block_free=0;

  IntPool() {blocks.reserve(16);}
  PRange alloc(const int *src, int n) {
    if (n>block_free) {
      blocks.emplace_back(new int[kBlockSize]);
      block_free=kBlockSize;
    }
    auto p=blocks.back().get()+kBlockSize-block_free;
    block_free-=n;
    if (src != nullptr) memcpy(p,src,sizeof(*src)*n);
    return PRange(p,n);
  }
};

int sqrt_int(int n) {
  return int(sqrt(n));
}

// Factor and its degree(power, exponent).
struct Factor {
  int n=0, d=0;
  Factor() {}
  Factor(int n, int d): n(n), d(d) {}
};

ostream& operator<<(ostream& s, Factor d) {
  return s<<"Factor("<<d.n<<","<<d.d<<")";
}

vector<Factor> sieve_degree(int n) {
  vector<Factor> factor(n+1);
  factor[1]=Factor(1,1);
  auto i=2, imax=sqrt_int(n);
  for (; i<=imax; i++) {
    if (factor[i].n!=0) continue;
    factor[i]={i,1};
    for (int j=i*i; j<=n; j+=i) {
      if (factor[j].n==0) factor[j]={i,1};
    }
    auto max_non_overflow=n/i;
    for (int k=i*i; k<=n; k*=i) {
      for (int j=k; j<=n; j+=k) {
        if (factor[j].n==i) factor[j].d++;
      }
      if (k>max_non_overflow) break;
    }
  }
  for (; i<=n; i++) {
    if (factor[i].n==0) factor[i]=Factor(i,1);
  }
  return factor;
}

vector<Factor> factorize(const vector<int> &factor, int n) {
  vector<Factor> result;
  if (n<=1) return result;
  Factor p(factor[n],1);
  n/=factor[n];
  while (n!=1) {
    if (p.n==factor[n]) {
      p.d++;
    } else {
      result.push_back(p);
      p=Factor(factor[n],1);
    }
    n/=factor[n];
  }
  result.push_back(p);
  return result;
}

void print_max_and_avg(const vector<vector<int>> &v) {
  i64 total=0, size_max=0;
  for (auto &subv : v) {
    total+=subv.size();
    size_max=max<i64>(subv.size(),size_max);
  }
  cout<<"total="<<total<<" avg="<<(total/v.size())<<" max="<<size_max<<endl;
}

vector<PRange> divisors(const vector<Factor> &ft, IntPool &pool) {
  vector<PRange> divs(ft.size());
  // To speed up use stack memory. We know numbers less or equal than 1e5 have
  // max 127 divisors.
  int tmp[140]={};
  for (int i=2; i<ft.size(); i++) {
    tmp[0]=ft[i].n;
    for (int j=1; j<ft[i].d; j++) {
      tmp[j]=tmp[j-1]*ft[i].n;
    }
    // All divisors are: powers of current, all from number without current
    // (minimal) divisor, and their pairwise multiplication.
    const auto multiples_n=ft[i].d;
    int h=multiples_n;
    int no_fi=i/tmp[multiples_n-1];
    if (no_fi!=1) {
      for (int j=0; j<divs[no_fi].n; j++) {
        tmp[h++]=divs[no_fi].p[j];
      }
      for (int k=0; k<multiples_n; k++) {
        for (int j=0; j<divs[no_fi].n; j++) {
          tmp[h++]=tmp[k]*divs[no_fi].p[j];
        }
      }
    }
    divs[i]=pool.alloc(nullptr,h);
    merge(tmp,tmp+multiples_n,tmp+multiples_n,tmp+h,divs[i].p);
  }
  return divs;
}

int count_divisors(const vector<Factor> &f) {
  auto c=f[0].d;
  for (int i=1; i<f.size(); i++) {
    c=(f[i].d+1)*c+f[i].d;
  }
  return c;  //1 is not counted.
}

void test(const vector<int> &ft, int n) {
  auto c=count_divisors(factorize(ft,n));
  cout<<"n="<<n<<" count "<<c<<endl;
}

PRange cascade_merge(
      const PRange &in1,
      const PRange &in2,
      IntPool &pool) {
  cout<<"-------------------\n";
  cout<<"in1:";
  for (int i=0; i<in1.n; i++) {
    cout<<" "<<in1.p[i];
  }
  cout<<endl;
  cout<<"in2:";
  for (int i=0; i<in2.n; i++) {
    cout<<" "<<in2.p[i];
  }
  cout<<endl;
  int tmp[140]={}, tmp_n=1;
  int i1=0, i2=0;
  while (i1<in1.n && i2<in2.n) {
    if (in1.p[i1]<in2.p[i2]) {
      if (in1.p[i1]!=tmp[tmp_n-1]) {
        cout<<"added "<<in1.p[i1]<<endl;
        tmp[tmp_n++]=in1.p[i1];
      }
      i1++;
    } else {
      if (in2.p[i2]!=tmp[tmp_n-1]) {
        cout<<"added "<<in2.p[i2]<<endl;
        tmp[tmp_n++]=in2.p[i2];
      }
      i2++;
    }
  }
  for (; i1<in1.n; i1++) tmp[tmp_n++]=in1.p[i1];
  for (; i2<in2.n; i2++) tmp[tmp_n++]=in2.p[i2];
  auto out=pool.alloc(tmp+1,tmp_n-1);
  cout<<"out:";
  for (int i=0; i<out.n; i++) {
    cout<<" "<<out.p[i];
  }
  cout<<endl;
  return out;
}

void cascade(vector<PRange> &divs, IntPool &pool, vector<vector<PRange>> &csd) {
  csd.resize(log2(divs.size()));
  csd[0]=move(divs);
  for (int i=1; i<csd.size(); i++) {
    cout<<"i="<<i<<endl;
    auto &src=csd[i-1];
    auto &dst=csd[i];
    dst.resize(src.size()>>1);
    cout<<"src.size="<<src.size()<<" dst.size="<<dst.size()<<endl;
    for (int j=0; j<src.size(); j+=2) {
      dst[j>>1]=cascade_merge(src[j],src[j+1],pool);
    }
  }
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  auto start=system_time();
  IntPool pool;
  constexpr int k=20;
  auto ft=sieve_degree(k);
  auto divs=divisors(ft,pool);
  auto t=time_diff(start,system_time());
  cout<<"Sieve and divisors "<<t*1000<<endl;
  // print_max_and_avg(divs);
  for (int i=0; i<=20; i++) {
    cout<<i<<":";
    for (int j=0; j<divs[i].n; j++) {
      cout<<" "<<divs[i].p[j];
    }
    cout<<endl;
  }

  start=system_time();
  vector<vector<PRange>> csd;
  cascade(divs,pool,csd);
  t=time_diff(start,system_time());
  cout<<"Cascading: "<<t*1000<<endl;

  // auto max_divisors=0, n=0;
  // unordered_map<int,int> freq_map;
  // i64 total=0;
  // for (int i=2; i<=k; i++) {
  //   auto c=count_divisors(factorize(ft,i));
  //   total+=c;
  //   if (c>max_divisors) {
  //     max_divisors=c;
  //     n=i;
  //   }
  //   freq_map[c]++;
  // }
  // Seconds d=system_time()-start;
  // cout<<"time="<<d.count()*1000<<" millids"<<endl;
  // cout<<"max_divisors="<<max_divisors<<" with "<<n<<endl;
  // // for (auto &e : freq_map) {
  // //   cout<<e.n<<" divisor have "<<e.d<<" numbers\n";
  // // }
  // cout<<"average "<<total/(k-1)<<" total "<<total<<endl;
  // // test(ft, 2);
  // // test(ft, 3);
  // // test(ft, 4);
  // // test(ft, 6);
  // // test(ft, 8);
  // // test(ft, 12);
  // // test(ft, 36);
  // // int c=0;
  // // for (int i=2; i<=36; i++) {
  // //   if (36%i==0) {
  // //     c++;
  // //     cout<<"factor "<<i<<endl;
  // //   }
  // // }
  // // cout<<c<<endl;
  return 0;
}
