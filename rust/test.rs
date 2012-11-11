// Rust programming test

use core::*;
use os::list_dir;

fn collect_matches(dir: &str) -> ~[~str]
{
  let mut v = dvec::DVec();
  v.get()
}

fn main()
{
  let fs = list_dir(&Path(&"."));

  
  for fs.each |f| {
    io::println(fmt!("%s", *f));
  }

  io::println("1234");
}
