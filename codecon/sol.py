import bisect

primes=sieve(100000)
def sieve(n):
  q=int(math.sqrt(n))
  s=[0]*(n+1)
  i=0
  while i<=q:
    if s[i]!=0: continue
    j=i*i
    while j<len(s):

      j+=i

def decomp(n):

def count_divisors(n):
  # Decompose on multipliers.
  d=2
  while (
