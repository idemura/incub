# Approximate Latency Numbers

```
L1 cache reference                      0.5 ns
Branch mispredict                       5 ns
L2 cache reference                      7 ns    14x L1
Mutex lock/unlock                      25 ns
RAM reference                         100 ns    20x L2 cache, 200x L1 cache
Compress 1K with zippy                  3 us
Send 1K over 1GBPS net                 10 us
Read 4K randomly from SSD             150 us    ~1GB/sec SSD
Read 1M sequentially from RAM         250 us
DC roundtrip                          500 us
Read 1 MB sequentially from SSD         1 ms    ~1GB/sec SSD, 4X memory
Disk seek                              10 ms    20x DC roundtrip
Read 1 MB sequentially from disk       20 ms    80x memory, 20x SSD
Send packet CA->Netherlands->CA       150 ms
```

## Notes
DC - data center
