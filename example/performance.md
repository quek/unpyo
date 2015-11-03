```
yarn:~/job/actindi/ikoyo/git% ab -c 10 -n 10000 'http://127.0.0.1:1958/hello/world'
This is ApacheBench, Version 2.3 <$Revision: 1706008 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking 127.0.0.1 (be patient)
Completed 1000 requests
Completed 2000 requests
Completed 3000 requests
Completed 4000 requests
Completed 5000 requests
Completed 6000 requests
Completed 7000 requests
Completed 8000 requests
Completed 9000 requests
Completed 10000 requests
Finished 10000 requests


Server Software:
Server Hostname:        127.0.0.1
Server Port:            1958

Document Path:          /hello/world
Document Length:        440 bytes

Concurrency Level:      10
Time taken for tests:   2.538 seconds
Complete requests:      10000
Failed requests:        0
Total transferred:      5310000 bytes
HTML transferred:       4400000 bytes
Requests per second:    3940.27 [#/sec] (mean)
Time per request:       2.538 [ms] (mean)
Time per request:       0.254 [ms] (mean, across all concurrent requests)
Transfer rate:          2043.25 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.1      0       2
Processing:     0    2   1.1      2      23
Waiting:        0    2   1.1      2      23
Total:          1    3   1.1      2      23

Percentage of the requests served within a certain time (ms)
  50%      2
  66%      2
  75%      2
  80%      2
  90%      3
  95%      4
  98%      6
  99%      7
 100%     23 (longest request)
```

```
~/letter/lisp/craft/unpyo/example $ ab -c 10 -n 10000 'http://127.0.0.1:1958/hello/world'
This is ApacheBench, Version 2.3 <$Revision: 1706008 $>
Copyright 1996 Adam Twiss, Zeus Technology Ltd, http://www.zeustech.net/
Licensed to The Apache Software Foundation, http://www.apache.org/

Benchmarking 127.0.0.1 (be patient)
Completed 1000 requests
Completed 2000 requests
Completed 3000 requests
Completed 4000 requests
Completed 5000 requests
Completed 6000 requests
Completed 7000 requests
Completed 8000 requests
Completed 9000 requests
Completed 10000 requests
Finished 10000 requests


Server Software:
Server Hostname:        127.0.0.1
Server Port:            1958

Document Path:          /hello/world
Document Length:        440 bytes

Concurrency Level:      10
Time taken for tests:   1.048 seconds
Complete requests:      10000
Failed requests:        0
Total transferred:      5310000 bytes
HTML transferred:       4400000 bytes
Requests per second:    9539.45 [#/sec] (mean)
Time per request:       1.048 [ms] (mean)
Time per request:       0.105 [ms] (mean, across all concurrent requests)
Transfer rate:          4946.73 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    0   0.0      0       1
Processing:     0    1   0.4      1       6
Waiting:        0    1   0.3      1       6
Total:          0    1   0.4      1       6

Percentage of the requests served within a certain time (ms)
  50%      1
  66%      1
  75%      1
  80%      1
  90%      1
  95%      1
  98%      2
  99%      2
 100%      6 (longest request)
```
