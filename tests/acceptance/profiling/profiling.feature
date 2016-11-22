Feature: Authorization

  Background:
    Given environment is up
    And u1 starts oneclient in /home/u1/onedata using token

  Scenario: Profile ls
    When u1 creates directories [s1/dir1]
    When u1 remounts oneclient
    When profiling is started on worker1.p1
    When u1 can list s1/dir1
    Then profiling is stopped on worker1.p1

  Scenario: Profile ls without remounting
    When u1 creates directories [s1/dir1]
    When profiling is started on worker1.p1
    When u1 can list s1/dir1
    Then profiling is stopped on worker1.p1

  Scenario: Profile ls nested
    When u1 creates structure of 10 nested directories in s1
    When u1 remounts oneclient
    When profiling is started on worker1.p1
    When u1 lists directory nested on level 10 in s1
    Then profiling is stopped on worker1.p1

  Scenario: Profile ls nested without remounting
    When u1 creates structure of 10 nested directories in s1
    When u1 remounts oneclient
    When profiling is started on worker1.p1
    When u1 lists directory nested on level 10 in s1
    Then profiling is stopped on worker1.p1

  Scenario: Profile mkdir
    When profiling is started on worker1.p1
    When u1 creates structure of 10 nested directories in s1
    Then profiling is stopped on worker1.p1

  Scenario: Profile rm
    When u1 creates structure of 10 nested directories in s1
    When u1 remounts oneclient
    When profiling is started on worker1.p1
    And u1 deletes non-empty directories [s1/0]
    Then profiling is stopped on worker1.p1

  Scenario: Profile touch
    When profiling is started on worker1.p1
    When u1 creates regular files [s1/file1]
    Then profiling is stopped on worker1.p1

  Scenario: Profile write sysbench
    When profiling is started on worker1.p1
    And u1 performs command "sysbench --test=fileio --file-total-size=5MB --file-num=5 --file-test-mode=rndrw --init-rng=on prepare" in s1 directory
    Then u1 can list s1
    Then profiling is stopped on worker1.p1

  Scenario: Profile dd
    When profiling is started on worker1.p1
    And u1 performs command "dd if=/dev/zero of=test.dd bs=1M count=10" in s1 directory
    And u1 performs command "dd if=test.dd of=/dev/null" in s1 directory
    Then profiling is stopped on worker1.p1

  Scenario: Profile write
    When u1 creates regular files [s1/file1]
    When u1 remounts oneclient
    When profiling is started on worker1.p1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1
    Then profiling is stopped on worker1.p1

  Scenario: Profile read
    When u1 creates regular files [s1/file1]
    And u1 writes "TEST TEXT ONEDATA" to s1/file1
    When u1 remounts oneclient
    When profiling is started on worker1.p1
    And u1 reads "TEST TEXT ONEDATA" from file s1/file1
    Then profiling is stopped on worker1.p1
