Feature: Multiprovider_proxy_replication

  Background:
    Given environment is defined in multiprovider_proxy_env.json
    And storage directories are empty
    And environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [token, token]

  Scenario: Create files and see them on external provider
    When user waits 5 seconds
    And u1 creates regular files [file1, file2, file3] on client1
    Then user waits 10 seconds
    And u2 sees [file1, file2, file3] in . on client2

  Scenario: Create empty file and read it on external provider
    When user waits 5 seconds
    And u1 creates regular files [file] on client1
    Then user waits 10 seconds
    And u2 reads "" from file on client2

  Scenario: Write to file and check size on remote provider
    When user waits 5 seconds
    And u1 creates regular files [file] on client1
    And u1 writes "TEST TEXT ONEDATA" to file1 on client1
    Then user waits 10 seconds
    And size of u2's file1 is 17 bytes on client2

  Scenario: Write to file and read on remote provider
    When user waits 5 seconds
    And u1 creates regular files [file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to file1 on client1
    Then user waits 10 seconds
    And u2 reads "TEST TEXT ONEDATA" from file1 on client2

  Scenario: Big file transfer with MD5 check
    When user waits 5 seconds
    And u1 creates regular files [file1] on client1
    And u1 writes 16 MB of random characters to file1 on client1 and saves MD5
    Then user waits 10 seconds
    And u2 checks MD5 of file1 on client2

  Scenario: Create nonempty file and override its contents on remote provider
    When user waits 5 seconds
    And u1 creates regular files [file1] on client1
    And u1 writes "123456789" to file1 on client1
    And user waits 10 seconds
    And u2 writes "abcd" to file1 on client2
    Then user waits 15 seconds
    And u1 reads "abcd" from file1 on client1

  Scenario: Create nonempty file and remove it on remote provider
    When user waits 5 seconds
    And u1 creates regular files [file1] on client1
    And u1 writes "123456789" to file1 on client1
    And user waits 10 seconds
    And u1 deletes files [file1] on client1
    And u1 doesn't see [file1] in . on client1
    Then user waits 10 seconds
    And u2 doesn't see [file1] in . on client2

  Scenario: Create nonempty file, append remotely, append locally and read both
    When user waits 5 seconds
    And u1 creates regular files [file1] on client1
    And u1 writes "a" to file1 on client1
    And user waits 10 seconds
    And u2 appends "b" to file1 on client2
    And user waits 10 seconds
    And u1 appends "c" to file1 on client1
    Then user waits 10 seconds
    And u1 reads "abc" from file1 on client1
    And u2 reads "abc" from file1 on client2

  Scenario: Concurrently write disjoint ranges and read the same on both providers
    When user waits 5 seconds
    And u1 creates regular files [file1] on client1
    And user waits 10 seconds
    And u2 writes "defg" at offset 3 to file1 on client2
    And u1 writes "abc" at offset 0 to file1 on client1
    Then user waits 10 seconds
    And u1 reads "abcdefg" from file1 on client1
    And u2 reads "abcdefg" from file1 on client2