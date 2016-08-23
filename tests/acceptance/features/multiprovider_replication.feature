Feature: Multiprovider_replication

  Background:
    Given environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]

  Scenario: Create files and see them on external provider
    When u1 creates regular files [s1/file1, s1/file2, s1/file3] on client1
    Then u2 sees [file1, file2, file3] in s1 on client2

  Scenario: Create empty file and read it on external provider
    When u1 creates regular files [s1/file] on client1
    Then u2 reads "" from file s1/file on client2

  Scenario: Write to file and check size on remote provider
    When u1 creates regular files [s1/file] on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    Then size of u2's s1/file1 is 17 bytes on client2

  Scenario: Write to file and read on remote provider
    When u1 creates regular files [s1/file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2

  Scenario: Big file transfer with MD5 check
    When u1 creates regular files [s1/file1] on client1
    And u1 writes 8 MB of random characters to s1/file1 on client1 and saves MD5
    Then u2 checks MD5 of s1/file1 on client2
    And u1 writes 8 MB of random characters to s1/file1 on client1 and saves MD5
    And u2 checks MD5 of s1/file1 on client2

  Scenario: Create nonempty file and override its contents on remote provider
    When u1 creates regular files [s1/file1] on client1
    And u2 sees [file1] in s1 on client2
    And u1 changes s1/file1 mode to 666 on client1
    And mode of u2's s1/file1 is 666 on client2
    And u1 writes "123456789" to s1/file1 on client1
    And user waits 10 seconds
    And u2 writes "abcd" to s1/file1 on client2
    And u1 reads "abcd" from file s1/file1 on client1

  Scenario: Create nonempty file and remove it on remote provider
    When u1 creates regular files [s1/file1] on client1
    And u2 sees [file1] in s1 on client2
    And u1 writes "123456789" to s1/file1 on client1
    And u1 deletes files [s1/file1] on client1
    Then u1 doesn't see [file1] in s1 on client1
    And u2 doesn't see [file1] in s1 on client2

  Scenario: Create nonempty file, append remotely, append locally and read both
    When u1 creates regular files [s1/file1] on client1
    And u1 writes "a" to s1/file1 on client1
    And u1 changes s1/file1 mode to 666 on client1
    And mode of u2's s1/file1 is 666 on client2
    And u1 waits 10 seconds
    And u2 appends "b" to s1/file1 on client2
    And u1 waits 10 seconds
    And u1 appends "c" to s1/file1 on client1
    Then u1 reads "abc" from file s1/file1 on client1
    And u2 reads "abc" from file s1/file1 on client2

  Scenario: Concurrently write disjoint ranges and read the same on both providers
    When u1 creates regular files [s1/file1] on client1
    And u2 sees [file1] in s1 on client2
    And u1 changes s1/file1 mode to 666 on client1
    And mode of u2's s1/file1 is 666 on client2
    And u2 writes "defg" at offset 3 to s1/file1 on client2
    And u1 writes "abc" at offset 0 to s1/file1 on client1
    Then u1 reads "abcdefg" from file s1/file1 on client1
    And u2 reads "abcdefg" from file s1/file1 on client2