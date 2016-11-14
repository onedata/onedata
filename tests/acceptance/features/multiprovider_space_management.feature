Feature: Space management with multiple providers

  Background:
    Given environment is up
    And users [u1, u2] register with passwords [password1, password2]
    And users [u1, u2] authorize with [p1, p2] certs
    And users [u1, u2] get their ids from OZ via REST

  Scenario: User joins unused space - test of proxy
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 doesn't see [s1] in . on client1
    And u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 can list s1 on client1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 can list s1 on client2
    And u1 creates regular files [s1/file1] on client1
    Then u2 sees [file1] in s1 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    And u2 creates regular files [s1/file2] on client2
    Then u1 sees [file2] in s1 on client1
    And u2 writes "TEST TEXT ONEDATA" to s1/file2 on client2
    Then u1 reads "TEST TEXT ONEDATA" from file s1/file2 on client1

  Scenario: User joins already used space - test of proxy
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 doesn't see [s1] in . on client1
    And u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 can list s1 on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    And u1 reads "TEST TEXT ONEDATA" from file s1/file1 on client1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 can list s1 on client2
    Then u2 sees [file1] in s1 on client2
    Then u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    And u1 writes "MODIFIED TEXT" to s1/file1 on client1
    Then u2 reads "MODIFIED TEXT" from file s1/file1 on client2
    And u2 creates regular files [s1/file2] on client2
    Then u1 sees [file2] in s1 on client1
    And u2 writes "TEST TEXT ONEDATA" to s1/file2 on client2
    Then u1 reads "TEST TEXT ONEDATA" from file s1/file2 on client1

  Scenario: User joins unused space - test of dbsync
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 doesn't see [s1] in . on client1
    And u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 can list s1 on client1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 can list s1 on client2
    And u1 assigns u2 privileges [space_add_provider] for space s1
    And u2 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u2 by provider p2
    And u1 creates regular files [s1/file1] on client1
    Then u2 sees [file1] in s1 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    And u2 creates regular files [s1/file2] on client2
    Then u1 sees [file2] in s1 on client1
    And u2 writes "TEST TEXT ONEDATA" to s1/file2 on client2
    Then u1 reads "TEST TEXT ONEDATA" from file s1/file2 on client1

  Scenario: User joins used space - test of dbsync
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 doesn't see [s1] in . on client1
    And u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 can list s1 on client1
    And u1 creates regular files [s1/file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    And u1 reads "TEST TEXT ONEDATA" from file s1/file1 on client1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 can list s1 on client2
    And u1 assigns u2 privileges [space_add_provider] for space s1
    And u2 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u2 by provider p2
    Then u2 sees [file1] in s1 on client2
    Then u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    And u2 creates regular files [s1/file2] on client2
    Then u1 sees [file2] in s1 on client1
    And u2 writes "TEST TEXT ONEDATA" to s1/file2 on client2
    Then u1 reads "TEST TEXT ONEDATA" from file s1/file2 on client1
    And u1 creates regular files [s1/file3] on client1
    And u1 writes "ANOTHER TEST TEXT ONEDATA" to s1/file3 on client1
    Then u1 reads "ANOTHER TEST TEXT ONEDATA" from file s1/file3 on client1
    Then u2 reads "ANOTHER TEST TEXT ONEDATA" from file s1/file3 on client2


  Scenario: Remove user from space
    Given [u2] start oneclients [client2] in
      [/home/u2/onedata] on client_hosts
      [client-host2] respectively,
      using [token]
    When u1 creates spaces [s1]
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u2 can list s1 on client2
    And u1 removes u2 from space s1
    Then u2 doesn't see [s1] in . on client2

  Scenario: Delete supported space
    Given [u2] start oneclients [client2] in
      [/home/u2/onedata] on client_hosts
      [client-host2] respectively,
      using [token]
    When u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 can list s1 on client2
    And u1 deletes space s1
    Then u2 doesn't see [s1] in . on client2

  Scenario: Space supported and unsupported
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 doesn't see [s1] in . on client1
    And u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 can list s1 on client1
    And u1 creates regular files [s1/file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    And u1 reads "TEST TEXT ONEDATA" from file s1/file1 on client1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 can list s1 on client2
    And u1 assigns u2 privileges [space_add_provider] for space s1
    And u2 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u2 by provider p2
    And u2 sees [file1] in s1 on client2
    And u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    And u2 creates regular files [s1/file2] on client2
    And u1 sees [file2] in s1 on client1
    And u2 writes "TEST TEXT ONEDATA" to s1/file2 on client2
    And u1 reads "TEST TEXT ONEDATA" from file s1/file2 on client1
    And provider p1 unsupports space s1
    And u1 reads "TEST TEXT ONEDATA" from file s1/file2 on client1
    And u2 reads "TEST TEXT ONEDATA" from file s1/file2 on client2
    And u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    And u1 creates regular files [s1/file3] on client1
    And u1 writes "ANOTHER TEST TEXT ONEDATA" to s1/file3 on client1
    And u1 reads "ANOTHER TEST TEXT ONEDATA" from file s1/file3 on client1
    And u2 reads "ANOTHER TEST TEXT ONEDATA" from file s1/file3 on client2

  Scenario: Exceed quota - test of proxy
    Given [u2] start oneclients [client2] in
      [/home/u2/onedata] on client_hosts
      [client-host2] respectively,
      using [token]
    When u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 can list s1 on client2
    And u2 writes 2 MB of random characters to s1/file1 on client2 and saves MD5
    And u2 waits 10 seconds
    And u2 fails to write "TEST TEXT ONEDATA" to s1/file1 on client2

  Scenario: Exceed quota - test of dbsync
    Given [u2] start oneclients [client2] in
      [/home/u2/onedata] on client_hosts
      [client-host2] respectively,
      using [token]
    When u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u1 assigns u2 privileges [space_add_provider] for space s1
    And u2 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u2 by provider p2
    And u2 can list s1 on client2
    And u2 writes 2 MB of random characters to s1/file1 on client2 and saves MD5
    And u2 waits 10 seconds
    And u2 fails to write "TEST TEXT ONEDATA" to s1/file1 on client2