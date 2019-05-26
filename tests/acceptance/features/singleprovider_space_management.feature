Feature: Space management with single provider

  Background:
    Given environment is up
    And users [u1, u2] register with passwords [password1, password2]
    And users [u1, u2] authorize with [p1, p1] certs
    And users [u1, u2] get their ids from OZ via REST

  Scenario: Create space and don't support it
    Given [u1] start oneclients [client1] in
      [/home/u1/onedata] on client_hosts
      [client-host1] respectively,
      using [token]
    When u1 doesn't see [s1] in . on client1
    And u1 creates spaces [s1]
    And u1 sees [s1] in . on client1
    Then u1 can't list s1 on client1

  Scenario: Create space and support it
    Given [u1] start oneclients [client1] in
      [/home/u1/onedata] on client_hosts
      [client-host1] respectively,
      using [token]
    When u1 doesn't see [s1] in . on client1
    And u1 creates spaces [s1]
    And u1 sees [s1] in . on client1
    And u1 can't list s1 on client1
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 can list s1 on client1
    When u1 creates regular files [s1/file1] on client1
    Then u1 can stat [file1] in s1 on client1
    Then u1 sees [file1] in s1 on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    Then u1 reads "TEST TEXT ONEDATA" from file s1/file1 on client1

  Scenario: Invite user to unused space
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u2 doesn't see [s1] in . on client2
    When u1 creates spaces [s1]
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 sees [s1] in . on client2
    And u2 can't list s1 on client2
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u2 can list s1 on client2
    And u2 creates regular files [s1/file1] on client2
    Then u2 can stat [file1] in s1 on client2
    Then u2 sees [file1] in s1 on client2
    And u2 writes "TEST TEXT ONEDATA" to s1/file1 on client2
    Then u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    And u1 reads "TEST TEXT ONEDATA" from file s1/file1 on client1
    And u1 creates regular files [s1/file2] on client1
    And u1 writes "ANOTHER TEST TEXT ONEDATA" to s1/file2 on client1
    Then u1 reads "ANOTHER TEST TEXT ONEDATA" from s1/file2 on client1
    And u2 reads "ANOTHER TEST TEXT ONEDATA" from s1/file2 on client1

  Scenario: Remove user from space
    Given [u2] start oneclients [client2] in
      [/home/u2/onedata] on client_hosts
      [client-host2] respectively,
      using [token]
    When u1 creates spaces [s1]
    When u1 invites u2 to space s1
    And u2 joins space s1
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u2 can list s1 on client2
    And u1 removes u2 from space s1
    Then u2 can't stat [s1] in . on client2
    Then u2 doesn't see [s1] in . on client2

  Scenario: Delete supported space
    Given [u1] start oneclients [client1] in
      [/home/u1/onedata] on client_hosts
      [client-host1] respectively,
      using [token]
    When u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 can list s1 on client1
    And u1 deletes space s1
    Then u1 can't stat [s1] in . on client1
    Then u1 doesn't see [s1] in . on client1

  Scenario: Exceed quota
    Given [u1] start oneclients [client1] in
      [/home/u1/onedata] on client_hosts
      [client-host1] respectively,
      using [token]
    When u1 creates spaces [s1]
    And u1 gets token to support spaces [s1]
    And s1 is supported with 1 MB for u1 by provider p1
    And u1 can list s1 on client1
    And u1 creates regular files [s1/file1] on client1
    And u1 writes 2 MB of random characters to s1/file1 on client1 and saves MD5
    And u1 waits 10 seconds
    And u1 fails to write "TEST TEXT ONEDATA" to s1/file1 on client1
