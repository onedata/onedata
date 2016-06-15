Feature: Space management with multiple providers

  Background:
    Given environment is up
#    Given openId server is started
    And users [u1, u2] register with passwords [password1, password2]
#    And users [u1, u2] will log in with emails [u1@mail.com, u2@mail.com]
#    And users [u1, u2] will authorize with [p1, p2] certs
#    And users are logged in
#    And users are authorized
#    And environment is clean

  Scenario: Default space with support
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 sees [u1's space] in . on client1
    When u2 sees [u2's space] in . on client2
    And  u1 can't list u1's space on client1
    And  u2 can't list u2's space on client2
    And u1 asks for support of space [u1's space]
    And u2 asks for support of space [u2's space]
    And [u1's space] is supported for u1 by p1 with 1 MB
    And [u2's space] is supported for u2 by p2 with 1 MB
    And  u1 can list u1's space on client1
    And  u2 can list u2's space on client2
    And u1 creates regular files [u1's space/file1] on client1
    And u2 creates regular files [u2's space/file1] on client2
    Then u1 sees [file1] in u1's space on client1
    Then u2 sees [file1] in u2's space on client2
    And u1 writes "TEST TEXT ONEDATA" to u1's space/file1 on client1
    And u2 writes "TEST TEXT ONEDATA" to u2's space/file1 on client2
    Then u1 reads "TEST TEXT ONEDATA" from u1's space/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from u2's space/file1 on client2

  Scenario: Default space of other user supported by other provider
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 sees [u1's space] in . on client1
    And u1 asks for support of space [u1's space]
    And [u1's space] is supported for u1 by p1 with 1 MB
    And u1 can list u1's space on client1
    And u1 invites u2 to space u1's space
    And u2 joins space u1's space
    And  u2 can list u2's space on client2
    And u1 creates regular files [u1's space/file1] on client1
    Then u2 sees [file1] in u1's space on client2
    And u1 writes "TEST TEXT ONEDATA" to u1's space/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from u1's space/file1 on client2
    And u2 creates regular files [u1's space/file2] on client2
    Then u1 sees [file2] in u1's space on client1
    And u2 writes "TEST TEXT ONEDATA" to u1's space/file1 on client2
    Then u1 reads "TEST TEXT ONEDATA" from u1's space/file1 on client1

  Scenario: Non-existing space of other user supported by other provider
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 sees [u1's space] in . on client1
    And u1 doesn't see [s1] in . on client1
    And u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 by p1 with 1 MB
    And u1 can list s1 on client1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And  u2 can list s1 on client2
    And u1 creates regular files [s1/file1] on client1
    Then u2 sees [file1] in s1 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from s1/file1 on client2
    And u2 creates regular files [s1/file2] on client2
    Then u1 sees [file2] in s1 on client1
    And u2 writes "TEST TEXT ONEDATA" to s1/file1 on client2
    Then u1 reads "TEST TEXT ONEDATA" from s1/file1 on client1


  Scenario: Space supported by two providers
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    When u1 sees [u1's space] in . on client1
    And u1 doesn't see [s1] in . on client1
    And u2 doesn't see [s1] in . on client2
    And u1 creates spaces [s1]
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 by p1 with 1 MB
    And u1 can list s1 on client1
    And u1 invites u2 to space s1
    And u2 joins space s1
    And  u2 can list s1 on client2
    And u1 assigns u2 privileges [space_add_provider] for space s1
    And u2 asks for support of space [s1]
    And [s1] is supported for u2 by p2 with 1 MB
    And u1 creates regular files [s1/file1] on client1
    Then u2 sees [file1] in s1 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from s1/file1 on client2
    And u2 creates regular files [s1/file2] on client2
    Then u1 sees [file2] in s1 on client1
    And u2 writes "TEST TEXT ONEDATA" to s1/file1 on client2
    Then u1 reads "TEST TEXT ONEDATA" from s1/file1 on client1

  Scenario: Remove user from space
    Given [u2] start oneclients [client2] in
      [/home/u2/onedata] on client_hosts
      [client-host2] respectively,
      using [token]
    When u1 creates spaces [s1]
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 by p1 with 1 MB
    And u2 can list s1 on client2
    And u1 removes u2 from space s1
    Then u2 doesn't see [s1] in . on client2

  Scenario: Delete supported default space
    Given [u2] start oneclients [client2] in
      [/home/u2/onedata] on client_hosts
      [client-host2] respectively,
      using [token]
    When u1 asks for support of space [u1's space]
    And [u1's space] is supported for u1 by p1 with 1 MB
    When u2 can list u1's space on client2
    And u2 deletes space u1's space
    Then u2 can't list u1's space on client2

  Scenario: Delete supported non-default space
    Given [u2] start oneclients [client2] in
      [/home/u2/onedata] on client_hosts
      [client-host2] respectively,
      using [token]
    When u1 creates spaces [s1]
    And u1 asks for support of space [s1]
    And [s1] is supported for u2 by p1 with 1 MB
    And u2 can list s1 on client2
    And u1 deletes space s1
    Then u2 doesn't see [s1] in . on client2