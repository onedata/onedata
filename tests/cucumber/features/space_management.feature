Feature: User management

  Background:
  Given environment is up
  Given openId server is started
  And users [u1, u2] will log in with emails [u1@mail.com, u2@mail.com]
  And users [u1, u2] will authorize with [p1, p1] certs
  And users are logged in
  And users are authorized
#  And users info is saved

  Scenario: Check default space
    Given [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host1] respectively,
      using [token, token]
    Then [u1's space] are mounted for u1 on client1
    And [u2's space] are mounted for u2 on client2

  Scenario: New space with support
    Given u1 starts oneclient in /home/u1/onedata using token on client1
    When [u1's space] is mounted for u1 on client1
    When u1 creates spaces [s1]
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 with 1 MB
    And u1 asks for support of space [u1's space]
    And [u1's space] is supported for u1 with 1 MB
    Then u1 sees [s1] in spaces on client1
#    And  u1 can list spaces/s1 on client1

  Scenario: New space without support
    Given u1 starts oneclient in /home/u1/onedata using token on client1
    When u1 creates spaces [s2]
    Then u1 can't list spaces/s2 on client1

  Scenario: Invite user to default space
    Given u2 starts oneclient in /home/u2/onedata using token on client1
    When u2 doesn't see [s1] in spaces on client1
    When u1 invites u2 to space u1's space
    And u2 joins space u1's space
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 with 1 MB
    And u2 can list spaces/u1's space on client1
    And u2 creates regular files [spaces/u1's space/file1] on client1
    Then u2 sees [file1] in spaces/u1's space on client1
    And u1 sees [file1] in spaces/u1's space on client1

  Scenario: Invite user to non-default space
    Given u2 starts oneclient in /home/u2/onedata using token on client1
    When u2 doesn't see [s1] in spaces on client1
    When u1 creates spaces [s1]
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 with 1 MB
    And u2 can list spaces/s1 on client1
    And u2 creates regular files [spaces/s1] on client1
    Then u2 sees [file1] in spaces/s1 on client1
    And u1 sees [file1] in spaces/s1 on client1

  Scenario: Remove user from space
    Given u2 starts oneclient in /home/u2/onedata using token on client1
    When u1 invites u2 to space u1's space
    And u2 joins space u1's space
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 with 1 MB
    And u2 can list spaces/u1's space on client1
    And u1 removes u2 from space u1's space
    Then u2 can't list spaces/u1's space on client1

  Scenario: Delete space
    Given u1 starts oneclient in /home/u1/onedata using token on client1
    When u1 asks for support of space [s1]
    And [s1] is supported for u1 with 1 MB
    Then u1 can list spaces/u1's space on client1
    And u1 deletes space u1's space
    Then u1 can't list spaces/u1's space on client1