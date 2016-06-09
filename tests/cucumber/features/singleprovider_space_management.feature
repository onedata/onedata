Feature: Space management with single provider

  Background:
  Given environment is up
  Given openId server is started
  And users [u1, u2] will log in with emails [u1@mail.com, u2@mail.com]
  And users [u1, u2] will authorize with [p1, p1] certs
  And users are logged in
  And users are authorized
  And environment is clean

  Scenario: Default space without support
    Given u1 starts oneclient in /home/u1/onedata using token on client1
    Then u1 can't list spaces/u1's space on client1

  Scenario: Default space with support
    Given u1 starts oneclient in /home/u1/onedata using token on client1
    When u1 sees [u1's space] in spaces on client1
    And  u1 can't list spaces/u1's space on client1
    And u1 asks for support of space [u1's space]
    And [u1's space] is supported for u1 by p1 with 1 MB
    And  u1 can list spaces/u1's space on client1
    And u1 creates regular files [file1] on client1
    Then u1 sees [file1] in spaces/u1's space on client1
    And u1 writes "TEST TEXT ONEDATA" to spaces/u1's space/file1 on client1
    Then u1 reads "TEST TEXT ONEDATA" from spaces/u1's space/file1 on client1

  Scenario: New space without support
    Given u1 starts oneclient in /home/u1/onedata using token on client1
    When u1 creates spaces [s1]
    Then u1 can't list spaces/s1 on client1

  Scenario: New space with support
    Given u1 starts oneclient in /home/u1/onedata using token on client1
    When u1 sees [u1's space] in spaces on client1
    And u1 doesn't see [s1] in spaces on client1
    And u1 creates spaces [s1]
    And u1 can't list spaces/s1 on client1
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 by p1 with 1 MB
    And  u1 can list spaces/s1 on client1
    When u1 creates regular files [spaces/s1/file1] on client1
    Then u1 sees [file1] in spaces/s1 on client1
    And u1 writes "TEST TEXT ONEDATA" to spaces/s1/file1 on client1
    Then u1 reads "TEST TEXT ONEDATA" from spaces/s1/file1 on client1

  Scenario: Invite user to default space
    Given u2 starts oneclient in /home/u2/onedata using token on client1
    When u2 doesn't see [u1's space] in spaces on client1
    When u1 invites u2 to space u1's space
    And u2 joins space u1's space
    And u2 sees [u1's space] in spaces on client1
    And u2 can't list spaces/s1 on client1
    And u1 asks for support of space [u1's space]
    And [u1's space] is supported for u1 by p1 with 1 MB
    And u2 can list spaces/u1's space on client1
    And u2 creates regular files [spaces/u1's space/file1] on client1
    Then u2 sees [file1] in spaces/u1's space on client1
    And u2 writes "TEST TEXT ONEDATA" to spaces/u1's space/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from spaces/u1's space/file1 on client1

  Scenario: Invite user to non-default space
    Given u2 starts oneclient in /home/u2/onedata using token on client1
    When u2 doesn't see [s1] in spaces on client1
    When u1 creates spaces [s1]
    And u1 invites u2 to space s1
    And u2 joins space s1
    And u2 sees [s1] in spaces on client1
    And u2 can't list spaces/s1 on client1
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 by p1 with 1 MB
    And u2 can list spaces/s1 on client1
    And u2 creates regular files [spaces/s1/file1] on client1
    Then u2 sees [file1] in spaces/s1 on client1
    And u2 writes "TEST TEXT ONEDATA" to spaces/s1/file1 on client1
    Then u2 reads "TEST TEXT ONEDATA" from spaces/s1/file1 on client1

  Scenario: Remove user from space
    Given u2 starts oneclient in /home/u2/onedata using token on client1
    When u1 invites u2 to space u1's space
    And u2 joins space u1's space
    And u1 asks for support of space [s1]
    And [s1] is supported for u1 by p1 with 1 MB
    And u2 can list spaces/u1's space on client1
    And u1 removes u2 from space u1's space
    Then u2 doesn't see [u1's space] in spaces on client1

  Scenario: Delete supported default space
    Given u1 starts oneclient in /home/u1/onedata using token on client1
    When u1 asks for support of space [u1's space]
    And [u1's space] is supported for u1 by p1 with 1 MB
    When u1 can list spaces/u1's space on client1
    And u1 deletes space u1's space
    Then u1 can't list spaces/u1's space on client1

  Scenario: Delete supported non-default space
    Given u2 starts oneclient in /home/u2/onedata using token on client1
    When u2 creates spaces [s1]
    And u2 asks for support of space [s1]
    And [s1] is supported for u2 by p1 with 1 MB
    And u2 can list spaces/s1 on client1
    And u2 deletes space s1
    Then u2 doesn't see [s1] in spaces on client1
