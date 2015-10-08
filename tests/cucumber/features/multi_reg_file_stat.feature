Feature: Multi_regular_file_stat

  Background:
    Given environment is defined in env.json
    And environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on nodes [1, 2] respectively,
      using [token, token]

  Scenario: Check file type when empty
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    Then file type of u2's file1 is regular empty file on client2

  Scenario: Check file type when non-empty
    When u2 creates regular files [file1] on client2
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u2 writes "TEST TEXT ONEDATA" to file1 on client2
    Then file type of u1's file1 is regular file on client1

  Scenario: Check default access permissions
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    Then mode of u2's file1 is 644 on client2

  Scenario: Change access permissions
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u1 changes file1 mode to 211 on client1
    Then mode of u2's file1 is 211 on client2

  Scenario: Increase regular file size
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u1 changes file1 size to 1000000 bytes on client1
    Then size of u2's file1 is 1000000 bytes on client2

  Scenario: Decrease regular file size
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u1 changes file1 size to 1000000 bytes on client1
    And size of u2's file1 is 1000000 bytes on client2
    And u1 changes file1 size to 0 bytes on client1
    Then size of u2's file1 is 0 bytes on client2

  Scenario: Timestamps at creation
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    Then modification time of u2's file1 is equal to access time on client2
    And status-change time of u2's file1 is equal to access time on client2

  Scenario: Access time
    When u1 writes "TEST TEXT ONEDATA" to file1 on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u1 waits 1 seconds on client1
    # call sleep, to be sure that time of write and read is different
    Then u1 reads "TEST TEXT ONEDATA" from file1 on client1
    And access time of u2's file1 is greater than modification time on client2
    And access time of u2's file1 is greater than status-change time on client2

  Scenario: Modification time
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u1 waits 1 seconds on client1
    # call sleep, to be sure that time of above and below operations is different
    And u1 writes "TEST TEXT ONEDATA" to file1 on client1
    Then last operation by u1 succeeds
    And modification time of u2's file1 is greater than access time on client2
    And modification time of u2's file1 is equal to status-change time on client2

  Scenario: Status-change time when changing mode
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u1 waits 1 seconds on client1
    # call sleep, to be sure that time of above and below operations is different
    And u1 changes file1 mode to 211 on client1
    Then last operation by u1 succeeds
    And status-change time of u2's file1 is greater than modification time on client2
    And status-change time of u2's file1 is greater than access time on client2

  Scenario: Status-change time when renaming
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u1 waits 1 seconds on client1
    # call sleep, to be sure that time of above and below operations is different
    And u1 renames file1 to file2 on client1
    Then last operation by u1 succeeds
    And status-change time of u2's file2 is greater than modification time on client2
    And status-change time of u2's file2 is greater than access time on client2
