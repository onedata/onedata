Feature: Regular_file_stat

  Background:
    Given environment is defined in env.json
    And environment is up
    And u1 starts oneclient in /home/u1/onedata using token

  Scenario: Check file type when empty
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    Then u1 checks if file1 file type is regular empty file

  Scenario: Check file type when non-empty
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    And u1 writes "TEST TEXT ONEDATA" to file1
    Then u1 checks if file1 file type is regular file

  Scenario: Check default access permissions
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    Then u1 checks if file1 mode is 644

  Scenario: Change access permissions
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    And u1 changes file1 mode to 211
    Then u1 checks if file1 mode is 211

  Scenario: Increase regular file size
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    And u1 changes file1 size to 1000000 bytes
    Then u1 checks if file1 size is 1000000 bytes

  Scenario: Decrease regular file size
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    And u1 changes file1 size to 1000000 bytes
    And u1 checks if file1 size is 1000000 bytes
    And u1 changes file1 size to 0 bytes
    Then u1 checks if file1 size is 0 bytes

  Scenario: Timestamps at creation
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    Then u1 checks if modification time of file1 is equal to access time
    And u1 checks if status-change time of file1 is equal to access time

  Scenario: Access time
    When u1 writes "TEST TEXT ONEDATA" to file1
    And u1 sees file1 in .
    And u1 waits 1 seconds
    # call sleep, to be sure that time of write and read is different
    Then u1 reads "TEST TEXT ONEDATA" from file1
    And u1 checks if access time of file1 is greater than modification time
    And u1 checks if access time of file1 is greater than status-change time

  Scenario: Modification time
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    And u1 waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    And u1 writes "TEST TEXT ONEDATA" to file1
    Then last operation by u1 succeeds
    And u1 checks if modification time of file1 is greater than access time
    And u1 checks if modification time of file1 is equal to status-change time

  Scenario: Status-change time when changing mode
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    And u1 waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    And u1 changes file1 mode to 211
    Then last operation by u1 succeeds
    And u1 checks if status-change time of file1 is greater than modification time
    And u1 checks if status-change time of file1 is greater than access time

  Scenario: Status-change time when renaming
    When u1 creates regular files [file1]
    And u1 sees file1 in .
    And u1 waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    And u1 renames file1 to file2
    Then last operation by u1 succeeds
    And u1 checks if status-change time of file2 is greater than modification time
    And u1 checks if status-change time of file2 is greater than access time
