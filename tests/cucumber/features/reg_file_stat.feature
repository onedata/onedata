Feature: Regular_file_stat

  Background:
    Given environment is defined in env.json
    Given environment is up
    Given u1 mounts onedata spaces in /root/onedata using token
    Given we are in space s1

  Scenario: Check file type when empty
    When u1 creates regular files [file1]
    Then file1 file type is regular empty file
    Then clean succeeds

  Scenario: Check file type when non-empty
    When u1 creates regular files [file1]
    When u1 writes "TEST TEXT ONEDATA" to file1
    Then file1 file type is regular file
    Then clean succeeds

  Scenario: Check default access permissions
    When u1 creates regular files [file1]
    Then file1 mode is 644
    Then clean succeeds

  Scenario: Change access permissions
    When u1 creates regular files [file1]
    When u1 changes file1 mode to 211
    Then file1 mode is 211
    Then clean succeeds

  Scenario: Increase regular file size
    When u1 creates regular files [file1]
    When u1 changes file1 size to 1000000 bytes
    Then file1 size is 1000000 bytes
    Then clean succeeds

  Scenario: Decrease regular file size
    When u1 creates regular files [file1]
    When u1 changes file1 size to 1000000 bytes
    When last operation succeeds
    When u1 changes file1 size to 0 bytes
    Then file1 size is 0 bytes
    Then clean succeeds

  Scenario: Timestamps at creation
    When u1 creates regular files [file1]
    Then modification time of file1 is equal to access time
    Then status-change time of file1 is equal to access time
    Then clean succeeds

  Scenario: Access time
    When u1 writes "TEST TEXT ONEDATA" to file1
    When user waits 1 seconds
    # call sleep, to be sure that time of write and read is different
    Then u1 reads "TEST TEXT ONEDATA" from file1
    Then access time of file1 is greater than modification time
    Then access time of file1 is greater than status-change time
    Then clean succeeds

  Scenario: Modification time
    When u1 creates regular files [file1]
    When user waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    When u1 writes "TEST TEXT ONEDATA" to file1
    Then last operation succeeds
    Then modification time of file1 is greater than access time
    Then modification time of file1 is equal to status-change time
    Then clean succeeds

  Scenario: Status-change time when changing mode
    When u1 creates regular files [file1]
    When user waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    When u1 changes file1 mode to 211
    Then last operation succeeds
    Then status-change time of file1 is greater than modification time
    Then status-change time of file1 is greater than access time
    Then clean succeeds

  Scenario: Status-change time when renaming
    When u1 creates regular files [file1]
    When user waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    When u1 renames file1 to file2
    Then last operation succeeds
    Then status-change time of file2 is greater than modification time
    Then status-change time of file2 is greater than access time
    Then clean succeeds
