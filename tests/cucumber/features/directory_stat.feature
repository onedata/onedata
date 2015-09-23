Feature: Directory_stat

  Background:
    Given environment is defined in env.json
    Given environment is up
    Given u1 mounts onedata spaces in /root/onedata using token
    Given we are in space s1

  Scenario: Check file type
    When u1 creates directories [dir1]
    Then dir1 file type is directory
    Then last operation succeeds
    Then clean succeeds

  Scenario: Check default access permissions
    When u1 creates directories [dir1]
    Then dir1 mode is 755
    Then clean succeeds

  Scenario: Change access permissions
    When u1 creates directories [dir1]
    When u1 changes dir1 mode to 211
    Then last operation succeeds
    Then dir1 mode is 211
    Then clean succeeds

  Scenario: Timestamps at creation
    When u1 creates directories [dir1]
    Then modification time of dir1 is equal to access time
    Then status-change time of dir1 is equal to access time
    Then clean succeeds

  Scenario: Access time
    When u1 creates directories [dir1]
    Then [file] are not in ls .
    Then access time of file1 is greater than modification time
    Then access time of file1 is greater than status-change time
    Then clean succeeds

  Scenario: Modification time when creating child
    When u1 creates directories [dir1]
    When u1 creates files [dir1/file1]
    Then modification time of file1 is greater than access time
    Then modification time of file1 is equal to status-change time
    Then clean succeeds

  Scenario: Status-change time when changing mode
    When u1 creates directories [dir1]
    When u1 changes directory dir1 to dir2
    Then status-change time of file1 is greater than modification time
    Then status-change time of file1 is greater than access time
    Then clean succeeds
    
  Scenario: Status-change time when renaming
    When u1 creates directories [dir1]
    When u1 changes dir1 mode to 211
    Then status-change time of dir2 is greater than modification time
    Then status-change time of dir2 is greater than access time
    Then clean succeeds

