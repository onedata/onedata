Feature: File_CRUD

  Background:
    Given environment is defined in env.json
    Given environment is up
    Given u1 mounts onedata spaces in /root/onedata using token
    Given we are in space s1
    
  Scenario: Create regular file
    When u1 creates regular files [file1, file2, file3]
    Then last operation succeeds
    Then [file1, file2, file3] are in ls .
    Then [file1, file2, file3] are in ls spaces/s1
    Then clean succeeds

    Scenario: Rename regular file
    When u1 creates regular files [file1]
    When u1 renames file1 to file2
    Then last operation succeeds
    Then [file2] are in ls .
    Then [file1] are not in ls .
    Then clean succeeds

  Scenario: Delete regular file
    When u1 creates regular files [file1]
    When u1 deletes files [file1]
    Then last operation succeeds
    Then [file1] are not in ls .
    Then clean succeeds
