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

  Scenario: Read and write to regular file
    When u1 creates regular files [file1]
    When u1 writes "TEST TEXT ONEDATA" to file1
    Then last operation succeeds
    Then u1 reads "TEST TEXT ONEDATA" from file1
    Then clean succeeds

    Scenario: Move regular file
    When u1 creates directory and parents [dir1/dir2, dir4]
    When u1 creates regular files [dir1/dir2/file1]
    When u1 renames dir1/dir2/file1 to dir4/file1
    Then last operation succeeds
    Then [file1] are not in ls dir1/dir2
    Then [file1] are not in ls spaces/s1/dir1/dir2
    Then [file1] are in ls dir4
    Then [file1] are in ls spaces/s1/dir4
    Then clean succeeds

  Scenario: Copy regular file
    When u1 creates directory and parents [dir1/dir2, dir3]
    When u1 creates regular files [dir1/dir2/file1]
    When u1 copies regular file dir1/dir2/file1 to dir3
    Then last operation succeeds
    Then [file1] are in ls dir1/dir2
    Then [file1] are in ls dir3
    Then clean succeeds
