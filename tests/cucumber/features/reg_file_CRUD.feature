Feature: Regular_file_CRUD

  Background:
    Given environment is up
    And u1 starts oneclient in /home/u1/onedata using token

  Scenario: Create regular file
    When u1 creates regular files [s1/file1, s1/file2, s1/file3]
    Then u1 sees [file1, file2, file3] in s1

  Scenario: Rename regular file
    When u1 creates regular files [s1/file1]
    And u1 sees [file1] in s1
    And u1 renames file1 to s1/file2
    Then u1 sees [file2] in s1
    And u1 doesn't see [file1] in s1

  Scenario: Delete regular file
    When u1 creates regular files [s1/file1]
    And u1 sees [file1] in s1
    And u1 deletes files [s1/file1]
    Then u1 doesn't see [file1] in s1

  Scenario: Read and write to regular file
    When u1 creates regular files [s1/file1]
    And u1 writes "TEST TEXT ONEDATA" to s1/file1
    Then u1 reads "TEST TEXT ONEDATA" from s1/file1
    And size of u1's s1/file1 is 17 bytes

  Scenario: Append regular file
    When u1 creates regular files [s1/file1]
    And u1 writes "TEST TEXT ONEDATA" to s1/file1
    Then u1 reads "TEST TEXT ONEDATA" from s1/file1
    And u1 appends " APPENDED TEXT" to s1/file1
    Then u1 reads "TEST TEXT ONEDATA APPENDED TEXT" from s1/file1
    And size of u1's s1/file1 is 31 bytes

  Scenario: Replace word in file
    When u1 creates regular files [s1/file1]
    And u1 writes "TEST ONEDATA TEST ONEDATA2 TEST ONEDATA3" to s1/file1
    And u1 replaces "TEST" with "SYSTEM" in s1/file1
    Then u1 reads "SYSTEM ONEDATA SYSTEM ONEDATA2 SYSTEM ONEDATA3" from s1/file1
    And size of u1's s1/file1 is 46 bytes

  Scenario: Move regular file and read
    When u1 creates directory and parents [s1/dir1/dir2, s1/dir3]
    And u1 creates regular files [s1/dir1/dir2/file1]
    And u1 sees [file1] in s1/dir1/dir2
    And u1 writes "TEST TEXT ONEDATA" to s1/dir1/dir2/file1
    And u1 reads "TEST TEXT ONEDATA" from s1/dir1/dir2/file1
    And u1 renames s1/dir1/dir2/file1 to s1/dir3/file1
    Then u1 doesn't see [file1] in s1/dir1/dir2
    And u1 sees [file1] in s1/dir3
    And u1 reads "TEST TEXT ONEDATA" from s1/dir3/file1
    And size of u1's s1/dir3/file1 is 17 bytes

  Scenario: Move big regular file and check MD5
    When u1 creates directory and parents [s1/dir1/dir2, s1/dir3]
    And u1 creates regular files [s1/dir1/dir2/file1]
    And u1 sees [file1] in s1/dir1/dir2
    And u1 writes 32 MB of random characters to s1/dir1/dir2/file1 and saves MD5
    And u1 renames s1/dir1/dir2/file1 to s1/dir3/file1
    Then u1 doesn't see [file1] in s1/dir1/dir2
    And u1 sees [file1] in s1/dir3
    And u1 checks MD5 of s1/dir3/file1

  Scenario: Copy regular file and read
    When u1 creates directory and parents [s1/dir1/dir2, s1/dir3]
    And u1 creates regular files [s1/dir1/dir2/file1]
    And u1 sees [file1] in s1/dir1/dir2
    And u1 writes "TEST TEXT ONEDATA" to s1/dir1/dir2/file1
    And u1 reads "TEST TEXT ONEDATA" from s1/dir1/dir2/file1
    And u1 copies regular file s1/dir1/dir2/file1 to s1/dir3
    Then u1 sees [s1/dir1, s1/dir3] in .
    And u1 sees [file1] in s1/dir1/dir2
    And u1 sees [file1] in s1/dir3
    And u1 reads "TEST TEXT ONEDATA" from s1/dir3/file1
    And size of u1's s1/dir3/file1 is 17 bytes


  Scenario: Copy big regular file and check MD5
    When u1 creates directory and parents [s1/dir1/dir2, s1/dir3]
    And u1 creates regular files [s1/dir1/dir2/file1]
    And u1 sees [file1] in s1/dir1/dir2
    And u1 writes 32 MB of random characters to s1/dir1/dir2/file1 and saves MD5
    And u1 copies regular file s1/dir1/dir2/file1 to s1/dir3/file1
    Then u1 sees [file1] in s1/dir1/dir2
    And u1 sees [file1] in s1/dir3
    And u1 checks MD5 of s1/dir3/file1
    And u1 checks MD5 of s1/dir1/dir2/file1
