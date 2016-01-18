Feature: Regular_file_CRUD

  Background:
    Given environment is defined in env.json
    And environment is up
    And u1 starts oneclient in /home/u1/onedata using token
    
  Scenario: Create regular file
    When u1 creates regular files [file1, file2, file3]
    Then u1 sees [file1, file2, file3] in .
    And u1 sees [file1, file2, file3] in spaces/s1

  Scenario: Rename regular file
    When u1 creates regular files [file1]
    And u1 sees [file1] in .
    And u1 renames file1 to file2
    Then u1 sees [file2] in .
    And u1 doesn't see [file1] in .

  Scenario: Delete regular file
    When u1 creates regular files [file1]
    And u1 sees [file1] in .
    And u1 deletes files [file1]
    Then u1 doesn't see [file1] in .

  Scenario: Read and write to regular file
    When u1 creates regular files [file1]
    And u1 writes "TEST TEXT ONEDATA" to file1nv.
    Then u1 reads "TEST TEXT ONEDATA" from file1
    And u1 reads "TEST TEXT ONEDATA" from spaces/s1/file1
    And size of u1's file1 is 17 bytes
    And size of u1's spaces/s1/file1 is 17 bytes

  Scenario: Append regular file
    When u1 creates regular files [file1]
    And u1 writes "TEST TEXT ONEDATA" to file1
    Then u1 reads "TEST TEXT ONEDATA" from file1
    And u1 appends " APPENDED TEXT" to file1
    Then u1 reads "TEST TEXT ONEDATA APPENDED TEXT" from file1
    And u1 reads "TEST TEXT ONEDATA APPENDED TEXT" from spaces/s1/file1
    And size of u1's file1 is 31 bytes
    And size of u1's spaces/s1/file1 is 31 bytes

  Scenario: Replace word in file
    When u1 creates regular files [file1]
    And u1 writes "TEST ONEDATA TEST ONEDATA2 TEST ONEDATA3" to file1
    And u1 replaces "TEST" with "SYSTEM" in file1
    Then u1 reads "SYSTEM ONEDATA SYSTEM ONEDATA2 SYSTEM ONEDATA3" from file1
    Then u1 reads "SYSTEM ONEDATA SYSTEM ONEDATA2 SYSTEM ONEDATA3" from spaces/s1/file1
    And size of u1's file1 is 46 bytes
    And size of u1's spaces/s1/file1 is 46 bytes

  Scenario: Move regular file and read
    When u1 creates directory and parents [dir1/dir2, dir3]
    And u1 creates regular files [dir1/dir2/file1]
    And u1 sees [file1] in dir1/dir2
    And u1 writes "TEST TEXT ONEDATA" to dir1/dir2/file1
    And u1 renames dir1/dir2/file1 to dir3/file1
    Then u1 doesn't see [file1] in dir1/dir2
    And u1 doesn't see [file1] in spaces/s1/dir1/dir2
    And u1 sees [file1] in dir3
    And u1 sees [file1] in spaces/s1/dir3
    And u1 reads "TEST TEXT ONEDATA" from dir3/file1
    And u1 reads "TEST TEXT ONEDATA" from spaces/s1/dir3/file1
    And size of u1's dir3/file1 is 17 bytes
    And size of u1's spaces/s1/dir3/file1 is 17 bytes

  Scenario: Move big regular file and check MD5
    When u1 creates directory and parents [dir1/dir2, dir3]
    And u1 creates regular files [dir1/dir2/file1]
    And u1 sees [file1] in dir1/dir2
    And u1 writes 1 MB of random characters to dir1/dir2/file1 and saves MD5
    And u1 renames dir1/dir2/file1 to dir3/file1
    Then u1 doesn't see [file1] in dir1/dir2
    And u1 doesn't see [file1] in spaces/s1/dir1/dir2
    And u1 sees [file1] in dir3
    And u1 sees [file1] in spaces/s1/dir3
    And u1 checks MD5 of dir3/file1
    And u1 checks MD5 of spaces/s1/dir3/file1

  Scenario: Copy regular file and read
    When u1 creates directory and parents [dir1/dir2, dir3]
    And u1 creates regular files [dir1/dir2/file1]
    And u1 sees [file1] in dir1/dir2
    And u1 writes "TEST TEXT ONEDATA" to dir1/dir2/file1
    And u1 copies regular file dir1/dir2/file1 to dir3
    Then u1 sees [dir1, dir3] in .
    And u1 sees [file1] in dir1/dir2
    And u1 sees [file1] in dir3
    And u1 sees [file1] in spaces/s1/dir1/dir2
    And u1 sees [file1] in spaces/s1/dir3
    And u1 reads "TEST TEXT ONEDATA" from dir3/file1
    And u1 reads "TEST TEXT ONEDATA" from spaces/s1/dir3/file1
    And size of u1's dir3/file1 is 17 bytes
    And size of u1's spaces/s1/dir3/file1 is 17 bytes


  Scenario: Copy big regular file and check MD5
    When u1 creates directory and parents [dir1/dir2, dir3]
    And u1 creates regular files [dir1/dir2/file1]
    And u1 sees [file1] in dir1/dir2
    And u1 writes 1024 MB of random characters to dir1/dir2/file1 and saves MD5
    And u1 copies regular file dir1/dir2/file1 to dir3/file1
    Then u1 sees [file1] in dir1/dir2
    And u1 sees [file1] in dir3
    And u1 sees [file1] in spaces/s1/dir1/dir2
    And u1 sees [file1] in spaces/s1/dir3
    And u1 checks MD5 of dir3/file1
    And u1 checks MD5 of dir1/dir2/file1
    And u1 checks MD5 of spaces/s1/dir1/dir2/file1
    And u1 checks MD5 of spaces/s1/dir3/file1