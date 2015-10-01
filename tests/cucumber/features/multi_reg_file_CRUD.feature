Feature: Regular_file_CRUD

  Background:
    Given environment is defined in env.json
    And environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/root/onedata1, /root/onedata2] on nodes [1, 2] respectively,
      using [token, token]

  Scenario: Create regular file
    When u1 creates regular files [file1, file2, file3] on client1
    Then u1 sees [file1, file2, file3] in . on client1
    And u2 sees [file1, file2, file3] in . on client2
    And u1 sees [file1, file2, file3] in spaces/s1 on client1
    And u2 sees [file1, file2, file3] in spaces/s1 on client2

  Scenario: Rename regular file
    When u1 creates regular files [file1] on client1
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u2 renames file1 to file2 on client2
    Then last operation by u2 fails
    And u1 sees [file2] in . on client1
    And u1 sees [file2] in spaces/s1 on client1
    And u2 sees [file2] in . on client2
    And u2 sees [file2] in spaces/s1 on client2
    And u1 doesn't see [file1] in . on client1
    And u1 doesn't see [file1] in spaces/s1 on client1
    And u2 doesn't see [file1] in . on client2
    And u2 doesn't see [file1] in spaces/s1 on client2

  Scenario: Delete regular file
    When u2 creates regular files [file1] on client2
    And u1 sees [file1] in . on client1
    And u2 sees [file1] in . on client2
    And u2 deletes files [file1] on client2
    Then u1 doesn't see [file1] in . on client1
    And u1 doesn't see [file1] in spaces/s1 on client1
    And u2 doesn't see [file1] in . on client2
    And u2 doesn't see [file1] in spaces/s1 on client2

  Scenario: Read and write to regular file
    When u1 creates regular files [file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to file1 on client1
    Then u1 reads "TEST TEXT ONEDATA" from file1 on client1
    And u2 reads "TEST TEXT ONEDATA" from file1 on client2
    And u1 checks if file1 size is 18 bytes on client1
    And u2 checks if file1 size is 18 bytes on client2

  Scenario: Move regular file and read
    When u1 creates directory and parents [dir1/dir2, dir3] on client1
    And u1 creates regular files [dir1/dir2/file1] on client1
    And u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 writes "TEST TEXT ONEDATA" to dir1/dir2/file1 on client1
    When u1 renames dir1/dir2/file1 to dir3/file1 on client1
    Then u1 doesn't see [file1] in dir1/dir2 on client1
    And u2 doesn't see [file1] in dir1/dir2 on client2
    And u1 doesn't see [file1] in spaces/s1/dir1/dir2 on client1
    And u2 doesn't see [file1] in spaces/s1/dir1/dir2 on client2
    And u1 sees [file1] in dir3 on client1
    And u2 sees [file1] in dir3 on client2
    And u1 sees [file1] in spaces/s1/dir3 on client1
    And u2 sees [file1] in spaces/s1/dir3 on client2
    And u1 reads "TEST TEXT ONEDATA" from file1 on client1
    And u1 reads "TEST TEXT ONEDATA" from spaces/s1/file1 on client1
    And u2 reads "TEST TEXT ONEDATA" from file1 on client2
    And u2 reads "TEST TEXT ONEDATA" from spaces/s1/file1 on client2
    
  Scenario: Move big regular file and check MD5
    When u1 creates directory and parents [dir1/dir2, dir3] on client1
    And u1 creates regular files [dir1/dir2/file1] on client1
    And u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 writes 1 MB of random characters to dir1/dir2/file1 on client1 and saves MD5
    And u1 renames dir1/dir2/file1 to dir3/file1
    Then u1 doesn't see [file1] in dir1/dir2 on client1
    And u1 doesn't see [file1] in spaces/s1/dir1/dir2 on client1
    And u2 doesn't see [file1] in dir1/dir2 on client2
    And u2 doesn't see [file1] in spaces/s1/dir1/dir2 on client2 
    And u1 sees [file1] in dir3 on client1
    And u1 sees [file1] in spaces/s1/dir3 on client1
    And u2 sees [file1] in dir3 on client2
    And u2 sees [file1] in spaces/s1/dir3 on client2
    And u1 checks MD5 of dir3/file1 on client1
    And u1 checks MD5 of spaces/s1/dir3/file1 on client1
    And u2 checks MD5 of dir3/file1 on client2
    And u2 checks MD5 of spaces/s1/dir3/file1 on client2

  Scenario: Copy regular file and read
    When u1 creates directory and parents [dir1/dir2, dir3] on client1
    And u1 creates regular files [dir1/dir2/file1] on client1
    And u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 writes "TEST TEXT ONEDATA" to dir1/dir2/file1 on client1
    When u1 copiers dir1/dir2/file1 to dir3/file1 on client1
    Then u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 sees [file1] in spaces/s1/dir1/dir2 on client1
    And u2 sees [file1] in spaces/s1/dir1/dir2 on client2
    And u1 sees [file1] in dir3 on client1
    And u2 sees [file1] in dir3 on client2
    And u1 sees [file1] in spaces/s1/dir3 on client1
    And u2 sees [file1] in spaces/s1/dir3 on client2
    And u1 reads "TEST TEXT ONEDATA" from file1 on client1
    And u1 reads "TEST TEXT ONEDATA" from spaces/s1/file1 on client1
    And u2 reads "TEST TEXT ONEDATA" from file1 on client2
    And u2 reads "TEST TEXT ONEDATA" from spaces/s1/file1 on client2
  Scenario: Copy big regular file and check MD5
    When u1 creates directory and parents [dir1/dir2, dir3] on client1
    And u1 creates regular files [dir1/dir2/file1] on client1
    And u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 writes 1024 MB of random characters to dir1/dir2/file1 on client1 and saves MD5
#    TODO check size ????
    And u1 copies dir1/dir2/file1 to dir3/file1
    Then u1 sees [file1] in dir1/dir2 on client1
    And u1 sees [file1] in spaces/s1/dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u2 sees [file1] in spaces/s1/dir1/dir2 on client2 
    And u1 sees [file1] in dir3 on client1
    And u1 sees [file1] in spaces/s1/dir3 on client1
    And u2 sees [file1] in dir3 on client2
    And u2 sees [file1] in spaces/s1/dir3 on client2
    And u1 checks MD5 of dir3/file1 on client1
    And u1 checks MD5 of spaces/s1/dir3/file1 on client1
    And u2 checks MD5 of dir3/file1 on client2
    And u2 checks MD5 of spaces/s1/dir3/file1 on client2