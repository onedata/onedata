Feature: Multi_regular_file_CRUD

  Background:
    Given environment is defined in env.json
    And storage directories are empty
    And environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
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
    And u2 waits 5 seconds on client2 # wait for events handling
    And u2 reads "TEST TEXT ONEDATA" from file1 on client2
    And size of u1's file1 is 17 bytes on client1
    And size of u2's file1 is 17 bytes on client2

  Scenario: Read regular file without read permission
    When u1 creates directories [dir1] on client1
    And u1 creates regular files [dir1/file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to dir1/file1 on client1
    And u1 changes dir1/file1 mode to 620 on client1
    Then u1 reads "TEST TEXT ONEDATA" from dir1/file1 on client1
    And u1 waits 5 seconds on client1 # wait for events handling
    And u2 reads "TEST TEXT ONEDATA" from dir1/file1 on client2
    And last operation by u2 fails
    And size of u1's dir1/file1 is 17 bytes on client1
    And size of u2's dir1/file1 is 17 bytes on client2
    And last operation by u2 fails

  Scenario: Write to regular file with write permission
    When u1 creates directories [dir1] on client1
    And u1 creates regular files [dir1/file1] on client1
    And u1 changes dir1/file1 mode to 620 on client1
    And u2 writes "TEST TEXT ONEDATA" to dir1/file1 on client2
    Then u1 reads "TEST TEXT ONEDATA" from dir1/file1 on client1
    And u1 waits 5 seconds on client1 # wait for events handling
    And u2 reads "TEST TEXT ONEDATA" from dir1/file1 on client2
    And size of u1's dir1/file1 is 17 bytes on client1
    And size of u2's dir1/file1 is 17 bytes on client2

  Scenario: Write to regular file without write permission
    When u1 creates directories [dir1] on client1
    And u1 creates regular files [dir1/file1] on client1
    And u1 changes dir1/file1 mode to 600 on client1
    And u2 writes "TEST TEXT ONEDATA" to dir1/file1 on client2
    Then u1 reads "TEST TEXT ONEDATA" from dir1/file1 on client1
    And u1 waits 5 seconds on client1 # wait for events handling
    And u2 reads "TEST TEXT ONEDATA" from dir1/file1 on client2
    And size of u1's dir1/file1 is 17 bytes on client1
    And size of u2's dir1/file1 is 17 bytes on client2

  Scenario: Execute file with execute permission
    When u1 creates directories [dir1] on client1
    And u1 creates regular files [dir1/script.sh] on client1
    And u1 changes dir1/script.sh mode to 654 on client1
    And u1 writes "#!/usr/bin/env bash\n\necho TEST" to dir1/file1 on client1
    And u2 executes dir1/script.sh on client2
    And last operation by u2 succeeds

  Scenario: Execute file without execute permission
    When u1 creates directories [dir1] on client1
    And u1 creates regular files [dir1/script.sh] on client1
    And u1 writes "#!/usr/bin/env bash\n\necho TEST" to dir1/script.sh on client1
    Then u1 reads "#!/usr/bin/env bash\n\necho TEST" from dir1/script.sh on client1
    And u2 waits 5 seconds on client2 # wait for events handling
    And u2 executes dir1/script.sh on client2
    And last operation by u2 fails

  Scenario: Move regular file and read
    When u1 creates directory and parents [dir1/dir2, dir3] on client1
    And u1 creates regular files [dir1/dir2/file1] on client1
    And u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 writes "TEST TEXT ONEDATA" to dir1/dir2/file1 on client1
    And u1 renames dir1/dir2/file1 to dir3/file1 on client1
    Then u1 doesn't see [file1] in dir1/dir2 on client1
    And u2 doesn't see [file1] in dir1/dir2 on client2
    And u1 doesn't see [file1] in spaces/s1/dir1/dir2 on client1
    And u2 doesn't see [file1] in spaces/s1/dir1/dir2 on client2
    And u1 sees [file1] in dir3 on client1
    And u2 sees [file1] in dir3 on client2
    And u1 sees [file1] in spaces/s1/dir3 on client1
    And u2 sees [file1] in spaces/s1/dir3 on client2
    And u2 waits 5 seconds on client2 # wait for events handling
    And u1 reads "TEST TEXT ONEDATA" from dir3/file1 on client1
    And u1 reads "TEST TEXT ONEDATA" from spaces/s1/dir3/file1 on client1
    And u2 reads "TEST TEXT ONEDATA" from dir3/file1 on client2
    And u2 reads "TEST TEXT ONEDATA" from spaces/s1/dir3/file1 on client2

  Scenario: Move big regular file and check MD5
    When u1 creates directory and parents [dir1/dir2, dir3] on client1
    And u1 creates regular files [dir1/dir2/file1] on client1
    And u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 writes 32 MB of random characters to dir1/dir2/file1 on client1 and saves MD5
    And u1 renames dir1/dir2/file1 to dir3/file1 on client1
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
    When u1 copies regular file dir1/dir2/file1 to dir3/file1 on client1
    Then u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 sees [file1] in spaces/s1/dir1/dir2 on client1
    And u2 sees [file1] in spaces/s1/dir1/dir2 on client2
    And u1 sees [file1] in dir3 on client1
    And u2 sees [file1] in dir3 on client2
    And u1 sees [file1] in spaces/s1/dir3 on client1
    And u2 sees [file1] in spaces/s1/dir3 on client2
    And u2 waits 5 seconds on client2 # wait for events handling
    And u1 reads "TEST TEXT ONEDATA" from dir3/file1 on client1
    And u1 reads "TEST TEXT ONEDATA" from spaces/s1/dir3/file1 on client1
    And u2 reads "TEST TEXT ONEDATA" from dir3/file1 on client2
    And u2 reads "TEST TEXT ONEDATA" from spaces/s1/dir3/file1 on client2

  Scenario: Copy big regular file and check MD5
    When u1 creates directory and parents [dir1/dir2, dir3] on client1
    And u1 creates regular files [dir1/dir2/file1] on client1
    And u1 sees [file1] in dir1/dir2 on client1
    And u2 sees [file1] in dir1/dir2 on client2
    And u1 writes 32 MB of random characters to dir1/dir2/file1 on client1 and saves MD5
    And u1 copies regular file dir1/dir2/file1 to dir3/file1 on client1
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
