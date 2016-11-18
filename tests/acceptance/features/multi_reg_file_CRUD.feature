Feature: Multi_regular_file_CRUD

  Background:
    Given environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]

  Scenario: Create regular file
    When u1 creates regular files [s1/file1, s1/file2, s1/file3] on client1
    Then u1 sees [file1, file2, file3] in s1 on client1
    And u2 sees [file1, file2, file3] in s1 on client2

  Scenario: Create many children
    When u1 creates children files of s1 with names in range [1, 127) on client1
    Then u2 lists only children of s1 with names in range [1, 127) on client2

  Scenario: Rename regular file without permission
    When u1 creates regular files [s1/file1] on client1
    And u1 sees [file1] in s1 on client1
    And u2 sees [file1] in s1 on client2
    And u2 fails to rename s1/file1 to s1/file2 on client2

  Scenario: Rename regular file with permission
    When u1 creates directories [s1/dir1] on client1
    And u1 creates regular files [s1/dir1/file1] on client1
    And u1 changes s1/dir1 mode to 775 on client1
    And u1 sees [file1] in s1/dir1 on client1
    And u2 sees [file1] in s1/dir1 on client2
    And u2 renames s1/dir1/file1 to s1/dir1/file2 on client2
    Then u1 sees [file2] in s1/dir1 on client1
    And u1 sees [file2] in s1/dir1 on client1
    And u2 sees [file2] in s1/dir1 on client2
    And u1 doesn't see [file1] in s1/dir1 on client1
    And u2 doesn't see [file1] in s1/dir1 on client2

  Scenario: Delete regular file by owner
    When u1 creates regular files [s1/file1] on client1
    And u1 sees [file1] in s1 on client1
    And u2 sees [file1] in s1 on client2
    And u1 deletes files [s1/file1] on client1
    Then u1 doesn't see [file1] in s1 on client1
    And u2 doesn't see [file1] in s1 on client2

  Scenario: Delete regular file by other user
    When u1 creates regular files [s1/file1] on client1
    And u1 sees [file1] in s1 on client1
    And u2 sees [file1] in s1 on client2
    And u2 fails to delete files [s1/file1] on client2
    Then u1 sees [file1] in s1 on client1
    And u2 sees [file1] in s1 on client2

  Scenario: Read and write to regular file
    When u1 creates regular files [s1/file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    Then u1 reads "TEST TEXT ONEDATA" from file s1/file1 on client1
    # TODO delete below sleep after resolving 2816
    And u2 waits 5 seconds
    And u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    And size of u1's s1/file1 is 17 bytes on client1
    And size of u2's s1/file1 is 17 bytes on client2

  Scenario: Read right after write by other client
    When u1 creates regular files [s1/file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    And u2 reads "TEST TEXT ONEDATA" from file s1/file1 on client2
    Then u1 reads "TEST TEXT ONEDATA" from file s1/file1 on client1
    And size of u1's s1/file1 is 17 bytes on client1
    And size of u2's s1/file1 is 17 bytes on client2

  Scenario: Read regular file without read permission
    When u1 creates directories [s1/dir1] on client1
    And u1 creates regular files [s1/dir1/file1] on client1
    And u1 writes "TEST TEXT ONEDATA" to s1/dir1/file1 on client1
    # TODO delete below sleep after resolving 2816
    Then u2 waits 5 seconds
    Then u2 reads "TEST TEXT ONEDATA" from file s1/dir1/file1 on client2
    And u1 changes s1/dir1/file1 mode to 620 on client1
    And u1 reads "TEST TEXT ONEDATA" from file s1/dir1/file1 on client1
    And mode of u2's s1/dir1/file1 is 620 on client2
    And u2 cannot read from s1/dir1/file1 on client2
    And size of u1's s1/dir1/file1 is 17 bytes on client1

  Scenario: Write to regular file with write permission
    When u1 creates directories [s1/dir1] on client1
    And u1 creates regular files [s1/dir1/file1] on client1
    And u1 changes s1/dir1/file1 mode to 660 on client1
    And mode of u2's s1/dir1/file1 is 660 on client2
    And u2 writes "TEST TEXT ONEDATA" to s1/dir1/file1 on client2
    Then u2 reads "TEST TEXT ONEDATA" from file s1/dir1/file1 on client2
    # TODO delete below sleep after resolving 2816
    And u1 waits 5 seconds
    And u1 reads "TEST TEXT ONEDATA" from file s1/dir1/file1 on client1
    And size of u1's s1/dir1/file1 is 17 bytes on client1
    And size of u2's s1/dir1/file1 is 17 bytes on client2

  Scenario: Write to regular file without write permission
    When u1 creates directories [s1/dir1] on client1
    And u1 creates regular files [s1/dir1/file1] on client1
    And u1 changes s1/dir1/file1 mode to 600 on client1
    Then mode of u2's s1/dir1/file1 is 600 on client2
    And u2 fails to write "TEST TEXT ONEDATA" to s1/dir1/file1 on client2
    And u1 sees [file1] in s1/dir1 on client1
    And u1 reads "" from file s1/dir1/file1 on client1

  Scenario: Execute file with execute permission
    When u1 creates directories [s1/dir1] on client1
    And u1 creates regular files [s1/dir1/script.sh] on client1
    And u1 changes s1/dir1/script.sh mode to 654 on client1
    And mode of u2's s1/dir1/script.sh is 654 on client2
    And u1 writes "#!/usr/bin/env bash\n\necho TEST" to s1/dir1/script.sh on client1
    # TODO delete below sleep after resolving 2816
    And u2 waits 5 seconds
    And u2 reads "#!/usr/bin/env bash\n\necho TEST" from file s1/dir1/script.sh on client2
    And u2 executes s1/dir1/script.sh on client2

  Scenario: Execute file without execute permission
    When u1 creates directories [s1/dir1] on client1
    And u1 creates regular files [s1/dir1/script.sh] on client1
    And u1 writes "#!/usr/bin/env bash\n\necho TEST" to s1/dir1/script.sh on client1
    # TODO delete below sleep after resolving 2816
    And u2 waits 5 seconds
    And u2 reads "#!/usr/bin/env bash\n\necho TEST" from file s1/dir1/script.sh on client2
    And u2 fails to execute s1/dir1/script.sh on client2

  Scenario: Move regular file and read
    When u1 creates directory and parents [s1/dir1/dir2, s1/dir3] on client1
    And u1 creates regular files [s1/dir1/dir2/file1] on client1
    And u1 sees [dir3] in s1 on client1
    And u1 sees [file1] in s1/dir1/dir2 on client1
    And u2 sees [dir3] in s1 on client2
    And u2 sees [file1] in s1/dir1/dir2 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/dir1/dir2/file1 on client1
    And u1 renames s1/dir1/dir2/file1 to s1/dir3/file1 on client1
    Then u1 doesn't see [file1] in s1/dir1/dir2 on client1
    And u2 doesn't see [file1] in s1/dir1/dir2 on client2
    And u1 sees [file1] in s1/dir3 on client1
    And u2 sees [file1] in s1/dir3 on client2
    And u1 reads "TEST TEXT ONEDATA" from file s1/dir3/file1 on client1
    And u2 reads "TEST TEXT ONEDATA" from file s1/dir3/file1 on client2

  Scenario: Move big regular file and check MD5
    When u1 creates directory and parents [s1/dir1/dir2, s1/dir3] on client1
    And u1 creates regular files [s1/dir1/dir2/file1] on client1
    And u1 sees [file1] in s1/dir1/dir2 on client1
    And u2 sees [file1] in s1/dir1/dir2 on client2
    And u1 writes 32 MB of random characters to s1/dir1/dir2/file1 on client1 and saves MD5
    And u1 renames s1/dir1/dir2/file1 to s1/dir3/file1 on client1
    Then u1 doesn't see [file1] in s1/dir1/dir2 on client1
    And u2 doesn't see [file1] in s1/dir1/dir2 on client2
    And u1 sees [file1] in s1/dir3 on client1
    And u2 sees [file1] in s1/dir3 on client2
    And u1 checks MD5 of s1/dir3/file1 on client1
    And u2 checks MD5 of s1/dir3/file1 on client2

  Scenario: Copy regular file and read
    When u1 creates directory and parents [s1/dir1/dir2, s1/dir3] on client1
    And u1 creates regular files [s1/dir1/dir2/file1] on client1
    And u1 sees [file1] in s1/dir1/dir2 on client1
    And u2 sees [file1] in s1/dir1/dir2 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/dir1/dir2/file1 on client1
    When u1 copies regular file s1/dir1/dir2/file1 to s1/dir3/file1 on client1
    Then u1 sees [file1] in s1/dir1/dir2 on client1
    And u2 sees [file1] in s1/dir1/dir2 on client2
    And u1 sees [file1] in s1/dir3 on client1
    And u2 sees [file1] in s1/dir3 on client2
    And u1 reads "TEST TEXT ONEDATA" from file s1/dir3/file1 on client1
    And u2 reads "TEST TEXT ONEDATA" from file s1/dir3/file1 on client2

  Scenario: Copy big regular file and check MD5
    When u1 creates directory and parents [s1/dir1/dir2, s1/dir3] on client1
    And u1 creates regular files [s1/dir1/dir2/file1] on client1
    And u1 sees [file1] in s1/dir1/dir2 on client1
    And u2 sees [file1] in s1/dir1/dir2 on client2
    And u1 writes 32 MB of random characters to s1/dir1/dir2/file1 on client1 and saves MD5
    And u1 copies regular file s1/dir1/dir2/file1 to s1/dir3/file1 on client1
    Then u1 sees [file1] in s1/dir1/dir2 on client1
    And u2 sees [file1] in s1/dir1/dir2 on client2
    And u1 sees [file1] in s1/dir3 on client1
    And u2 sees [file1] in s1/dir3 on client2
    And u1 checks MD5 of s1/dir3/file1 on client1
    And u2 checks MD5 of s1/dir3/file1 on client2

  Scenario: Deleting file opened by other user for reading
    When u1 creates directories [s1/dir1] on client1
    And u1 creates regular files [s1/dir1/file1] on client1
    And u1 sees [file1] in s1/dir1 on client1
    And u2 sees [file1] in s1/dir1 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/dir1/file1 on client1
    And u1 reads "TEST TEXT ONEDATA" from file s1/dir1/file1 on client1
    And u1 changes s1/dir1 mode to 777 on client1
    Then mode of u2's s1/dir1 is 777 on client2
    And u1 opens s1/dir1/file1 with mode r on client1
    And u2 deletes files [s1/dir1/file1] on client2
    And u2 doesn't see [file1] in s1/dir1 on client2
    And u1 doesn't see [file1] in s1/dir1 on client1
    And u1 reads "TEST TEXT ONEDATA" from previously opened file s1/dir1/file1 on client1
    And u1 closes s1/dir1/file1 on client1

  Scenario: Deleting file opened by other user for reading and writing
    When u1 creates directories [s1/dir1] on client1
    And u1 creates regular files [s1/dir1/file1] on client1
    And u1 sees [file1] in s1/dir1 on client1
    And u2 sees [file1] in s1/dir1 on client2
    And u1 changes s1/dir1 mode to 777 on client1
    Then mode of u2's s1/dir1 is 777 on client2
    And u1 opens s1/dir1/file1 with mode r+ on client1
    And u2 deletes files [s1/dir1/file1] on client2
    And u1 writes "TEST TEXT ONEDATA" to previously opened s1/dir1/file1 on client1
    And u1 sets current file position in s1/dir1/file1 at offset 0 on client1
    And u1 reads "TEST TEXT ONEDATA" from previously opened file s1/dir1/file1 on client1
    And u1 closes s1/dir1/file1 on client1
    And u1 doesn't see [file1] in s1/dir1 on client1

  Scenario: Deleting file without permission, file is opened by other user
    When u1 creates regular files [s1/file1] on client1
    And u1 sees [file1] in s1 on client1
    And u2 sees [file1] in s1 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    And u1 opens s1/file1 with mode r+ on client1
    And u2 fails to delete files [s1/file1] on client2
    # because u2 has no write permission to file1
    And u1 closes s1/file1 on client1
    # TODO below sleep should be deleted after resolving VFS-2828
    And u1 waits 10 seconds
    And u1 sees [file1] in s1 on client1
    And u1 reads "TEST TEXT ONEDATA" from file s1/file1 on client1

  Scenario: Deleting file right after closing it
    When u1 creates regular files [s1/file1] on client1
    And u1 sees [file1] in s1 on client1
    And u2 sees [file1] in s1 on client2
    And u1 writes "TEST TEXT ONEDATA" to s1/file1 on client1
    And u1 opens s1/file1 with mode r+ on client1
#    And u2 fails to delete files [s1/file1] on client2
    # because u2 has no write permission to file1
    And u1 closes s1/file1 on client1
    And u1 deletes files [s1/file1] on client1
    And s1 is empty for u1 on client1

