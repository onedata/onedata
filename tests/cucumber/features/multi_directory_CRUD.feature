Feature: Multi_directory_CRUD

  Background:
    Given environment is defined in env.json
    And environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [token, token]

  Scenario: Create directory
    When u1 creates directories [dir1, dir2, dir3]
      on client1
    Then u1 sees [dir1, dir2, dir3] in . on client1
    And u1 sees [dir1, dir2, dir3] in spaces/s1 on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1, dir2, dir3] in . on client2
    And u2 sees [dir1, dir2, dir3] in spaces/s1 on client2

  Scenario: Create directory in default space
    When u1 creates directories [spaces/s1/dir1, spaces/s1/dir2, spaces/s1/dir3]
      on client1
    Then u1 sees [dir1, dir2, dir3] in . on client1
    And u1 sees [dir1, dir2, dir3] in spaces/s1 on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1, dir2, dir3] in . on client2
    And u2 sees [dir1, dir2, dir3] in spaces/s1 on client2

  Scenario: Create directory in non-default space
    When u2 creates directories [spaces/s2/dir1, spaces/s2/dir2, spaces/s2/dir3]
      on client2
    And u1 waits 10 seconds on client1
    Then u1 sees [dir1, dir2, dir3] in spaces/s2 on client1
    And u2 sees [dir1, dir2, dir3] in spaces/s2 on client2
    Then u1 doesn't see [dir1, dir2, dir3] in . on client1
    Then u2 doesn't see [dir1, dir2, dir3] in . on client2

  Scenario: Rename someone's directory without permission
    When u1 creates directories [dir1] on client1
    And last operation by u1 succeeds
    And u2 renames dir1 to dir2 on client2
    Then last operation by u2 fails

  Scenario: Rename someone's directory with permission
    When u1 creates directory and parents [dir1/child1] on client1
    And u1 changes dir1/child1 mode to 775 on client1
    And u2 waits 10 seconds on client2
    And u2 renames dir1/child1 to dir1/child2 on client2
    And last operation by u2 succeeds
    And u1 waits 10 seconds on client1
    Then u1 sees [child2] in dir1 on client1
    And u1 doesn't see [child1] in dir1 on client1
    And u2 sees [child2] in dir1 on client2
    And u2 doesn't see [child1] in dir1 on client2

  Scenario: Rename own directory
    When u1 creates directories [dir1] on client1
    And last operation by u1 succeeds
    And u1 renames dir1 to dir2 on client1
    Then u1 sees [dir2] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir2] in . on client2
    And u1 doesn't see [dir1] in . on client1
    And u2 doesn't see [dir1] in . on client2

  Scenario: Delete someone's empty directory
    When u2 creates directories [dir1] on client2
    And last operation by u2 succeeds
    And u1 waits 10 seconds on client1
    And u1 deletes empty directories [dir1] on client1
    Then last operation by u1 fails
    And u1 sees [dir1] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2

  Scenario: Delete own empty directory
    When u2 creates directories [dir1] on client2
    And last operation by u2 succeeds
    And u2 deletes empty directories [dir1] on client2
    And u1 waits 10 seconds on client1
    Then u1 doesn't see [dir1] in . on client1
    And u2 doesn't see [dir1] in . on client2

  Scenario: List directory without read permission
    When u2 creates directory and parents [dir1/dir2] on client2
    And u2 changes dir1/dir2 mode to 735 on client2
    Then last operation by u2 succeeds
    And u1 waits 10 seconds on client1
    And u1 can't list dir1/dir2 on client1

  Scenario: Create file in directory without write permission
    When u2 creates directories [dir1] on client2
    And u2 changes dir1 mode to 755 on client2
    And u1 waits 10 seconds on client1
    And u1 creates directories [dir1/dir2] on client1
    Then last operation by u1 fails

  Scenario: Create file in directory with write permission
    When u2 creates directories [dir1] on client2
    And u1 waits 10 seconds on client1
    And u1 creates directories [dir1/dir2] on client1
    And u2 waits 10 seconds on client2
    Then u2 sees [dir2] in dir1 on client2

  Scenario: Delete file in directory without write permission
    When u2 creates directory and parents [dir1/dir2] on client2
    And u2 changes dir1 mode to 755 on client2
    And u1 waits 10 seconds on client1
    And u1 deletes empty directories [dir1/dir2] on client1
    Then last operation by u1 fails

  Scenario: Delete file in directory with write permission
    When u2 creates directory and parents [dir1/dir2] on client2
    And u1 waits 10 seconds on client1
    And u1 deletes empty directories [dir1/dir2] on client1
    And u2 waits 10 seconds on client2
    Then u2 doesn't see [dir2] in dir1 on client2

  Scenario: Rename file in directory without write permission
    When u2 creates directory and parents [dir1/dir2] on client2
    And u2 changes dir1 mode to 755 on client2
    And u1 waits 10 seconds on client1
    And u1 renames file dir1/dir2 to dir1/dir3 on client1
    Then last operation by u1 fails

  Scenario: Rename file in directory with write permission
    When u2 creates directory and parents [dir1/dir2] on client2
    And u1 renames file dir1/dir2 to dir1/dir3 on client1
    And u2 waits 10 seconds on client2
    Then u2 sees [dir3] in dir1 on client2
    And u2 doesn't see [dir2] in dir1 on client2

  Scenario: Recreate directory deleted by other user
    When u1 creates directories [dir1] on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2
    And u1 deletes empty directories [dir1] on client1
    And u2 waits 10 seconds on client2
    And u2 doesn't see [dir1] in . on client2
    And u2 creates directories [dir1] on client2
    Then u2 sees [dir1] in . on client2
    And u1 waits 10 seconds on client2
    And u1 sees [dir1] in . on client1

  Scenario: Child directories
    When u1 creates directory and parents [dir1/child1, dir1/child2, dir1/child3]
      on client1
    Then u1 sees [child1, child2, child3] in dir1 on client1
    And u2 waits 10 seconds on client2
    And u2 sees [child1, child2, child3] in dir1 on client2

  Scenario: Child directories 2
    When u2 creates directory and parents [dir1/dir2/dir3/child1, dir1/dir2/child1, dir1/child1]
      on client2
    Then u1 waits 10 seconds on client1
    Then u1 sees [dir2, child1] in dir1 on client1
    And u2 sees [dir2, child1] in dir1 on client2
    And u1 sees [dir3, child1] in dir1/dir2 on client1
    And u2 sees [dir3, child1] in dir1/dir2 on client2
    And u1 sees [child1] in dir1/dir2/dir3 on client1
    And u2 sees [child1] in dir1/dir2/dir3 on client2

  Scenario: Duplication
    When u1 creates directories [dir1] on client1
    And u2 waits 10 seconds on client2
    And u2 creates directories [dir1] on client2
    Then last operation by u2 fails

  Scenario: Duplication in spaces
    When u2 creates directories [dir1] on client2
    And u1 waits 10 seconds on client1
    And u1 creates directories [spaces/s1/dir1] on client1
    Then last operation by u1 fails

  Scenario: Delete empty directory and parents
    #rmdir -p dir1/dir2/dir3
    When u1 creates directory and parents [dir1/dir2/dir3] on client1
    And u1 sees [dir1] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2
    And u1 sees [dir2] in dir1 on client1
    And u2 sees [dir2] in dir1 on client2
    And u1 sees [dir3] in dir1/dir2 on client1
    And u2 sees [dir3] in dir1/dir2 on client2
    And u2 deletes empty directory and parents [dir1/dir2/dir3] on client2
    Then last operation by u2 fails
    # u2 can't delete dir1 because sticky bit is set for onedata dir
    And u1 waits 10 seconds on client1
    And u1 sees [dir1] in . on client1
    And u1 doesn't see [dir2] in dir1 on client1

  Scenario: Delete non-empty directory in wrong way
    #wrong way means using rmdir instead of rm -rf
    When u2 creates directories [dir1, dir1/child1] on client2
    And u1 waits 10 seconds on client1
    And u1 sees [dir1] in . on client1
    And u2 sees [dir1] in . on client2
    And u1 sees [child1] in dir1 on client1
    And u2 sees [child1] in dir1 on client2
    And u1 deletes empty directories [dir1] on client1
    #dir1 is not empty, but we use step for empty dirs
    Then last operation by u1 fails
    And u1 sees [dir1] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2
    And u1 sees [child1] in dir1 on client1
    And u2 sees [child1] in dir1 on client2

  Scenario: Delete non-empty directory
    #rm -rf dir1 dir2
    When u1 creates directory and parents [dir1/child1, dir1/child2, dir2/dir3/child1]
      on client1
    And u1 sees [dir1, dir2] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1, dir2] in . on client2
    And u1 sees [child1, child2] in dir1 on client1
    And u2 sees [child1, child2] in dir1 on client2
    And u1 sees [dir3] in dir2 on client1
    And u2 sees [dir3] in dir2 on client2
    And u1 sees [child1] in dir2/dir3 on client1
    And u2 sees [child1] in dir2/dir3 on client2
    And u1 deletes non-empty directories [dir1, dir2] on client1
    Then u1 doesn't see [dir1, dir2] in . on client1
    And u2 waits 10 seconds on client2
    Then u2 doesn't see [dir1, dir2] in . on client2

  Scenario: Move directory
    When u1 creates directory and parents [dir1/dir2/dir3, dir4/dir5] on client1
    And u1 sees [dir1, dir4] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1, dir4] in . on client2
    And u1 sees [dir2] in dir1 on client1
    And u2 sees [dir2] in dir1 on client2
    And u1 sees [dir3] in dir1/dir2 on client1
    And u2 sees [dir3] in dir1/dir2 on client2
    And u1 sees [dir5] in dir4 on client1
    And u2 sees [dir5] in dir4 on client2
    And u1 renames dir4/dir5 to dir1/dir2/dir3 on client1
    Then u1 doesn't see [dir5] in dir4 on client1
    And u2 waits 10 seconds on client2
    And u2 doesn't see [dir5] in dir4 on client2
    And u1 sees [dir5] in dir1/dir2/dir3 on client1
    And u2 sees [dir5] in dir1/dir2/dir3 on client2

  Scenario: Copy directory
    When u1 creates directory and parents [dir1/dir2/dir3, dir4/dir5] on client1
    And u1 sees [dir1, dir4] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1, dir4] in . on client2
    And u1 sees [dir2] in dir1 on client1
    And u2 sees [dir2] in dir1 on client2
    And u1 sees [dir3] in dir1/dir2 on client1
    And u2 sees [dir3] in dir1/dir2 on client2
    And u1 sees [dir5] in dir4 on client1
    And u2 sees [dir5] in dir4 on client2
    And u1 copies directory dir4 to dir1/dir2/dir3 on client1
    Then u1 sees [dir4] in dir1/dir2/dir3 on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir4] in dir1/dir2/dir3 on client2
    And u1 sees [dir5] in dir1/dir2/dir3/dir4 on client1
    And u2 sees [dir5] in dir1/dir2/dir3/dir4 on client2
    And u1 sees [dir5] in dir4 on client1
    And u2 sees [dir5] in dir4 on client2

  Scenario: Move directory to itself
    When u1 creates directories [dir1] on client1
    And u1 sees [dir1] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2
    And u1 renames dir1 to dir1 on client1
    Then last operation by u1 fails
    And u1 sees [dir1] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2

  Scenario: Move directory to its subtree
    When u1 creates directory and parents [dir1/dir2/dir3] on client1
    And u1 sees [dir1] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2
    And u1 sees [dir2] in dir1 on client1
    And u2 sees [dir2] in dir1 on client2
    And u1 sees [dir3] in dir1/dir2 on client1
    And u2 sees [dir3] in dir1/dir2 on client2
    And u1 renames dir1 to dir1/dir2/dir3 on client1
    Then last operation by u1 fails
    And u1 sees [dir1] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2
    And u1 sees [dir2] in dir1 on client1
    And u2 sees [dir2] in dir1 on client2
    And u1 sees [dir3] in dir1/dir2 on client1
    And u2 sees [dir3] in dir1/dir2 on client2

  Scenario: Move directory to itself in spaces
    When u2 creates directory and parents [dir1/dir2] on client2
    And u2 changes dir1 mode to 775 on client2
    And u1 waits 10 seconds on client1
    And u1 sees [dir1] in . on client1
    And u2 sees [dir1] in . on client2
    And u1 renames dir1/dir2 to spaces/s1/dir1/dir2 on client1
    Then last operation by u1 fails
    And u1 sees [dir2] in dir1 on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir2] in dir1 on client2

  Scenario: Move directory to itself in default space
    When u2 creates directory and parents [spaces/s1/dir1/dir2] on client2
    And u2 changes spaces/s1/dir1 mode to 775 on client2
    And u1 waits 10 seconds on client1
    And u1 sees [dir2] in spaces/s1/dir1 on client1
    And u2 sees [dir2] in spaces/s1/dir1 on client2
    And u1 renames spaces/s1/dir1/dir2 to dir1/dir2 on client1
    Then last operation by u1 fails
    And u1 sees [dir2] in dir1 on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir2] in dir1 on client2

  Scenario: Move directory to its subtree in spaces
    When u1 creates directory and parents [dir1/dir2/dir3] on client1
    And u1 sees [dir1] in . on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in . on client2
    And u1 sees [dir2] in dir1 on client1
    And u2 sees [dir2] in dir1 on client2
    And u1 sees [dir3] in dir1/dir2 on client1
    And u2 sees [dir3] in dir1/dir2 on client2
    And u1 changes dir1 mode to 775 on client1
    And u1 changes dir1/dir2 mode to 775 on client1
    And u1 changes dir1/dir2/dir3 mode to 775 on client1
    And u2 renames dir1/dir2 to spaces/s1/dir1/dir2/dir3 on client2
    Then last operation by u2 fails
    And u1 waits 10 seconds on client1
    And u1 sees [dir1] in . on client1
    And u2 sees [dir1] in . on client2
    And u1 sees [dir2] in dir1 on client1
    And u2 sees [dir2] in dir1 on client2
    And u1 sees [dir3] in dir1/dir2 on client1
    And u2 sees [dir3] in dir1/dir2 on client2

  Scenario: Move directory to its subtree in default space
    When u1 creates directory and parents [spaces/s1/dir1/dir2/dir3] on client1
    And u1 sees [dir1] in spaces/s1 on client1
    And u2 waits 10 seconds on client2
    And u2 sees [dir1] in spaces/s1 on client2
    And u1 sees [dir2] in spaces/s1/dir1 on client1
    And u2 sees [dir2] in spaces/s1/dir1 on client2
    And u1 sees [dir3] in spaces/s1/dir1/dir2 on client1
    And u2 sees [dir3] in spaces/s1/dir1/dir2 on client2
    And u1 changes spaces/s1/dir1 mode to 775 on client1
    And u1 changes spaces/s1/dir1/dir2 mode to 775 on client1
    And u1 changes spaces/s1/dir1/dir2/dir3 mode to 775 on client1
    And u2 waits 10 seconds on client2
    And u2 renames spaces/s1/dir1/dir2 to dir1/dir2/dir3 on client2
    Then last operation by u2 fails
    And u1 waits 10 seconds on client1
    And u1 sees [dir1] in spaces/s1 on client1
    And u2 sees [dir1] in spaces/s1 on client2
    And u1 sees [dir2] in spaces/s1/dir1 on client1
    And u2 sees [dir2] in spaces/s1/dir1 on client2
    And u1 sees [dir3] in spaces/s1/dir1/dir2 on client1
    And u2 sees [dir3] in spaces/s1/dir1/dir2 on client2