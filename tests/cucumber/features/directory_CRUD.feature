Feature: Directory_CRUD

  Background:
    Given environment is defined in env.json
    Given environment is up
    Given u1 mounts onedata spaces in /root/onedata using token
    Given we are in space s1

  Scenario: Create directory
    When u1 creates directories [dir1, dir2, dir3]
    Then last operation succeeds
    Then [dir1, dir2, dir3] are in ls .
    Then [dir1, dir2, dir3] are in ls spaces/s1
    Then clean succeeds

  Scenario: Rename directory
    When u1 creates directories [dir1]
    When u1 renames dir1 to dir2
    Then last operation succeeds
    Then [dir2] are in ls .
    Then [dir1] are not in ls .
    Then clean succeeds

  Scenario: Delete empty directory
    When u1 creates directories [dir1]
    When u1 deletes empty directories [dir1]
    Then last operation succeeds
    Then [dir1] are not in ls .
    Then clean succeeds

  Scenario: Child directories
    When u1 creates directory and parents [dir1/child1, dir1/child2, dir1/child3]
    Then last operation succeeds
    Then [child1, child2, child3] are in ls dir1
    Then clean succeeds

  Scenario: Child directories 2
    When u1 creates directory and parents [dir1/dir2/dir3/child1, dir1/dir2/child1, dir1/child1]
    Then last operation succeeds
    Then [dir2, child1] are in ls dir1
    Then [dir3, child1] are in ls dir1/dir2
    Then [child1] are in ls dir1/dir2/dir3
    Then clean succeeds

  Scenario: Duplication
    When u1 creates directories [dir1]
    When u1 creates directories [dir1]
    Then last operation fails
    Then clean succeeds

  Scenario: Duplication in spaces
    When u1 creates directories [dir1, spaces/s1/dir1]
    Then last operation fails
    Then clean succeeds

  Scenario: Delete empty directory and parents
    #rmdir -p dir1/dir2/dir3
    When u1 creates directory and parents [dir1/dir2/dir3]
    When u1 deletes empty directory and parents [dir1/dir2/dir3]
    Then last operation succeeds
    Then clean succeeds

  Scenario: Delete non-empty directory in wrong way
    #wrong way means using rmdir instead of rm -rf
    When u1 creates directories [dir1, dir1/child1]
    When u1 deletes empty directories [dir1]
    #dir1 is not empty, but we use step for empty dirs
    Then last operation fails
    Then clean succeeds

  Scenario: Delete non-empty directory
    When u1 creates directory and parents [dir1/child1, dir1/child2, dir2/dir3/child1]
    When u1 deletes non-empty directories [dir1, dir2]
    Then last operation succeeds
    Then [dir1] are not in ls .
    Then [dir2] are not in ls .
    Then clean succeeds

  Scenario: Move directory
    When u1 creates directory and parents [dir1/dir2/dir3, dir4/dir5]
    When u1 renames dir4/dir5 to dir1/dir2/dir3
    Then last operation succeeds
    Then [dir5] are not in ls dir4
    Then [dir5] are not in ls spaces/s1/dir4
    Then [dir1, dir4] are in ls .
    Then [dir1, dir4] are in ls spaces/s1
    Then [dir3] are in ls dir1/dir2
    Then [dir3] are in ls spaces/s1/dir1/dir2
    Then [dir5] are in ls dir1/dir2/dir3
    Then [dir5] are in ls spaces/s1/dir1/dir2/dir3
    Then clean succeeds

  Scenario: Copy directory
    When u1 creates directory and parents [dir1/dir2/dir3, dir4/dir5]
    When u1 copies directory dir4/dir5 to dir1/dir2/dir3
    Then last operation succeeds
    Then [dir1, dir4] are in ls .
    Then [dir3] are in ls dir1/dir2
    Then [dir4] are in ls dir1/dir2/dir3
    Then [dir5] are in ls dir1/dir2/dir3/dir4
    Then [dir5] are in ls dir4
    Then clean succeeds

  Scenario: Move directory to itself
    When u1 creates directories [dir1]
    When u1 renames dir1 to dir1
    Then last operation fails
    Then [dir1] are in ls .
    Then [dir1] are in ls spaces/s1
    Then clean succeeds

    Scenario: Move directory to its subtree
    When u1 creates directory and parents [dir1/dir2/dir3]
    When u1 renames dir1 to dir1/dir2/dir3
    Then last operation fails
    Then [dir1] are in ls .
    Then [dir1] are in ls spaces/s1
    Then clean succeeds

  Scenario: Move directory to itself in spaces
    When u1 creates directories [dir1]
    When u1 renames dir1 to spaces/s1/dir1
    Then last operation fails
    Then [dir1] are in ls .
    Then [dir1] are in ls spaces/s1
    Then clean succeeds

  Scenario: Move directory to its subtree in spaces
    When u1 creates directory and parents [dir1/dir2/dir3]
    When u1 renames dir1 to spaces/s1/dir1/dir2/dir3
    Then last operation fails
    Then [dir1] are in ls .
    Then [dir1] are in ls spaces/s1
    Then clean succeeds