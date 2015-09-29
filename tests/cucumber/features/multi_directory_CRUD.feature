Feature: Directory_CRUD

  Background:
    Given environment is defined in env.json
    And environment is up
    And u1 mounts onedata spaces in /root/onedata using token
    And we are in space s1

  Scenario: Create directory
    When u1 creates directories [dir1, dir2, dir3]
    Then [dir1, dir2, dir3] are in ls .
    And [dir1, dir2, dir3] are in ls spaces/s1
    And clean succeeds

  Scenario: Create directory in default space
    When u1 creates directories [spaces/s1/dir1, spaces/s1/dir2, spaces/s1/dir3]
    Then [dir1, dir2, dir3] are in ls spaces/s1
    And [dir1, dir2, dir3] are in ls .
    And clean succeeds

  Scenario: Create directory in non-default space
    When u1 creates directories [spaces/s2/dir1, spaces/s2/dir2, spaces/s2/dir3]
    Then [dir1, dir2, dir3] are in ls spaces/s2
    And [dir1, dir2, dir3] are not in ls .
    And clean succeeds

  Scenario: Create directory spaces
    When u1 creates directories [spaces]
    Then last operation fails
    And clean succeeds

  Scenario: Create space
    When u1 creates directories [spaces/s1]
    Then last operation fails
    And clean succeeds

  Scenario: Rename directory
    When u1 creates directories [dir1]
    When last operation succeeds
    And u1 renames dir1 to dir2
    Then [dir2] are in ls .
    And [dir1] are not in ls .
    And clean succeeds

  Scenario: Delete empty directory
    When u1 creates directories [dir1]
    When last operation succeeds
    And u1 deletes empty directories [dir1]
    Then last operation succeeds
    And [dir1] are not in ls .
    And clean succeeds

  Scenario: Delete directory spaces
    When u1 deletes non-empty directories [spaces]
    Then last operation fails
    And clean succeeds

  Scenario: Delete space
    When u1 deletes non-empty directories [spaces/s1]
    Then last operation fails
    And clean succeeds

  Scenario: Child directories
    When u1 creates directory and parents [dir1/child1, dir1/child2, dir1/child3]
    Then [child1, child2, child3] are in ls dir1
    And clean succeeds

  Scenario: Child directories 2
    When u1 creates directory and parents [dir1/dir2/dir3/child1, dir1/dir2/child1, dir1/child1]
    Then [dir2, child1] are in ls dir1
    And [dir3, child1] are in ls dir1/dir2
    And [child1] are in ls dir1/dir2/dir3
    And clean succeeds

  Scenario: Duplication
    When u1 creates directories [dir1]
    And u1 creates directories [dir1]
    Then last operation fails
    And clean succeeds

  Scenario: Duplication in spaces
    When u1 creates directories [dir1]
    And u1 creates directories [spaces/s1/dir1]
    Then last operation fails
    And clean succeeds

  Scenario: Delete empty directory and parents
    #rmdir -p dir1/dir2/dir3
    When u1 creates directory and parents [dir1/dir2/dir3]
    When last operation succeeds
    And u1 deletes empty directory and parents [dir1/dir2/dir3]
    Then last operation succeeds
    And clean succeeds

  Scenario: Delete non-empty directory in wrong way
    #wrong way means using rmdir instead of rm -rf
    When u1 creates directories [dir1, dir1/child1]
    When last operation succeeds
    And u1 deletes empty directories [dir1]
    #dir1 is not empty, but we use step for empty dirs
    Then last operation fails
    And clean succeeds

  Scenario: Delete non-empty directory
    #rm -rf dir1 dir2
    When u1 creates directory and parents [dir1/child1, dir1/child2, dir2/dir3/child1]
    When last operation succeeds
    And u1 deletes non-empty directories [dir1, dir2]
    Then last operation succeeds
    And [dir1] are not in ls .
    And [dir2] are not in ls .
    And clean succeeds

  Scenario: Move directory
    When u1 creates directory and parents [dir1/dir2/dir3, dir4/dir5]
    When last operation succeeds
    And u1 renames dir4/dir5 to dir1/dir2/dir3
    Then [dir5] are not in ls dir4
    And [dir5] are not in ls spaces/s1/dir4
    And [dir1, dir4] are in ls .
    And [dir1, dir4] are in ls spaces/s1
    And [dir3] are in ls dir1/dir2
    And [dir3] are in ls spaces/s1/dir1/dir2
    And [dir5] are in ls dir1/dir2/dir3
    And [dir5] are in ls spaces/s1/dir1/dir2/dir3
    And clean succeeds

  Scenario: Copy directory
    When u1 creates directory and parents [dir1/dir2/dir3, dir4/dir5]
    When last operation succeeds
    And u1 copies directory dir4 to dir1/dir2/dir3
    Then [dir1, dir4] are in ls .
    And [dir2] are in ls dir1
    And [dir3] are in ls dir1/dir2
    And [dir4] are in ls dir1/dir2/dir3
    And [dir5] are in ls dir1/dir2/dir3/dir4
    And [dir5] are in ls dir4
    And clean succeeds

  Scenario: Move directory to itself
    When u1 creates directories [dir1]
    When last operation succeeds
    And u1 renames dir1 to dir1
    Then last operation fails
    And [dir1] are in ls .
    And [dir1] are in ls spaces/s1
    And clean succeeds

  Scenario: Move directory to its subtree
    When u1 creates directory and parents [dir1/dir2/dir3]
    When last operation succeeds
    And u1 renames dir1 to dir1/dir2/dir3
    Then last operation fails
    And [dir1] are in ls .
    And [dir1] are in ls spaces/s1
    And clean succeeds

  Scenario: Move directory to itself in spaces
    When u1 creates directories [dir1]
    When last operation succeeds
    And u1 renames dir1 to spaces/s1/dir1
    Then last operation fails
    And [dir1] are in ls .
    And [dir1] are in ls spaces/s1
    And clean succeeds

  Scenario: Move directory to itself in default space
    When u1 creates directories [spaces/s1/dir1]
    When last operation succeeds
    And u1 renames dir1 to dir1
    Then last operation fails
    And [dir1] are in ls .
    And [dir1] are in ls spaces/s1
    And clean succeeds

  Scenario: Move directory to its subtree in spaces
    When u1 creates directory and parents [dir1/dir2/dir3]
    When last operation succeeds
    And u1 renames dir1 to spaces/s1/dir1/dir2/dir3
    Then last operation fails
    And [dir1] are in ls .
    And [dir1] are in ls spaces/s1
    And clean succeeds

  Scenario: Move directory to its subtree in default space
    When u1 creates directory and parents [spaces/s1/dir1/dir2/dir3]
    When last operation succeeds
    And u1 renames dir1 to dir1/dir2/dir3
    Then last operation fails
    And [dir1] are in ls .
    And [dir1] are in ls spaces/s1
    And clean succeeds