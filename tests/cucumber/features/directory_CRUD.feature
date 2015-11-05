Feature: Directory_CRUD

  Background:
    Given environment is defined in env.json
    And environment is up
    And u1 starts oneclient in /home/u1/onedata using token

  Scenario: Create directory
    When u1 creates directories [dir1, dir2, dir3]
    Then u1 sees [dir1, dir2, dir3] in .
    And u1 sees [dir1, dir2, dir3] in spaces/s1

  Scenario: Create directory in default space
    When u1 creates directories [spaces/s1/dir1, spaces/s1/dir2, spaces/s1/dir3]
    Then u1 sees [dir1, dir2, dir3] in .
    And u1 sees [dir1, dir2, dir3] in spaces/s1

  Scenario: Create directory in non-default space
    When u1 creates directories [spaces/s2/dir1, spaces/s2/dir2, spaces/s2/dir3]
    Then u1 sees [dir1, dir2, dir3] in spaces/s2
    Then u1 doesn't see [dir1, dir2, dir3] in .

  Scenario: Create directory spaces
    When u1 creates directories [spaces]
    Then last operation by u1 fails

  Scenario: Create space
    When u1 creates directories [spaces/s1]
    Then last operation by u1 fails

  Scenario: Rename directory
    When u1 creates directories [dir1]
    And u1 renames dir1 to dir2
    Then u1 sees [dir2] in .
    And u1 doesn't see [dir1] in .

  Scenario: Delete empty directory
    When u1 creates directories [dir1]
    And last operation by u1 succeeds
    And u1 deletes empty directories [dir1]
    Then u1 doesn't see [dir1] in .

  Scenario: Delete directory spaces
    When u1 deletes non-empty directories [spaces]
    Then last operation by u1 fails

  Scenario: Delete space
    When u1 deletes non-empty directories [spaces/s1]
    Then last operation by u1 fails

  Scenario: Child directories
    When u1 creates directory and parents [dir1/child1, dir1/child2, dir1/child3]
    Then u1 sees [child1, child2, child3] in dir1

  Scenario: Child directories 2
    When u1 creates directory and parents [dir1/dir2/dir3/child1, dir1/dir2/child1, dir1/child1]
    Then u1 sees [dir2, child1] in dir1
    And u1 sees [dir3, child1] in dir1/dir2
    And u1 sees [child1] in dir1/dir2/dir3

  Scenario: Duplication
    When u1 creates directories [dir1]
    And u1 creates directories [dir1]
    Then last operation by u1 fails

  Scenario: Duplication in spaces
    When u1 creates directories [dir1]
    And u1 creates directories [spaces/s1/dir1]
    Then last operation by u1 fails

  Scenario: Delete empty directory and parents
    #rmdir -p dir1/dir2/dir3
    When u1 creates directory and parents [dir1/dir2/dir3]
    And u1 sees [dir1] in .
    And u1 sees [dir2] in dir1
    And u1 sees [dir3] in dir1/dir2
    And u1 deletes empty directory and parents [dir1/dir2/dir3]
    Then u1 doesn't see [dir1] in .

  Scenario: Delete non-empty directory in wrong way
    #wrong way means using rmdir instead of rm -rf
    When u1 creates directories [dir1, dir1/child1]
    And u1 sees [dir1] in .
    And u1 sees [child1] in dir1
    And u1 deletes empty directories [dir1]
    #dir1 is not empty, but we use step for empty dirs
    Then last operation by u1 fails
    And u1 sees [dir1] in .
    And u1 sees [child1] in dir1

  Scenario: Delete non-empty directory
    #rm -rf dir1 dir2
    When u1 creates directory and parents [dir1/child1, dir1/child2, dir2/dir3/child1]
    And u1 sees [dir1, dir2] in .
    And u1 sees [child1, child2] in dir1
    And u1 sees [dir3] in dir2
    And u1 sees [child1] in dir2/dir3
    And u1 deletes non-empty directories [dir1, dir2]
    Then u1 doesn't see [dir1] in .
    Then u1 doesn't see [dir2] in .

  Scenario: Move directory
    When u1 creates directory and parents [dir1/dir2/dir3, dir4/dir5]
    And u1 sees [dir1, dir4] in .
    And u1 sees [dir2] in dir1
    And u1 sees [dir3] in dir1/dir2
    And u1 sees [dir5] in dir4
    And u1 renames dir4/dir5 to dir1/dir2/dir3
    Then u1 doesn't see [dir5] in dir4
    And u1 doesn't see [dir5] in spaces/s1/dir4
    And u1 sees [dir1, dir4] in .
    And u1 sees [dir5] in dir1/dir2/dir3
    And u1 sees [dir5] in spaces/s1/dir1/dir2/dir3

  Scenario: Copy directory
    When u1 creates directory and parents [dir1/dir2/dir3, dir4/dir5]
    And u1 sees [dir1, dir4] in .
    And u1 sees [dir2] in dir1
    And u1 sees [dir3] in dir1/dir2
    And u1 sees [dir5] in dir4
    And u1 copies directory dir4 to dir1/dir2/dir3
    Then u1 sees [dir5] in dir4
    And u1 sees [dir5] in dir1/dir2/dir3/dir4
    And u1 sees [dir5] in spaces/s1/dir4
    And u1 sees [dir5] in spaces/s1/dir1/dir2/dir3/dir4

  Scenario: Move directory to itself
    When u1 creates directories [dir1]
    And u1 sees [dir1] in .
    And u1 renames dir1 to dir1
    Then last operation by u1 fails
    And u1 sees [dir1] in .
    And u1 sees [dir1] in spaces/s1

  Scenario: Move directory to its subtree
    When u1 creates directory and parents [dir1/dir2/dir3]
    And u1 sees [dir1] in .
    And u1 sees [dir2] in dir1
    And u1 sees [dir3] in dir1/dir2
    And u1 renames dir1 to dir1/dir2/dir3
    Then last operation by u1 fails
    And u1 sees [dir1] in .
    And u1 sees [dir1] in spaces/s1
    And u1 doesn't see [dir1] in dir1/dir2/dir3

  Scenario: Move directory to itself in spaces
    When u1 creates directories [dir1]
    And u1 sees [dir1] in .
    And u1 renames dir1 to spaces/s1/dir1
    Then last operation by u1 fails
    And [dir1] are in ls .
    And u1 sees [dir1] in .
    And u1 sees [dir1] in spaces/s1

  Scenario: Move directory to itself in default space
    When u1 creates directories [spaces/s1/dir1]
    And u1 sees [dir1] in spaces/s1
    And u1 renames spaces/s1/dir1 to dir1
    Then last operation by u1 fails
    And u1 sees [dir1] in .
    And u1 sees [dir1] in spaces/s1

  Scenario: Move directory to its subtree in spaces
    When u1 creates directory and parents [dir1/dir2/dir3]
    And u1 sees [dir1] in .
    And u1 sees [dir2] in dir1
    And u1 sees [dir3] in dir1/dir2
    And u1 renames dir1 to spaces/s1/dir1/dir2/dir3
    Then last operation by u1 fails
    And u1 sees [dir1] in .
    And u1 sees [dir1] in spaces/s1

  Scenario: Move directory to its subtree in default space
    When u1 creates directory and parents [spaces/s1/dir1/dir2/dir3]
    And u1 sees [dir1] in spaces/s1
    And u1 sees [dir2] in spaces/s1/dir1
    And u1 sees [dir3] in spaces/s1/dir1/dir2
    And u1 renames dir1 to spaces/s1/dir1/dir2/dir3
    Then last operation by u1 fails
    And u1 sees [dir1] in .
    And u1 sees [dir1] in spaces/s1