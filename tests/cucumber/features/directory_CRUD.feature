Feature: Directory_CRUD

  Background:
    Given environment is defined in env.json
    Given environment is up
    Given u1 mounts onedata spaces using token
    Given we are in space s1

  Scenario: Create directory
    When user creates directories [dir1, dir2, dir3]
    Then last operation succeeds
    Then [dir1, dir2, dir3] are in ls .

  Scenario: Rename directory
    When user renames dir1 to dir2
    Then last operation succeeds
    Then [dir2] are in ls .
    Then [dir1] are not in ls .

  Scenario: Delete empty directory
    When user deletes empty directory dir2
    Then last operation succeeds
    Then [dir2] are not in ls .

  Scenario: Child directories
    When user creates directories [dir1]
    When user creates directories [dir1/child1, dir2/child2, dir3/child3]
    Then last operation succeeds
    Then [child1, child2, child3] are in ls dir1

  Scenario: Child directories 2
    When user creates directories [dir1, dir1/dir2, dir1/dir2/child1, dir1/child1]
    Then last operation succeeds
    Then [dir2, child1] are in ls dir1
    Then [child1] are in ls dir1/dir2

  Scenario: Double creation
    When user creates directories [double]
    When user creates directories [double]
    Then last operation fails

  Scenario: Create directory and parents
    When user creates directory and parents dir1/dir2/dir3
    Then last operation succeeds
    Then [dir1] are in ls .
    Then [dir2] are in ls dir1
    Then [dir3] are in ls dir1/dir2

  Scenario: Delete empty directory and parents
    #rmdir -p dir1/dir2/dir3
    When user creates directory and parents dir1/dir2/dir3
    When user deletes empty directory and parents dir1/dir2/dir3
    Then last operation succeeds

  Scenario: Delete non-empty directory in wrong way
    #wrong way means using rmdir instead of rm -rf
    When user creates directories [dir1, dir1/child1]
    When user deletes empty directory dir1
    #dir1 is not empty, but we use step for empty dirs
    Then last operation fails

  Scenario: Delete non-empty directory
    When user creates directories [dir1, dir1/child1, dir1/child2]
    When user deletes non-empty directory dir1
    Then last operation succeeds
    Then [dir1] are not in ls .

  Scenario: Move directory
    When user creates directory and parents [dir1/dir2, dir3]
    When user renames dir3 to dir1/dir2
    Then last operation succeeds
    Then [dir3] are in ls dir1/dir2