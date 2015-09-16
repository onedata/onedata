Feature: Directory_CRUD

  Background:
    Given environment is defined in env.json
    Given environment is up
    Given u1 mounts onedata spaces using token
    Given we are in space s1

  Scenario: Create directory
    When user creates directories [dir1]
    Then last operation succeeds
    Then [dir1] are in ls .

  Scenario: Rename directory
    When user renames dir1 to dir2
    Then last operation succeeds
    Then [dir2] are in ls .
    Then [dir1] are not in ls .

  Scenario: Delete directory
    When user deletes directory dir2
    Then last operation succeeds
    Then [dir2] are not in ls .

  Scenario: Child directories
    When user creates directories [dir1]
    When user creates directories [dir1/child1, dir2/child2, dir3/child3]
    Then last operation succeeds
    Then [child1, child2, child3] are in ls dir1

  Scenario: Double creation
    When user creates directories [double]
    When user creates directories [double]
    Then last operation fails