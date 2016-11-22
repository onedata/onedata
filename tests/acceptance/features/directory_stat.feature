Feature: Directory_stat

  Background:
    Given environment is up
    And u1 starts oneclient in /home/u1/onedata using token

  Scenario: Check file type
    When u1 creates directories [s1/dir1]
    Then file type of u1's s1/dir1 is directory

  Scenario: Check default access permissions
    When u1 creates directories [s1/dir1]
    Then mode of u1's s1/dir1 is 755

  Scenario: Change access permissions
    When u1 creates directories [s1/dir1]
    And u1 changes s1/dir1 mode to 211
    Then mode of u1's s1/dir1 is 211
    And u1 changes s1/dir1 mode to 755
    And mode of u1's s1/dir1 is 755

  Scenario: Timestamps at creation
    When u1 creates directories [s1/dir1]
    Then modification time of u1's s1/dir1 is equal to access time
    And status-change time of u1's s1/dir1 is equal to access time

  Scenario: Update timestamps
    When u1 creates directories [s1/dir1]
    And u1 waits 1 second
    And u1 creates directories [s1/dir1/dir2]
    And u1 updates [s1/dir1] timestamps
    # aim of above step is to call touch on s1/dir1
    # after creation of subdir access time and
    # modification time were different
    # after touch both will be updated to current time
    Then modification time of u1's s1/dir1 is equal to access time

  Scenario: Access time
    When u1 creates directories [s1/dir1]
    And u1 waits 1 second
    And u1 creates directories [s1/dir1/dir2]
    # two steps above ensure that access time is older than
    # modification time or status-change time and
    # will be modified on next access
    And u1 waits 1 second
    Then u1 sees [dir2] in s1/dir1
    #aim of above step is to call ls
    And access time of u1's s1/dir1 is greater than modification time
    And access time of u1's s1/dir1 is greater than status-change time

  Scenario: Modification time
    When u1 creates directories [s1/dir1]
    And u1 waits 1 second
    # call sleep, to be sure that time of above and below operations is different
    And u1 creates directories [s1/dir1/dir2]
    Then modification time of u1's s1/dir1 is greater than access time
    And modification time of u1's s1/dir1 is equal to status-change time

  Scenario: Status-change time when renaming
    When u1 creates directories [s1/dir1]
    And u1 waits 1 second
    # call sleep, to be sure that time of above and below operations is different
    And u1 renames s1/dir1 to s1/dir2
    Then status-change time of u1's s1/dir2 is greater than modification time
    And status-change time of u1's s1/dir2 is greater than access time

  Scenario: Status-change time when changing mode
    When u1 creates directories [s1/dir1]
    And u1 waits 1 second
    # call sleep, to be sure that time of above and below operations is different
    And u1 changes s1/dir1 mode to 711
    Then status-change time of u1's s1/dir1 is greater than modification time
    And status-change time of u1's s1/dir1 is greater than access time
