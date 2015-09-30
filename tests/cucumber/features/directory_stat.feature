Feature: Directory_stat

  Background:
    Given environment is defined in env.json
    And environment is up
    And u1 starts oneclient in /root/onedata using token

  Scenario: Check file type
    When u1 creates directories [dir1]
    Then u1 checks if dir1 file type is directory

  Scenario: Check default access permissions
    When u1 creates directories [dir1]
    Then u1 checks if dir1 mode is 755

  Scenario: Change access permissions
    When u1 creates directories [dir1]
    And u1 changes dir1 mode to 211
    Then u1 checks if dir1 mode is 211

  Scenario: Timestamps at creation
    When u1 creates directories [dir1]
    Then u1 checks if modification time of dir1 is equal to access time
    And u1 checks if status-change time of dir1 is equal to access time

  Scenario: Update timestamps
    When u1 creates directories [dir1]
    And u1 waits 1 seconds
    And u1 creates directories [dir1/dir2]
    And u1 updates [dir1] timestamps
    # aim of above step is to call touch on dir1
    # after creation of subdir access time and
    # modification time were different
    # after touch both will be updated to current time
    Then u1 checks if modification time of dir1 is equal to access time

  Scenario: Access time
    When u1 creates directories [dir1]
    And u1 waits 1 seconds
    Then u1 doesn't see [dir] in dir1
    #aim of above step is to call ls
    And u1 checks if access time of dir1 is greater than modification time
    And u1 checks if access time of dir1 is greater than status-change time

  Scenario: Modification time
    When u1 creates directories [dir1]
    And u1 waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    And u1 creates directories [dir1/dir2]
    Then u1 checks if modification time of dir1 is greater than access time
    And u1 checks if modification time of dir1 is equal to status-change time

  Scenario: Status-change time when renaming
    When u1 creates directories [dir1]
    And u1 waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    And u1 renames dir1 to dir2
    Then u1 checks if status-change time of dir2 is greater than modification time
    And u1 checks if status-change time of dir2 is greater than access time

  Scenario: Status-change time when changing mode
    When u1 creates directories [dir1]
    And u1 waits 1 seconds
    # call sleep, to be sure that time of above and below operations is different
    And u1 changes dir1 mode to 211
    Then u1 checks if status-change time of dir1 is greater than modification time
    And u1 checks if status-change time of dir1 is greater than access time
