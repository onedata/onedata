Feature: Multi_directory_stat

  Background:
    Given environment is defined in env.json
    And environment is up
    And [u1, u2] start oneclients [client1, client2] in
      [/home/u1/onedata, /home/u2/onedata] on client_hosts
      [client_host_1, client_host_2] respectively,
      using [token, token]

  Scenario: Check file type
    When u1 creates directories [dir1] on client1
    Then u2 waits 10 seconds on client2
    Then file type of u2's dir1 is directory on client2

  Scenario: Check default access permissions
    When u1 creates directories [dir1] on client1
    Then u2 waits 10 seconds on client2
    Then mode of u2's dir1 is 775 on client2

  Scenario: Change access permissions
    When u1 creates directories [dir1] on client1
    And u1 changes dir1 mode to 211 on client1
    Then u2 waits 10 seconds on client2
    Then mode of u2's dir1 is 211 on client2

  Scenario: Change someone's file access permissions
    When u1 creates directories [dir1] on client1
    And u2 waits 10 seconds on client2
    And u2 changes dir1 mode to 211 on client2
    Then last operation by u2 fails
 
  Scenario: Timestamps at creation
    When u1 creates directories [dir1] on client1
    Then u2 waits 10 seconds on client2
    Then modification time of u2's dir1 is equal to access time on client2
    And status-change time of u2's dir1 is equal to access time on client2

  Scenario: Update timestamps without write permission
    # touch dir1
    When u1 creates directories [dir1] on client1
    And u1 waits 1 seconds on client1
    And u1 changes dir1 mode to 755 on client1
    And u1 creates directories [dir1/dir2] on client1
    And u2 waits 10 seconds on client2
    And u2 updates [dir1] timestamps on client2
    # aim of above step is to call touch on dir1
    # after creation of subdir access time and
    # modification time were different
    # after touch both will be updated to current time
    Then last operation by u2 fails

  Scenario: Update timestamps with write permission
    # touch dir1
    When u1 creates directories [dir1] on client1
    And u1 waits 1 seconds on client1
    And u1 changes dir1 mode to 725 on client1
    And u1 creates directories [dir1/dir2] on client1
    And u2 waits 10 seconds on client2
    And u2 updates [dir1] timestamps on client2
    # aim of above step is to call touch on dir1
    # after creation of subdir access time and
    # modification time were different
    # after touch both will be updated to current time
    Then modification time of u2's dir1 is equal to access time on client2

  Scenario: Access time
    When u1 creates directories [dir1] on client1
    And u1 waits 1 seconds
    And u1 creates directories [dir1/dir2] on client1
    # two steps above ensure that access time is older than
    # modification time or status-change time and
    # will be modified on next access
    And u1 waits 1 seconds on client1
    Then u1 sees [dir2] in dir1 on client1
    #aim of above step is to call ls
    And u2 waits 10 seconds on client2
    And access time of u2's dir1 becomes greater than modification time on client2 within 5 seconds
    And access time of u2's dir1 becomes greater than status-change time on client2 within 5 seconds

  Scenario: Modification time
    When u1 creates directories [dir1] on client1
    And u1 waits 1 seconds on client1
    # call sleep, to be sure that time of above and below operations is different
    And u1 creates directories [dir1/dir2] on client1
    And u2 waits 10 seconds on client2
    Then modification time of u2's dir1 becomes greater than access time on client2 within 5 seconds
    And modification time of u2's dir1 becomes equal to status-change time on client2 within 5 seconds

  Scenario: Status-change time when renaming
    When u1 creates directories [dir1] on client1
    When u1 waits 1 seconds on client1
    # call sleep, to be sure that time of above and below operations is different
    When u1 renames dir1 to dir2 on client1
    And u2 waits 10 seconds on client2
    Then status-change time of u2's dir2 becomes greater than modification time on client2 within 5 seconds
    Then status-change time of u2's dir2 becomes greater than access time on client2 within 5 seconds

  Scenario: Status-change time when changing mode
    When u1 creates directories [dir1] on client1
    When u1 waits 1 seconds on client1
    # call sleep, to be sure that time of above and below operations is different
    When u1 changes dir1 mode to 211 on client1
    And u2 waits 10 seconds on client2
    Then status-change time of u2's dir1 becomes greater than modification time on client2 within 5 seconds
    Then status-change time of u2's dir1 becomes greater than access time on client2 within 5 seconds