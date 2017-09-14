Feature: POSIX privileges acceptance mixed tests multi user

  Background:
    Given environment is up
    And [user1, user2] start oneclients [client1, client2] in
      [/home/user1/onedata, /home/user2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    And user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user2" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started

  Scenario: User1 creates file using oneclient and user2 fails to change its permission using web gui

    # Create file
    When user1 creates regular files [space1/file1] on client1

    # Fail to change permission code
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    And user of browser clicks on input box in active modal
    And user of browser sets "775" permission code in active modal
    And user of browser clicks "Ok" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*failed.*
    And user1 deletes files [space1/file1] on client1


  Scenario: User1 creates directory using oneclient and user2 fails to change its permission using web gui

    # Create directory
    When user1 creates directories [space1/dir1] on client1

    # Fail to change permission code
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects "dir1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Change element permissions"
    And user of browser sees that "Edit permissions" modal has appeared
    And user of browser selects "POSIX" permission type in active modal
    And user of browser clicks on input box in active modal
    And user of browser sets "664" permission code in active modal
    And user of browser clicks "Ok" confirmation button in displayed modal
    Then user of browser sees an error notify with text matching to: .*failed.*
    And user1 deletes empty directories [space1/dir1] on client1


  Scenario: User2 creates file using web gui and user1 fails to change its permission using oneclient

    # Create file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    # Fail to change permission code
    Then user1 fails to change space1/file1 mode to 775 on client1
    And user2 deletes files [space1/file1] on client2 on client2


  Scenario: User2 creates directory using web gui and user1 fails to change its permission using oneclient

    # Create directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    # Fail to change permission code
    Then user1 fails to change space1/dir1 mode to 664 on client1
    And user2 deletes empty directories [space1/dir1] on client2
