Feature: Oneprovider directories multiclient
  Various operations on directories using multiclient


  Background:
    Given environment is up
    And [user1, user2] start oneclients [client1, client2] in
      [/home/user1/onedata, /home/user2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    And user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User2 creates directory using oneclient and user1 fails to remove it using browser
    # create: space1/dir1
    When user2 creates directories [space1/dir1] on client2

    # user1 tries to remove dir1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal

    Then user of browser sees item(s) named "dir1" in file browser
    And user2 sees [dir1] in space1 on client2


  Scenario: User1 creates directory using browser and user2 fails to remove it using oneclient
    # create: space1/dir1
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser

    # user2 tries to remove dir1
    And user2 sees [dir1] in space1 on client2
    And user2 fails to delete empty directories [space1/dir1] on client2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    Then user2 sees [dir1] in space1 on client2
    And user of browser sees item(s) named "dir1" in file browser


  Scenario: User2 creates directory using oneclient and user1 fails to rename it using browser
    # create: space1/dir1
    When user2 creates directories [space1/dir1] on client2

    # user1 tries to rename dir1 to dir2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser does not see any item(s) named "dir2" in file browser
    And user2 sees [dir1] in space1 on client2
    And user2 doesn't see [dir2] in space1 on client2

  Scenario: User1 creates directory using browser and user2 fails to rename it using oneclient
    # create: space1/dir1
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser

    # user2 tries to rename dir1 to dir2
    And user2 sees [dir1] in space1 on client2
    And user2 fails to rename space1/dir1 to space1/dir2 on client2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    Then user2 sees [dir1] in space1 on client2
    And user2 doesn't see [dir2] in space1 on client2
    And user of browser sees item(s) named "dir1" in file browser
    And user of browser does not see any item(s) named "dir2" in file browser

