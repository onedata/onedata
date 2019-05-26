Feature: Oneprovider files multiclient
  Various operations on files using multiclient


  Background:
    Given environment is up
    And [user1, user2] start oneclients [client1, client2] in
      [/home/user1/onedata, /home/user2/onedata] on client_hosts
      [client-host1, client-host2] respectively,
      using [token, token]
    And user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "devLogin" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User2 creates file using oneclient and user1 removes it using browser
    # create: space1/file1
    When user2 creates regular files [space1/file1] on client2

    # user1 removes file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*

    Then user of browser does not see any item(s) named "file1" in file browser
    And user2 doesn't see [file1] in space1 on client2


  Scenario: User1 creates file using browser and user2 removes it using oneclient
    # create: space1/file1
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser

    # user2 removes file1
    And user2 sees [file1] in space1 on client2
    And user2 deletes files [space1/file1] on client2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    Then user of browser does not see any item(s) named "file1" in file browser
    And user2 doesn't see [file1] in space1 on client2


  Scenario: User2 creates file using oneclient and user1 renames it using browser
    # create: space1/file1
    When user2 creates regular files [space1/file1] on client2

    # user1 renames file1 to file2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user of browser sees an info notify with text matching to: .*renamed.*

    And user of browser sees item(s) named "file2" in file browser
    And user of browser does not see any item(s) named "file1" in file browser
    And user2 sees [file2] in space1 on client2
    And user2 doesn't see [file1] in space1 on client2


  Scenario: User1 creates file using browser and user2 renames it using oneclient
    # create: space1/file1
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser

    # user2 renames file1 to file2
    And user2 sees [file1] in space1 on client2
    And user2 renames space1/file1 to space1/file2 on client2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    Then user of browser sees item(s) named "file2" in file browser
    And user of browser does not see any item(s) named "file1" in file browser
    And user2 sees [file2] in space1 on client2
    And user2 doesn't see [file1] in space1 on client2
