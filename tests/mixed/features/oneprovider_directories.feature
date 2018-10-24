Feature: Oneprovider directories
  Various operations on directories using single client

  Background:
    Given environment is up
    And user1 starts oneclient in /home/user1/onedata using token
    And user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "devLogin" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started

    
  Scenario: User creates directory using oneclient and sees in browser that it has appeared
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser


  Scenario: User creates directory using browser and using oneclient he sees that it has appeared
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user1 sees [dir1] in space1
    And file type of user1's space1/dir1 is directory

    
  Scenario: User renames directory using oneclient and sees in browser that its name has changed
    # create: dir1 in space1
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser

    # rename dir1 to dir2
    And user1 renames space1/dir1 to space1/dir2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "dir2" has appeared in file browser
    And user of browser sees that item named "dir2" is directory in file browser


  Scenario: User renames directory using browser and using oneclient he sees that its name has changed
    # create: dir1 in space1
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user1 sees [dir1] in space1

    # rename dir1 to dir2
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user1 sees [dir2] in space1
    And file type of user1's space1/dir2 is directory
    And user1 doesn't see [dir1] in space1


  Scenario: User removes empty directory using oneclient and sees in browser that it has disappeared
    # create: dir1 in space1
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser

    # remove dir1 from space1
    And user1 deletes empty directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User removes empty directory using browser and using oneclient he sees that it has disappeared
    # create: dir1 in space1
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user1 sees [dir1] in space1

    # remove dir1 from space1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser

    Then user1 doesn't see [dir1] in space1


  Scenario: User creates file tree using oneclient and sees in browser that it looks the same
    # create: space1/dir1/child1, space1/dir1/child2, space1/dir1/child3
    When user1 creates directory and parents [space1/dir1/child1, space1/dir1/child2, space1/dir1/child3]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that items named ["child1", "child2", "child3"] have appeared in file browser


  Scenario: User creates file tree using browser and using oneclient he sees it looks the same
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create: dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # create: dir1/child1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    # create: dir1/child2
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child2" has appeared in file browser

    # create: dir1/child3
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child3" has appeared in file browser

    Then user1 sees dir1 in space1
    And file type of user1's space1/dir1 is directory
    And user1 sees [child1, child2, child3] in space1/dir1


  Scenario: User creates file tree using oneclient and sees in browser that it looks the same (version 2)
    # create: space1/dir1/dir2/dir3/child1, space1/dir1/dir2/child1, space1/dir1/child1
    When user1 creates directory and parents [space1/dir1/dir2/dir3/child1, space1/dir1/dir2/child1, space1/dir1/child1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that items named ["dir2", "child1"] have appeared in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees that items named ["dir3", "child1"] have appeared in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees that item named "child1" has appeared in file browser


  Scenario: User creates file tree using browser and using oneclient he sees it looks the same (version 2)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create: dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # create: dir1/child1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    # create: dir1/dir2
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # create: dir1/dir2/child1
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    # create: dir1/dir2/dir3
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # create: dir1/dir2/dir3/child1
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    Then user1 sees dir1 in space1
    And user1 sees [dir2, child1] in space1/dir1
    And user1 sees [dir3, child1] in space1/dir1/dir2
    And user1 sees [child1] in space1/dir1/dir2/dir3


  Scenario: User fails to create directory using oneclient because of existing directory with given name and sees in browser that only one directory exists
    # create: dir1 in space1
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser

    # fail to create dir1 in space1 cause directory with that name already exists
    And user1 fails to create directories [space1/dir1]
    Then user of browser sees that there is 1 item in file browser


  Scenario: User removes empty directory and its parents using oneclient and sees in browser that they have disappeared
    # create: space1/dir1/dir2/dir3
    When user1 creates directory and parents [space1/dir1/dir2/dir3]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that item named "dir2" has appeared in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees that item named "dir3" has appeared in file browser

    # delete file tree: space1/dir1/dir2/dir3
    And user1 deletes empty directory and parents [space1/dir1/dir2/dir3]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User removes empty directory and its parents using browser and using oneclient he sees that they have disappeared
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create: dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # create: dir2 in space1/dir1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    # create: dir3 in space1/dir1/dir2
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir3" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir3" has appeared in file browser

    And user of browser changes current working directory to / using directory tree
    And user1 sees [dir1] in space1
    And user1 sees [dir2] in space1/dir1
    And user1 sees [dir3] in space1/dir1/dir2

    # remove dir1 from space1
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser

    Then user1 doesn't see [dir1] in space1


  Scenario: User removes non-empty directory in wrong way using oneclient and sees in browser that it has not disappeared
    # create: space1/dir1, space1/dir1/child1
    When user1 creates directories [space1/dir1, space1/dir1/child1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser

    # dir1 is not empty, but we use step for empty dirs
    And user1 fails to delete empty directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "child1" in file browser


  Scenario: User removes non-empty directory using oneclient and sees in browser that they have disappeared
    # create: space1/dir1/child1, space1/dir1/child2
    When user1 creates directory and parents [space1/dir1/child1, space1/dir1/child2]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that items named ["child1", "child2"] have appeared in file browser

    # remove space1/dir1
    And user1 deletes non-empty directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User removes non-empty directory using browser and using oneclient he sees that they have disappeared
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create: dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # create: dir1/child1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    # create: dir1/child2
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child2" has appeared in file browser

    And user of browser changes current working directory to / using directory tree
    And user1 sees [dir1] in space1
    And user1 sees [child1, child2] in space1/dir1

    # remove space1/dir1
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser

    Then user1 doesn't see [dir1] in space1


  Scenario: User moves directory using oneclient and sees in browser that it has been moved
    # create: space1/dir1/dir2/dir3, space1/dir4
    When user1 creates directory and parents [space1/dir1/dir2/dir3, space1/dir4]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir4"] have appeared in file browser

    # move space1/dir4 to space1/dir1/dir2/dir3
    And user1 renames space1/dir4 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir4" has disappeared from files browser
    And user of browser changes current working directory to /dir1/dir2/dir3 using directory tree
    And user of browser sees item(s) named "dir4" in file browser


  Scenario: User moves non-empty directory using oneclient and sees in browser that its content has not changed
    # create: space1/dir1/dir2/dir3, space1/dir4/dir5
    When user1 creates directory and parents [space1/dir1/dir2/dir3, space1/dir4/dir5]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir4"] have appeared in file browser

    # move space1/dir4 to space1/dir1/dir2/dir3
    And user1 renames space1/dir4 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser changes current working directory to /dir1/dir2/dir3 using directory tree
    And user of browser sees item(s) named "dir4" in file browser
    And user of browser double clicks on item named "dir4" in file browser
    And user of browser sees item(s) named "dir5" in file browser


  Scenario: User copies directory using oneclient and sees in browser that it has been copied
    # create: space1/dir1/dir2/dir3, space1/dir4
    When user1 creates directory and parents [space1/dir1/dir2/dir3, space1/dir4]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir4"] have appeared in file browser

    # copy space1/dir4 to space1/dir1/dir2/dir3
    And user1 copies directory space1/dir4 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["dir1", "dir4"] in file browser
    And user of browser changes current working directory to /dir1/dir2/dir3 using directory tree
    And user of browser sees item(s) named "dir4" in file browser


  Scenario: User copies non-empty directory using oneclient and sees in browser that it has not changed
    # create: space1/dir1/dir2/dir3, space1/dir4/dir5
    When user1 creates directory and parents [space1/dir1/dir2/dir3, space1/dir4/dir5]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir4"] have appeared in file browser

    # copy space1/dir4 to space1/dir1/dir2/dir3
    And user1 copies directory space1/dir4 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["dir1", "dir4"] in file browser
    And user of browser changes current working directory to /dir1/dir2/dir3 using directory tree
    And user of browser sees item(s) named ["dir4"] in file browser
    And user of browser double clicks on item named "dir4" in file browser
    And user of browser sees item(s) named "dir5" in file browser
    And user of browser changes current working directory to /dir4 using directory tree
    And user of browser sees item(s) named "dir5" in file browser


  Scenario: User fails to move directory to its subtree using oneclient and sees in browser that it has not been moved
    # create: space1/dir1/dir2/dir3
    When user1 creates directory and parents [space1/dir1/dir2/dir3]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser

    # fail to move space1/dir1 to space1/dir1/dir2/dir3
    And user1 fails to rename space1/dir1 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser changes current working directory to /dir1/dir2/dir3 using directory tree
    And user of browser does not see any item(s) named "dir1" in file browser


  Scenario: User creates directory using oneclient and renames it using browser
    # create: space1/dir1
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # rename dir1 to dir2
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    Then user1 sees [dir2] in space1
    And user1 doesn't see [dir1] in space1
    And user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "dir2" has appeared in file browser
    And user of browser sees that item named "dir2" is directory in file browser


  Scenario: User creates directory using browser and renames it using oneclient
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
    And user1 sees [dir1] in space1

    # rename dir1 to dir2
    And user1 renames space1/dir1 to space1/dir2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    Then user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "dir2" has appeared in file browser
    And user of browser sees that item named "dir2" is directory in file browser
    And user1 sees [dir2] in space1
    And user1 doesn't see [dir1] in space1


  Scenario: User creates directory using oneclient and removes it using browser
    # create: space1/dir1
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # remove dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal

    Then user1 doesn't see [dir1] in space1
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User creates directory using browser and removes it using oneclient
    #create space1/dir1
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user1 sees [dir1] in space1

    # remove dir1
    And user1 deletes empty directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    Then user of browser sees that item named "dir1" has disappeared from files browser
    And user1 doesn't see [dir1] in space1


  Scenario: User renames directory using browser and using oneclient he sees that status-change time has changed
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

    # call sleep, to be sure that time of above and below operations is different
    And user1 waits 2 second

    # rename dir1 to dir2
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    Then user1 sees [dir2] in space1
    And user1 doesn't see [dir1] in space1
    And status-change time of user1's space1/dir2 is equal to modification time
    And status-change time of user1's space1/dir2 is equal to access time


  Scenario: User changes directory using browser and using oneclient he sees that modification time has changed
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
    And user of browser double clicks on item named "dir1" in file browser

    # call sleep, to be sure that time of above and below operations is different
    And user1 waits 2 second

    # create: space1/dir1/dir2
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir2" has appeared in file browser

    Then user1 sees [dir1] in space1
    And user1 sees [dir2] in space1/dir1
    And modification time of user1's space1/dir1 is less than access time
    And modification time of user1's space1/dir1 is equal to status-change time


  Scenario: User changes directory using oneclient and sees in browser that modification time has changed
    # create: space1/dir1
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser

    # call sleep, to be sure that time of above and below operations is different
    And user1 waits 80 second

    # create: space1/dir1/dir2
    And user1 creates directories [space1/dir1/dir2]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that modification date of item named "dir1" is not earlier than 70 seconds ago in file browser


  Scenario: User creates directory using oneclient, removes it using browser and then recreates it using oneclient
    # create: space1/dir1
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # remove dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that item named "dir1" has disappeared from files browser
    And user1 doesn't see [dir1] in space1

    # create: space1/dir1
    And user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has appeared in file browser
    And user1 sees [dir1] in space1


  Scenario: User creates directory using browser, removes it using oneclient and then recreates it using browser
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

    # remove dir1
    And user1 deletes empty directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has disappeared from files browser
    And user1 doesn't see [dir1] in space1

    # create: space1/dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that item named "dir1" has appeared in file browser
    And user1 sees [dir1] in space1