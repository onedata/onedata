Feature: Basic operations on directories

  Background:
    Given environment is up
    And user1 starts oneclient in /home/user1/onedata using token
    And user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
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
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    
    And user1 renames space1/dir1 to space1/dir2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "dir2" has appeared in file browser
    And user of browser sees that item named "dir2" is directory in file browser


  Scenario: User renames directory using browser and using oneclient he sees that its name has changed
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
    And user1 sees [dir1] in space1

    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user1 sees [dir2] in space1
    And user1 doesn't see [dir1] in space1


  Scenario: User removes empty directory using oneclient and sees in browser that it has disappeared
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    
    And user1 deletes empty directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User removes empty directory using browser and using oneclient he sees that it has disappeared
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

    And user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user1 doesn't see [dir1] in space1


  Scenario: User creates file tree using oneclient and using web gui he sees it looks the same
    When user1 creates directory and parents [space1/dir1/child1, space1/dir1/child2, space1/dir1/child3]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that items named ["child1", "child2", "child3"] have appeared in file browser
    
    # TODO: mby merge this three steps?
    And user of browser sees that item named "child1" is directory in file browser
    And user of browser sees that item named "child2" is directory in file browser
    And user of browser sees that item named "child3" is directory in file browser


  Scenario: User creates file tree using web gui and using oneclient he sees it looks the same
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    # create child1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    # create child2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child2" has appeared in file browser

    # create child3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child3" has appeared in file browser

    Then user1 sees [child1, child2, child3] in space1/dir1


  Scenario: User creates file tree using oneclient and using web gui he sees it looks the same (version 2)
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


  Scenario: User creates file tree using web gui and using oneclient he sees it looks the same (version 2)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    # create dir1/child1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    # create dir2
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir2" has appeared in file browser

    # create dir1/dir2/child1
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    # create dir3
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir3" has appeared in file browser

    # create dir1/dir2/dir3/child1
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    Then user1 sees [dir2, child1] in space1/dir1
    And user1 sees [dir3, child1] in space1/dir1/dir2
    And user1 sees [child1] in space1/dir1/dir2/dir3


  Scenario: User fails to create directory because of existing directory with given name using oneclient
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user1 fails to create directories [space1/dir1]
    Then user of browser sees that there is 1 item in file browser

    # TODO
  Scenario: User fails to create directory because of existing directory with given name using web gui
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
    And user1 sees [dir1] in space1

    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that there is 1 item in file browser
  
  Scenario: User deletes empty directory and its parents using oneclient and and using web gui he sees that they have disappeared
    When user1 creates directory and parents [space1/dir1/dir2/dir3]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser sees that item named "dir2" is directory in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "dir3" in file browser
    And user of browser sees that item named "dir3" is directory in file browser
    
    And user1 deletes empty directory and parents [space1/dir1/dir2/dir3]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User deletes empty directory and its parents using web gui and using oneclient he sees that they have disappeared
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    # create dir2 in space1/dir1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir2" has appeared in file browser

    # create dir3 in space1/dir1/dir2
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir1/dir2
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir3" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir3" has appeared in file browser

    And user of browser changes current working directory to space1 using breadcrumbs

    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser

    Then user1 doesn't see [dir1] in space1

  # TODO
  Scenario: User deletes non-empty directory in wrong way using oneclient and using web gui he sees that they have not disappeared
    When user1 creates directories [space1/dir1, space1/dir1/child1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "child1" in file browser
    And user of browser sees that item named "child1" is directory in file browser

    And user1 fails to delete empty directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser changes current working directory to space1 using breadcrumbs
    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "child1" in file browser
    And user of browser sees that item named "child1" is directory in file browser


  Scenario: User deletes non-empty directory using oneclient and using web gui he sees that they have disappeared
    When user1 creates directory and parents [space1/dir1/child1, space1/dir1/child2]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named ["child1", "child2"] in file browser
    And user of browser sees that item named "child1" is directory in file browser
    And user of browser sees that item named "child2" is directory in file browser

    And user1 deletes non-empty directories [space1/dir1, space1/dir2]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser

  Scenario: User deletes non-empty directory using web gui and using oneclient he sees that they have disappeared
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    # create dir1/child1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child1" has appeared in file browser

    # create dir1/child2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "child2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "child2" has appeared in file browser
    Then user1 doesn't see [dir1] in space1


  Scenario: User moves empty directory using oneclient and using web gui he sees that it has been moved
     When user1 creates directory and parents [space1/dir1/dir2/dir3, space1/dir4]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir4"] have appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "dir3" in file browser

    And user1 renames space1/dir4 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser changes current working directory to space1 using breadcrumbs
    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir4" has disappeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "dir3" in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees item(s) named "dir4" in file browser


  Scenario: User moves nonempty directory using oneclient and using web gui he sees that its content has not changed
    When user1 creates directory and parents [space1/dir1/dir2/dir3, space1/dir4/dir5]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir4"] have appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "dir3" in file browser
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser double clicks on item named "dir4" in file browser
    And user of browser sees item(s) named "dir5" in file browser

    And user1 renames space1/dir4/dir5 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser changes current working directory to space1 using breadcrumbs
    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir4" has disappeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "dir3" in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees item(s) named "dir4" in file browser
    And user of browser double clicks on item named "dir4" in file browser
    And user of browser sees item(s) named "dir5" in file browser


  Scenario: User copies nonempty directory using oneclient and using web gui he sees that it has not changed
    When user1 creates directory and parents [space1/dir1/dir2/dir3, space1/dir4/dir5]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir4"] have appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "dir3" in file browser
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser double clicks on item named "dir4" in file browser
    And user of browser sees item(s) named "dir5" in file browser

    And user1 copies directory space1/dir4 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser changes current working directory to space1 using breadcrumbs
    Then user of browser sees item(s) named ["dir1", "dir4"] in file browser
    And user of browser double clicks on item named "dir4" in file browser
    And user of browser sees item(s) named "dir5" in file browser
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "dir3" in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees item(s) named "dir4" in file browser
    And user of browser double clicks on item named "dir4" in file browser
    And user of browser sees item(s) named "dir5" in file browser


  Scenario: Move directory to its subtree
    When user1 creates directory and parents [space1/dir1/dir2/dir3]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser sees that item named "dir2" is directory in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "dir3" in file browser
    And user of browser sees that item named "dir3" is directory in file browser

    And user1 fails to rename space1/dir1 to space1/dir1/dir2/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser changes current working directory to space1 using breadcrumbs
    Then user of browser sees item(s) named "dir1" in file browser
    And user of browser changes current working directory to space1/dir1/dir2/dir3 using breadcrumbs
    And user of browser does not see any item named "dir1" in file browser


  Scenario: User creates directory using oneclient and renames it using web gui
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

    Then user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "dir2" has appeared in file browser
    And user of browser sees that item named "dir2" is directory in file browser
    And user1 sees [dir2] in space1
    And user1 doesn't see [dir1] in space1


  Scenario: User creates directory using web gui and renames it using oneclient
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # rename dir1 to dir2
    And user1 renames space1/dir1 to space1/dir2

    Then user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "dir2" has appeared in file browser
    And user of browser sees that item named "dir2" is directory in file browser
    And user1 sees [dir2] in space1
    And user1 doesn't see [dir1] in space1


  Scenario: User creates directory using oneclient and removes it using web gui
    When user1 creates directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # remove dir1
    And user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal

    Then user of browser sees that item named "dir1" has disappeared from files browser
    And user1 doesn't see [dir1] in space1


  Scenario: User creates directory using web gui and removes it using oneclient
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

    Then user of browser sees that item named "dir1" has disappeared from files browser
    And user1 doesn't see [dir1] in space1


  Scenario: User renames directory using web gui and using oneclient he sees that status-change time has changed
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

    And user1 waits 2 second

    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    Then status-change time of user1's space1/dir1 is equal to modification time
    And status-change time of user1's space1/dir1 is equal to access time


  Scenario: User changes directory using web gui and using oneclient he sees that modification time has changed
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

    And u1 waits 2 second

    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir2" has appeared in file browser
    And user of browser sees that item named "dir2" is directory in file browser

    Then modification time of user1's space1/dir1 is greater than access time
    And modification time of user1's space1/dir1 is equal to status-change time 