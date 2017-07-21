Feature: Basic operations on files

  # TODO: moves and copies as in case of directories

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


  Scenario: User creates file using oneclient and sees in browser that it has appeared
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser


  Scenario: User creates file using browser and using oneclient he sees that it has appeared
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user1 sees [file1] in space1
    And file type of user1's space1/file1 is regular


  Scenario: User renames file using oneclient and sees in browser that its name has changed
    When user1 creates regular files [space1/file1]

    # TODO: delete if ..
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser

    And user1 renames space1/file1 to space1/file2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has disappeared from files browser
    And user of browser sees that item named "file2" has appeared in file browser
    And user of browser sees that item named "file2" is file in file browser


  Scenario: User renames file using browser and using oneclient he sees that its name has changed
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

    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user1 sees [file2] in space1
    And user1 doesn't see [file1] in space1


  Scenario: User removes empty file using oneclient and sees in browser that it has disappeared
    When user1 creates regular files [space1/file1]

    # TODO: delete if ...
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser

    And user1 deletes files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User removes empty file using browser and using oneclient he sees that it has disappeared
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
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal

    Then user1 doesn't see [file1] in space1


  Scenario: User writes to file using oneclient and sees in browser that file's content has changed
    When user1 creates regular files [space1/file1]

    # TODO: delete if ...
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser

    And user1 writes "TEST TEXT ONEDATA" to space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file1" is equal to: "TEST TEXT ONEDATA"


  Scenario: User appends text to file using oneclient and sees in browser that file's content has changed
    When user1 creates regular files [space1/file1]

    # TODO: delete if ...
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser

    And user1 writes "TEST TEXT ONEDATA" to space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" is of 17 B size in file browser

    And user1 appends " APPENDED DATA" to space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" is of 31 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file" is equal to: "TEST TEXT ONEDATA APPENDED DATA"


  Scenario: User replaces word in file using oneclient and sees in browser that file's content has changed
    When user1 creates regular files [space1/file1]

    # TODO: delete if ...
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser

    And user1 writes "TEST ONEDATA1 TEST ONEDATA2 TEST ONEDATA3" to space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" is of 41 B size in file browser

    And user1 replaces "TEST" with "SYSTEM" in space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" is of 47 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file1" is equal to: "SYSTEM ONEDATA1 SYSTEM ONEDATA2 SYSTEM ONEDATA3"


  Scenario: User moves empty file using oneclient and sees in browser that file has been moved
    When user1 creates directory and parents [space1/dir1/dir2, space1/dir3]
    And user1 creates regular files [space1/dir1/dir2/file1]

    # TODO: delete if ...
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir3"] have appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser

    And user1 renames space1/dir1/dir2/file1 to space1/dir3/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that items named ["dir1", "dir3"] in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file" is equal to: ""
    And user of browser changes current working directory to space1/dir1/dir2 using breadcrumbs
    And user of browser sees that item named "file1" has disappeared in file browser


  Scenario: User moves non-empty file using oneclient and sees in browser that its content has not changed
    When user1 creates directory and parents [space1/dir1/dir2, space1/dir3]
    And user1 creates regular files [space1/dir1/dir2/file1]
    And user1 writes "TEST TEXT ONEDATA" to space1/dir1/dir2/file1

    # TODO: delete if ...
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir3"] have appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser

    And user1 renames space1/dir1/dir2/file1 to space1/dir3/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["dir1", "dir3"] in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file" is equal to: "TEST TEXT ONEDATA"
    And user of browser changes current working directory to space1/dir1/dir2 using breadcrumbs
    And user of browser sees that item named "file1" has disappeared in file browser


  Scenario: User copies file using oneclient and sees in browser that it has been copied
    When user1 creates directory and parents [space1/dir1/dir2, space1/dir3]
    And user1 creates regular files [space1/dir1/dir2/file1]
    And user1 writes "TEST TEXT ONEDATA" to space1/dir1/dir2/file1

    # TODO: delete if ...
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir3"] have appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser

    And user1 copies regular file space1/dir1/dir2/file1 to space1/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["dir1", "dir3"] in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file" is equal to: "TEST TEXT ONEDATA"
    And user of browser changes current working directory to space1/dir1/dir2 using breadcrumbs
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file" is equal to: "TEST TEXT ONEDATA"


  Scenario: User removes file right after read using oneclient and sees in browser that it has been removed
    When user1 creates directory and parents [space1/dir1/dir2]
    And user1 creates regular files [space1/dir1/dir2/file1]

    # TODO: delete this if ...
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "dir1" has appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item(s) named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item(s) named "file1" in file browser

    And user1 writes "TEST TEXT ONEDATA" to space1/dir1/dir2/file1
    And user1 reads "TEST TEXT ONEDATA" from file space1/dir1/dir2/file1
    And user1 deletes non-empty directories [space1/dir1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User uploads file using browser and using oneclient he sees that it has appeared and has the same content
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    Then user1 sees [20B-0.txt] in space1
    And user1 reads "00000000000000000000" from file space1/20B-0.txt
    And size of user1's space1/file1 is 20 bytes


  Scenario: User creates file using browser and renames it using oneclient
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

    # rename file1 to file2
    And user1 renames space1/file1 to space1/file2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user1 sees [file2] in space1
    And user1 doesn't see [file1] in space1
    And user of browser sees that item named "file1" has disappeared from files browser
    And user of browser sees that item named "file2" has appeared in file browser
    And user of browser sees that item named "file2" is file in file browser


  Scenario: User creates file using oneclient and renames it using browser
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # rename file1 to file2
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    Then user1 sees [file2] in space1
    And user1 doesn't see [file1] in space1
    And user of browser sees that item named "file1" has disappeared from files browser
    And user of browser sees that item named "file2" has appeared in file browser
    And user of browser sees that item named "file2" is file in file browser


  Scenario: User creates file using browser and removes it using oneclient
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

    And user1 deletes files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user1 doesn't see [file1] in space1
    And user of browser sees that item named "file1" has disappeared from files browser

  Scenario: User creates file using oneclient and removes it using browser
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal

    Then user1 doesn't see [file1] in space1
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User renames file using browser and using oneclient he sees that status-change time has changed
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

    And user1 waits 2 second

    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared

    Then status-change time of user1's space1/file2 is equal to modification time
    And status-change time of user1's space1/file2 is equal to access time


   Scenario: User changes file using oneclient and using web gui he sees that modification time has changed
    When user1 creates regular files [space1/file1]
    And user1 waits 60 second
    And user1 writes "TEST TEXT ONEDATA" to space1/file1
    Then user of browser sees that modification date of item named "file1" is not earlier than 60 seconds ago in file browser