Feature: Oneprovider files
  Various operations on files using single client


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


  Scenario: User creates file using oneclient and sees in browser that it has appeared
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser


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
    And size of user1's space1/file1 is 0 bytes


  Scenario: User renames file using oneclient and sees in browser that its name has changed
    # create: space1/file1
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser

    # rename file1 to file2
    And user1 renames space1/file1 to space1/file2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has disappeared from files browser
    And user of browser sees that item named "file2" has appeared in file browser
    And user of browser sees that item named "file2" is file in file browser


  Scenario: User renames file using browser and using oneclient he sees that its name has changed
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
    And user1 sees [file1] in space1

    # rename file1 to file2
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user1 sees [file2] in space1
    And file type of user1's space1/file2 is regular
    And user1 doesn't see [file1] in space1


  Scenario: User removes empty file using oneclient and sees in browser that it has disappeared
    # create: space1/file1
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser

    # remove file1
    And user1 deletes files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User removes empty file using browser and using oneclient he sees that it has disappeared
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
    And user1 sees [file1] in space1

    # remove file1
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user1 doesn't see [file1] in space1


  Scenario: User writes to file using oneclient and sees in browser that file's content has changed
    # create: space1/file1
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser

    # write text to file1
    And user1 writes "TEST TEXT ONEDATA" to space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file1" is equal to: "TEST TEXT ONEDATA"


  Scenario: User appends text to file using oneclient and sees in browser that file's content has changed
    # create: space1/file1
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser

    # write text to file1
    And user1 writes "TEST TEXT ONEDATA" to space1/file1

    # append text to file1
    And user1 appends " APPENDED DATA" to space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" is of 31 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file1" is equal to: "TEST TEXT ONEDATA APPENDED DATA"


  Scenario: User replaces word in file using oneclient and sees in browser that file's content has changed
    # create: space1/file1
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser

    # write text to file1
    And user1 writes "TEST ONEDATA1 TEST ONEDATA2 TEST ONEDATA3" to space1/file1

    # replace word in file1
    And user1 replaces "TEST" with "SYSTEM" in space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" is of 47 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file1" is equal to: "SYSTEM ONEDATA1 SYSTEM ONEDATA2 SYSTEM ONEDATA3"


  Scenario: User moves file using oneclient and sees in browser that file has been moved
    # create: space1/dir1/dir2, space1/dir3
    When user1 creates directory and parents [space1/dir1/dir2, space1/dir3]
    And user1 creates regular files [space1/dir1/dir2/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir3"] have appeared in file browser

    # move space1/dir1/dir2/file1 to space1/dir3/file1
    And user1 renames space1/dir1/dir2/file1 to space1/dir3/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["dir1", "dir3"] in file browser
    And user of browser changes current working directory to /dir1/dir2 using directory tree
    And user of browser sees that item named "file1" has disappeared from files browser
    And user of browser changes current working directory to /dir3 using directory tree
    And user of browser sees item(s) named "file1" in file browser


  Scenario: User moves non-empty file using oneclient and sees in browser that its content has not changed
    # create: space1/dir1/dir2, space1/dir3
    When user1 creates directory and parents [space1/dir1/dir2, space1/dir3]
    And user1 creates regular files [space1/dir1/dir2/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir3"] have appeared in file browser

    And user1 writes "TEST TEXT ONEDATA" to space1/dir1/dir2/file1

    # move space1/dir1/dir2/file1 to space1/dir3/file1
    And user1 renames space1/dir1/dir2/file1 to space1/dir3/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["dir1", "dir3"] in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file1" is equal to: "TEST TEXT ONEDATA"


  Scenario: User copies file using oneclient and sees in browser that it has been copied
    # create: space1/dir1/dir2, space1/dir3 
    When user1 creates directory and parents [space1/dir1/dir2, space1/dir3]
    And user1 creates regular files [space1/dir1/dir2/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir3"] have appeared in file browser

    # copy space1/dir1/dir2/file1 to space1/dir3
    And user1 copies regular file space1/dir1/dir2/file1 to space1/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["dir1", "dir3"] in file browser
    And user of browser changes current working directory to /dir1/dir2 using directory tree
    And user of browser sees item(s) named "file1" in file browser
    And user of browser changes current working directory to /dir3 using directory tree
    And user of browser sees item(s) named "file1" in file browser


  Scenario: User copies non-empty file using oneclient and sees in browser that it has not changed
    # create: space1/dir1/dir2, space1/dir3
    When user1 creates directory and parents [space1/dir1/dir2, space1/dir3]
    And user1 creates regular files [space1/dir1/dir2/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir3"] have appeared in file browser

    And user1 writes "TEST TEXT ONEDATA" to space1/dir1/dir2/file1

    # copy: space1/dir1/dir2/file1 to space1/dir3
    And user1 copies regular file space1/dir1/dir2/file1 to space1/dir3
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["dir1", "dir3"] in file browser
    And user of browser changes current working directory to /dir1/dir2 using directory tree
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file1" is equal to: "TEST TEXT ONEDATA"
    And user of browser changes current working directory to /dir3 using directory tree
    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser double clicks on item named "file1" in file browser
    And user of browser sees that content of downloaded file "file1" is equal to: "TEST TEXT ONEDATA"


  Scenario: User removes file right after read using oneclient and sees in browser that it has been removed
    # create: space1/dir1/dir2
    When user1 creates directory and parents [space1/dir1/dir2]
    And user1 creates regular files [space1/dir1/dir2/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser changes current working directory to /dir1/dir2 using directory tree
    And user of browser sees that item named "file1" has appeared in file browser

    # write text to space1/dir1/dir2/file1, read it and delete file
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
    And size of user1's space1/20B-0.txt is 20 bytes


  Scenario: User creates file using browser and renames it using oneclient
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
    And user1 sees [file1] in space1

    # rename file1 to file2
    And user1 renames space1/file1 to space1/file2
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has disappeared from files browser
    And user of browser sees that item named "file2" has appeared in file browser
    And user of browser sees that item named "file2" is file in file browser
    And user1 doesn't see [file1] in space1
    And user1 sees [file2] in space1


  Scenario: User creates file using oneclient and renames it using browser
    # create: space1/file1
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
    And user1 sees [file1] in space1

    # remove file1
    And user1 deletes files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has disappeared from files browser
    And user1 doesn't see [file1] in space1


  Scenario: User creates file using oneclient and removes it using browser
    # create: space1/file1
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # remove file1
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal

    Then user1 doesn't see [file1] in space1
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User renames file using browser and using oneclient he sees that status-change time has changed
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
    And user1 sees [file1] in space1

    # call sleep, to be sure that time of above and below operations is different
    And user1 waits 2 second

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
    And status-change time of user1's space1/file2 is equal to modification time
    And status-change time of user1's space1/file2 is equal to access time


   Scenario: User changes file using oneclient and sees in browser that modification time has changed
    # create: space1/file1 
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser

    # call sleep, to be sure that time of above and below operations is different
    And user1 waits 80 second
    
    # write text to file1
    And user1 writes "TEST TEXT ONEDATA" to space1/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that modification date of item named "file1" is not earlier than 70 seconds ago in file browser


  Scenario: User creates file using oneclient, removes it using browser and then recreates it using oneclient
    # create: space1/file1
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # remove file1
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that item named "file1" has disappeared from files browser
    And user1 doesn't see [file1] in space1

    # create: space1/file1
    And user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has appeared in file browser
    And user1 sees [file1] in space1

  Scenario: User creates file using browser, removes it using oneclient and then recreates it using browser
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

    # remove file1
    And user1 deletes files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has disappeared from files browser
    And user1 doesn't see [file1] in space1

    # create: space1/file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that item named "file1" has appeared in file browser
    And user1 sees [file1] in space1