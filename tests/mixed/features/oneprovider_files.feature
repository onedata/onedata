Feature: Basic operations on files

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

  Scenario: User creates file using oneclient and sees in browser that the file has appeared
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser

  Scenario: User renames file using oneclient and sees in browser that the file name has changed
    When user1 creates regular files [space1/file1]
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

  Scenario: User deletes empty file using oneclient and sees in browser that file has disappeared
    When user1 creates regular files [space1/file1]
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

  Scenario: User writes to file using oneclient and sees in browser that file's size has changed
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser
    And user1 writes "TEST TEXT ONEDATA" to space1/file1
    Then user of browser sees that item named "file1" is of 17 B size in file browser

  Scenario: User appends text to file using oneclient and sees in browser that file's size has changed
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser
    And user1 writes "TEST TEXT ONEDATA" to space1/file1
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user1 appends " APPENDED DATA" to space1/file1
    Then user of browser sees that item named "file1" is of 31 B size in file browser

  Scenario: User replaces word if file using oneclient and sees in browser that file's size has changed
    When user1 creates regular files [space1/file1]
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser sees that item named "file1" is of 0 B size in file browser
    And user1 writes "TEST ONEDATA1 TEST ONEDATA2 TEST ONEDATA3" to space1/file1
    And user of browser sees that item named "file1" is of 41 B size in file browser
    And user1 replaces "TEST" with "SYSTEM" in space1/file1
    Then user of browser sees that item named "file1" is of 47 B size in file browser

  Scenario: User moves file using oneclient and sees in browser that file has been moved
    When user1 creates directory and parents [space1/dir1/dir2, space1/dir3]
    And user1 creates regular files [space1/dir1/dir2/file1]
    And user1 writes "TEST TEXT ONEDATA" to space1/dir1/dir2/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that items named ["dir1", "dir3"] has appeared in file browser
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees item named "dir2" in file browser
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees item named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user1 renames space1/dir1/dir2/file1 to space1/dir3/file1
    And user of browser refreshes site
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user sees that items named ["dir1", "dir3"] in file browser
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees item named "file1" in file browser
    And user of browser sees that item named "file1" is of 17 B size in file browser
    And user of browser changes current working directory to space1/dir1/dir2 using breadcrumbs
    And user of browser sees that item named "file1" has disappeared in file browser