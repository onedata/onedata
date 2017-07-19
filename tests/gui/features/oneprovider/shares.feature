Feature: Basic data tab operations on shares in file browser


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And initial spaces configuration in "z1" Onezone service:
        space1:
            owner: user1
            providers:
                - p1:
                    storage: onestorage
                    size: 1000000
            storage:
                defaults:
                    provider: p1
                directory tree:
                    - dir1
                    - dir2:
                        - file1: 11111
                        - file2: 22222
                        - dir3:
                            - file3: 33333
                            - dir4

    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser logged as user1 to Onezone service
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser clicked on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser seen that Oneprovider session has started


  Scenario: User sees share icon on directory after sharing it (presses ENTER after typing share name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    Then user of browser sees that item named "dir1" is shared in file browser


  Scenario: User sees share icon on directory after sharing it (clicks CREATE confirmation button after typing share name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    Then user of browser sees that item named "dir1" is shared in file browser


  Scenario: User fails to share already shared directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # try to share dir1 again
    And user of browser sees that item named "dir1" is shared in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"

    Then user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared


  Scenario: User shares a directory and views information about it (clicks "Open the share" in "Share summary" modal)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*

    Then user of browser sees that share named "share1" has appeared in the shared list
    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that selected share is named "share1"
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir1


  Scenario: User shares a directory and views information about it (changes tab to shared after closing "Share summary" modal)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser clicks on the "shared" tab in main menu sidebar
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*

    Then user of browser sees that share named "share1" has appeared in the shared list
    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that selected share is named "share1"
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir1


  Scenario: User shares a directory and views information about it (clicks "Open the share" in "Share summary" modal after clicking on share icon)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser clicks on share tool icon in file row for "dir1" in file browser
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*

    Then user of browser sees that share named "share1" has appeared in the shared list
    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that selected share is named "share1"
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir1


  Scenario: User does not see any share when he opens shares index view after removing only shared directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal

    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser sees that share named "share1" has appeared in the shared list

    # remove dir1
    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared

    And user of browser clicks on the "shared" tab in main menu sidebar
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares
    Then user of browser does not see any share


  Scenario: User sees new files after adding them to shared directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal

    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser sees that share named "share1" has appeared in the shared list

    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1 in dir1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser clicks on the "shared" tab in main menu sidebar
    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser sees file browser in shared tab in Oneprovider page

    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir1
    And user of browser sees item(s) named "file1" in file browser


  Scenario: User renames share
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal

    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that selected share is named "share1"
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir1

    # rename share
    And user of browser clicks on settings icon displayed for "share1" item on the shares sidebar list
    And user of browser clicks on the "RENAME" item in settings dropdown for share named "share1"
    And user of browser sees that "Rename share" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that "share1" has been renamed to "helloworld"

    And user of browser selects "helloworld" from shares sidebar list
    And user of browser sees that selected share is named "helloworld"
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir1


  Scenario: User removes share
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal

    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that selected share is named "share1"
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir1

    # remove share
    And user of browser clicks on settings icon displayed for "share1" item on the shares sidebar list
    And user of browser clicks on the "REMOVE" item in settings dropdown for share named "share1"
    And user of browser sees that "Remove share" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    Then user of browser sees that share named "share1" has disappeared from the shares list


  Scenario: User does not see files in file browser in share view after removing them from shared directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir2
    And user of browser clicks once on item named "dir2" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared
    And user of browser sees that "Share summary" modal has appeared

    # go to shared tab and check share config
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser sees that share named "share1" has appeared in the shared list
    And user of browser selects "share1" from shares sidebar list

    And user of browser sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser sees file browser in shared tab in Oneprovider page
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir2
    And user of browser sees item(s) named ["dir3", "file1", "file2"] in file browser in given order

    # return to data tab
    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # remove dir2/file1
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser

    And user of browser clicks on the "shared" tab in main menu sidebar
    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser sees file browser in shared tab in Oneprovider page
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir2
    Then user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User can change working directory using breadcrumbs
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir2
    And user of browser clicks once on item named "dir2" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared

    # go to shared tab
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser sees that share named "share1" has appeared in the shared list

    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that selected share is named "share1"
    And user of browser sees file browser in shared tab in Oneprovider page
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir2
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser double clicks on item named "dir2" in file browser
    Then user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir2
    And user of browser double clicks on item named "dir3" in file browser
    Then user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir2/dir3
    And user of browser double clicks on item named "dir4" in file browser
    Then user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir2/dir3/dir4
    And user of browser changes current working directory to share1/dir2 using breadcrumbs from share's file browser
    Then user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir2


  Scenario: User can jump to data tab by clicking on dir in breadcrumbs from shared tab
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir2/dir3
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser clicks once on item named "dir3" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared

    # go to shared tab
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser sees that share named "share1" has appeared in the shared list

    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that selected share is named "share1"
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir2/dir3

    And user of browser sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser sees file browser in shared tab in Oneprovider page
    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir3

    And user of browser clicks on space1/dir2 using breadcrumbs from share info header
    Then user of browser sees that URL matches: https?://[^/]*/#/onedata/data/.*
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir2


  Scenario: User downloads files from shared directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir2
    And user of browser clicks once on item named "dir2" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared

    # go to shared tab
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser sees that share named "share1" has appeared in the shared list

    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser sees file browser in shared tab in Oneprovider page
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees that current working directory path visible in share's file browser is as follows: share1/dir2
    And user of browser double clicks on item named "file1" in file browser

    Then user of browser sees that content of downloaded file "file1" is equal to: "11111"


  Scenario: User can remove directory which contains shared directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # share dir2/dir3
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser clicks once on item named "dir3" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that "Share the directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that the modal has disappeared

    # go to shared tab
    And user of browser sees that "Share summary" modal has appeared
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser sees that share named "share1" has appeared in the shared list

    And user of browser selects "share1" from shares sidebar list
    And user of browser sees that selected share is named "share1"
    And user of browser sees that absolute share path visible in share's info header is as follows: space1/dir2/dir3

    # return to data tab
    And user of browser clicks on the "data" tab in main menu sidebar
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser selects "dir2" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir2" has disappeared from files browser

    # return to shared tab to see if share has been deleted
    And user of browser clicks on the "shared" tab in main menu sidebar
    And user of browser is idle for 10 seconds
    And user of browser refreshes site
    Then user of browser sees that share named "share1" has disappeared from the shares list
