Feature: Basic data tab operations on public shares in file browser


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
                    - dir1:
                        - file1: 11111
                        - dir2:
                            - file2: 22222

    And users opened [browser1, browser2] browsers' windows
    And user of browser1 opened z1 onezone page
    And user of browser1 logged as user1 to Onezone service
    And user of browser1 expanded the "go to your files" Onezone sidebar panel
    And user of browser1 clicked on "p1" provider in expanded "GO TO YOUR FILES" Onezone panel
    And user of browser1 clicked on the "Go to your files" button in "p1" provider's popup displayed on world map
    And user of browser1 seen that Oneprovider session has started


  Scenario: User views and downloads files from public share
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser1 clicks once on item named "dir1" in file browser
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"
    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sees an info notify with text matching to: .*copied.*
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    And user of browser2 opens received URL
    Then user of browser2 sees that URL matches: https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    And user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1
    And user of browser2 sees file browser in public share view
    And user of browser2 double clicks on item named "dir1" in file browser
    And user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1/dir1
    And user of browser2 double clicks on item named "dir2" in file browser
    And user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1/dir1/dir2
    And user of browser2 double clicks on item named "file2" in file browser
    And user of browser2 sees that content of downloaded file "file2" is equal to: "22222"


  Scenario: User sees that public share name has changed after other user renamed it
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser1 clicks once on item named "dir1" in file browser
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"
    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sees an info notify with text matching to: .*copied.*
    And user of browser1 sends copied URL to user of browser2

    And user of browser1 clicks "Open the share" confirmation button in displayed modal
    And user of browser1 sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser1 selects "share1" from shares sidebar list
    And user of browser1 sees that selected share is named "share1"
    And user of browser1 sees that absolute share path visible in share's info header is as follows: space1/dir1

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches: https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    # rename share1
    And user of browser1 clicks on settings icon displayed for "share1" item on the shares sidebar list
    And user of browser1 clicks on the "RENAME" item in settings dropdown for share named "share1"
    And user of browser1 sees that "Rename share" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "helloworld" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that "share1" has been renamed to "helloworld"

    Then user of browser2 sees that public share is named "helloworld"


  Scenario: User sees that he no longer can view public share after other user removed it
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser1 clicks once on item named "dir1" in file browser
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"
    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sees an info notify with text matching to: .*copied.*
    And user of browser1 sends copied URL to user of browser2

    And user of browser1 clicks "Open the share" confirmation button in displayed modal
    And user of browser1 sees that URL matches: https?://[^/]*/#/onedata/shares/.*
    And user of browser1 selects "share1" from shares sidebar list
    And user of browser1 sees that selected share is named "share1"
    And user of browser1 sees that absolute share path visible in share's info header is as follows: space1/dir1

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches: https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    # remove share
    And user of browser1 clicks on settings icon displayed for "share1" item on the shares sidebar list
    And user of browser1 clicks on the "REMOVE" item in settings dropdown for share named "share1"
    And user of browser1 sees that "Remove share" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that share named "share1" has disappeared from the shares list

    And user of browser2 is idle for 10 seconds
    And user of browser2 refreshes site
    And user of browser2 is idle for 3 seconds
    Then user of browser2 sees that he no longer can view the share


  Scenario: User sees new files in public share view after other user added them to shared directory
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser1 clicks once on item named "dir1" in file browser
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"
    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sees an info notify with text matching to: .*copied.*
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches: https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    And user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1
    And user of browser2 sees file browser in public share view
    And user of browser2 double clicks on item named "dir1" in file browser
    And user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1/dir1
    And user of browser2 does not see any item(s) named "file3" in file browser

    # create dir1/file3
    And user of browser1 double clicks on item named "dir1" in file browser
    And user of browser1 clicks the button from top menu bar with tooltip "Create file"
    And user of browser1 sees that "New file" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "file3" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that item named "file3" has appeared in file browser

    And user of browser2 refreshes site
    Then user of browser1 sees that item named "file3" has appeared in file browser


  Scenario: User does not see file in public share view after other user removed them from shared directory
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser1 clicks once on item named "dir1" in file browser
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"
    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sees an info notify with text matching to: .*copied.*
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches: https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    And user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1
    And user of browser2 sees file browser in public share view
    And user of browser2 double clicks on item named "dir1" in file browser
    And user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1/dir1
    And user of browser2 sees item(s) named "file1" in file browser

    # remove dir1/file2
    And user of browser1 double clicks on item named "dir1" in file browser
    And user of browser1 clicks once on item named "file1" in file browser
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that item named "file1" has disappeared from files browser

    And user of browser2 refreshes site
    And user of browser2 sees file browser in public share view
    Then user of browser2 sees that item named "file1" has disappeared from files browser


  Scenario: User changes working directory using breadcrumbs from file browser in public share view
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 sees file browser in data tab in Oneprovider page

    # share dir1
    And user of browser1 clicks once on item named "dir1" in file browser
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"
    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sees an info notify with text matching to: .*copied.*
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches: https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"
    And user of browser2 sees file browser in public share view

    And user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1
    And user of browser2 double clicks on item named "dir1" in file browser
    Then user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1/dir1
    And user of browser2 sees item(s) named "file1" in file browser
    And user of browser2 double clicks on item named "dir2" in file browser
    Then user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1/dir1/dir2
    And user of browser2 sees item(s) named "file2" in file browser
    And user of browser2 changes current working directory to share1/dir1 using breadcrumbs from public share's file browser
    Then user of browser2 sees that current working directory path visible in public share's file browser is as follows: share1/dir1
