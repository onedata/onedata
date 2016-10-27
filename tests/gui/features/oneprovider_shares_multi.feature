Feature: Oneprovider Share view
  Various operations on Share view using multiple browsers

  Background:
    Given users opened [browser1, browser2] browsers' windows
    And user of browser1 opened Onezone URL
    And user of browser1 clicked on the "plgrid" login button
    And user of browser1 clicked on the "user1" link
    And user of browser1 expanded the "go to your files" Onezone sidebar panel
    And user of browser1 clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser1 clicked on the "Go to your files" button in provider popup
    And user of browser1 seen that Oneprovider session has started


  Scenario: User views and downloads files from public share
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir1" has appeared on files list

    And user of browser1 double clicks on directory named "dir1" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-0.txt" has appeared on files list

    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir2" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir2" has appeared on files list

    And user of browser1 double clicks on directory named "dir2" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-1.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-1\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-1.txt" has appeared on files list

    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"

    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared

    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"
    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser2 double clicks on directory named "dir1" from files list
    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1/dir1
    And user of browser2 double clicks on directory named "dir2" from files list
    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1/dir1/dir2
    And user of browser2 double clicks on file named "20B-1.txt" from files list
    And user of browser2 sees that content of downloaded file "20B-1.txt" is equal to: "11111111111111111111"

    # TODO rm after integrating with swagger
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 does not see any shared directory named "dir1" on files list


  Scenario: User sees that public share name has changed after other user renamed it
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir1" has appeared on files list

    And user of browser1 double clicks on directory named "dir1" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-0.txt" has appeared on files list

    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir2" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir2" has appeared on files list

    And user of browser1 double clicks on directory named "dir2" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-1.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-1\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-1.txt" has appeared on files list

    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"

    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared

    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 closes all notifies
    And user of browser1 clicks "Open the share" confirmation button in displayed modal
    And user of browser1 sees that URL matches https?://[^/]*/#/onedata/shares/.*

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    And user of browser1 selects "share1" from shares sidebar list
    And user of browser1 sees that selected share is named "share1"
    And user of browser1 sees that absolute share path visible in share's info header is as follows: space1/dir1

    And user of browser1 clicks on settings icon displayed for "share1" item on the shares sidebar list
    And user of browser1 clicks on the "RENAME" item in settings dropdown for share named "share1"
    And user of browser1 sees that "Rename share" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "helloworld" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that "share1" has been renamed to "helloworld"

    And user of browser2 refreshes site
    And user of browser2 sees that public share is named "helloworld"

    # TODO rm after integrating with swagger
    Then user of browser1 clicks on the "data" tab in main menu sidebar
    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 does not see any shared directory named "dir1" on files list


  Scenario: User sees that he no longer can view public share after other user removed it
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir1" has appeared on files list

    And user of browser1 double clicks on directory named "dir1" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-0.txt" has appeared on files list

    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir2" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir2" has appeared on files list

    And user of browser1 double clicks on directory named "dir2" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-1.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-1\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-1.txt" has appeared on files list

    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"

    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared

    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 closes all notifies
    And user of browser1 clicks "Open the share" confirmation button in displayed modal
    And user of browser1 sees that URL matches https?://[^/]*/#/onedata/shares/.*

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    And user of browser1 selects "share1" from shares sidebar list
    And user of browser1 sees that selected share is named "share1"
    And user of browser1 sees that absolute share path visible in share's info header is as follows: space1/dir1

    And user of browser1 clicks on settings icon displayed for "share1" item on the shares sidebar list
    And user of browser1 clicks on the "REMOVE" item in settings dropdown for share named "share1"
    And user of browser1 sees that "Remove share" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that share named "share1" has disappeared from the shares list

    And user of browser2 refreshes site
    And user of browser2 sees that he no longer can view the share

    # TODO rm after integrating with swagger
    Then user of browser1 clicks on the "data" tab in main menu sidebar
    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 does not see any shared directory named "dir1" on files list


  Scenario: User sees new files in public share view after other user added them to shared directory
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir1" has appeared on files list

    And user of browser1 double clicks on directory named "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir2" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir2" has appeared on files list

    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"

    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared

    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser2 double clicks on directory named "dir1" from files list
    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1/dir1
    And user of browser2 does not see any file named "file2" on files list

    And user of browser1 double clicks on directory named "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Create file"
    And user of browser1 sees that "New file" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "file2" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that file named "file2" has appeared on files list

    And user of browser2 refreshes site
    And user of browser2 sees that file named "file2" has appeared on files list

    # TODO rm after integrating with swagger
    Then user of browser1 clicks on the "data" tab in main menu sidebar
    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 does not see any shared directory named "dir1" on files list


  Scenario: User does not see file in public share view after other user removed them from shared directory
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir1" has appeared on files list

    And user of browser1 double clicks on directory named "dir1" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-0.txt" has appeared on files list

    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir2" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir2" has appeared on files list

    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"

    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared

    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser2 double clicks on directory named "dir1" from files list
    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1/dir1
    And user of browser2 sees file named "20B-0.txt" on files list

    And user of browser1 double clicks on directory named "dir1" from files list
    And user of browser1 selects "20B-0.txt" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that file named "20B-0.txt" has disappeared from files list

    And user of browser2 refreshes site
    And user of browser2 sees that file named "file2" has disappeared from files list

    # TODO rm after integrating with swagger
    Then user of browser1 clicks on the "data" tab in main menu sidebar
    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 does not see any shared directory named "dir1" on files list


  Scenario: User changes working directory using breadcrumbs from file browser in public share view
    When user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir1" has appeared on files list

    And user of browser1 double clicks on directory named "dir1" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-0.txt" has appeared on files list

    And user of browser1 clicks the button from top menu bar with tooltip "Create directory"
    And user of browser1 sees that "New directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "dir2" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that directory named "dir2" has appeared on files list

    And user of browser1 double clicks on directory named "dir2" from files list
    And user of browser1 uses upload button in toolbar to upload file "20B-1.txt" to current dir
    And user of browser1 sees an info notify with text matching to: .*20B-1\.txt.*uploaded successfully.*
    And user of browser1 sees that file named "20B-1.txt" has appeared on files list

    # in order to change cwd to root dir change space to other than change back
    And user of browser1 uses spaces select to change data space to "Small space"
    And user of browser1 uses spaces select to change data space to "space1"
    And user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Share element"

    And user of browser1 sees that "Share the directory" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types "share1" on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees an info notify with text matching to: Share created sucessfully
    And user of browser1 sees that the modal has disappeared

    And user of browser1 sees that "Share summary" modal has appeared
    And user of browser1 clicks on copy button in active modal
    And user of browser1 sends copied URL to user of browser2
    And user of browser1 clicks "Close" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    And user of browser2 opens received URL
    And user of browser2 sees that URL matches https?://[^/]*/#/public/shares/.*
    And user of browser2 sees that public share is named "share1"

    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1
    And user of browser2 double clicks on directory named "dir1" from files list
    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1/dir1
    And user of browser2 sees file named "20B-0.txt" on files list
    And user of browser2 double clicks on directory named "dir2" from files list
    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1/dir1/dir2
    And user of browser2 sees file named "20B-1.txt" on files list
    And user of browser2 changes current working directory to share1/dir1 using breadcrumbs from share's file browser
    And user of browser2 sees that current working directory path visible in share's file browser is as follows: share1/dir1
    And user of browser2 sees file named "20B-0.txt" on files list

    # TODO rm after integrating with swagger
    Then user of browser1 selects "dir1" from files list
    And user of browser1 clicks the button from top menu bar with tooltip "Remove element"
    And user of browser1 sees that "Remove files" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees an info notify with text matching to: .*removed.*
    And user of browser1 sees that the modal has disappeared
    And user of browser1 does not see any shared directory named "dir1" on files list
