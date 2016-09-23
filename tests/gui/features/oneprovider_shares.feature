Feature: Oneprovider Share view
  Various operations on Share view

  Background:
    Given user opened browser window
    And user of browser opened Onezone URL
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User creates share and see share icon on shared directory (presses ENTER after typing share name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "directory1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "directory1" has appeared in file list

    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Share element"

    And user of browser sees that input box in "Share the directory" modal is active
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that "Share the directory" modal has disappeared

    And user of browser sees that input box in "Share summary" modal is active
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that "Share summary" modal has disappeared
    And user of browser sees that directory named "directory1" has became shared with alias "share1"

    # TODO rm after integrating with swagger
    Then user of browser uses spaces select to change data space to "Small space"
    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that shared directory named "directory1" has disappeared from file list


  Scenario: User creates share and see share icon on shared directory (clicks CREATE confirmation button after typing share name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "directory1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "directory1" has appeared in file list

    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Share element"

    And user of browser sees that input box in "Share the directory" modal is active
    And user of browser types "share1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that "Share the directory" modal has disappeared

    And user of browser sees that input box in "Share summary" modal is active
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that "Share summary" modal has disappeared
    And user of browser sees that directory named "directory1" has became shared with alias "share1"

    # TODO rm after integrating with swagger
    Then user of browser uses spaces select to change data space to "Small space"
    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that shared directory named "directory1" has disappeared from file list


  Scenario: Opening shares index view after removing shared dir
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "directory1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "directory1" has appeared in file list

    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Share element"

    And user of browser sees that input box in "Share the directory" modal is active
    And user of browser types "share1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that new share named "share1" was created from directory "directory1"
    And user of browser sees that "Share the directory" modal has disappeared

    And user of browser sees that input box in "Share summary" modal is active
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that "Share summary" modal has disappeared
    And user of browser sees that url matches https?://[^/]*/#/onedata/shares/.*
    And user of browser clicks on the "data" tab in main menu sidebar

    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*

    And user of browser clicks on the "shared" tab in main menu sidebar
    And user of browser does not see any share info

#    # TODO rm after integrating with swagger
    Then user of browser clicks on the "data" tab in main menu sidebar
    And user of browser uses spaces select to change data space to "space1"
    And user of browser sees that shared directory named "directory1" has disappeared from file list


  Scenario: User fails to share already shared directory
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "directory1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "directory1" has appeared in file list

    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Share element"

    And user of browser sees that input box in "Share the directory" modal is active
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that new share named "share1" was created from directory "directory1"
    And user of browser sees that "Share the directory" modal has disappeared

    And user of browser sees that input box in "Share summary" modal is active
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that "Share summary" modal has disappeared

    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Share element"
    And user of browser sees that input box in "Share summary" modal is active
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that "Share summary" modal has disappeared

    # TODO rm after integrating with swagger
    Then user of browser uses spaces select to change data space to "Small space"
    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that shared directory named "directory1" has disappeared from file list


  Scenario: User creates share and sees it's info
    When user of browser uses spaces select to change data space to "space1"
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "directory1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "directory1" has appeared in file list

    And user of browser double clicks on directory "directory1" from files list
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*
    And user of browser sees that file named "20B-0.txt" has appeared in file list

    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "directory2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New directory" modal has disappeared
    And user of browser sees that directory named "directory2" has appeared in file list

    And user of browser double clicks on directory "directory2" from files list
    And user of browser uses upload button in toolbar to upload file "20B-1.txt" to current dir
    And user of browser sees an info notify with text matching to: .*20B-1\.txt.*uploaded successfully.*
    And user of browser sees that file named "20B-1.txt" has appeared in file list

    And user of browser uses spaces select to change data space to "Small space"
    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Share element"

    And user of browser sees that input box in "Share the directory" modal is active
    And user of browser types "share1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees an info notify with text matching to: Share created sucessfully
    And user of browser sees that new share named "share1" was created from directory "directory1"
    And user of browser sees that "Share the directory" modal has disappeared

    And user of browser sees that input box in "Share summary" modal is active
    And user of browser clicks "Open the share" confirmation button in displayed modal
    And user of browser sees that "Share summary" modal has disappeared
    And user of browser sees that url matches https?://[^/]*/#/onedata/shares/.*

    And user of browser selects "share1" from shares list
    And user of browser sees valid share info for "share1"

    # TODO rm after integrating with swagger
    Then user of browser clicks on the "data" tab in main menu sidebar
    And user of browser uses spaces select to change data space to "Small space"
    And user of browser uses spaces select to change data space to "space1"
    And user of browser selects directory1 from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser sees that shared directory named "directory1" has disappeared from file list
