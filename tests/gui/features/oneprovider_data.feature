Feature: Oneprovider Data view
  Various operations on Data view

  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open a Onezone URL in [Firefox, Chrome]
    Given there is user named "user1" in the system
    And user opened browser window
    And user of browser opened Onezone URL
    # not used in non-homepage tests
    # And user clicks on the "login" link in Homepage main menu
    And user of browser clicked on the "username" login button
    And user of browser seen that "Login with username and password" modal has appeared
    And user of browser entered credentials of user1 in "Login with username and password" modal
    And user of browser clicked "Sign In" confirmation button in displayed modal

    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "Example Provider" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


  Scenario: User downloads file and checks it's content
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-1.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-1.txt" has appeared in file browser
    And user of browser double clicks on item named "20B-1.txt" in file browser
    Then user of browser sees that content of downloaded file "20B-1.txt" is equal to: "11111111111111111111"

    # TODO rm after integrating with swagger
    And user of browser selects "20B-1.txt" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-1.txt" has disappeared from files browser


  Scenario: User uploads a small file to space that accepts large files
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    Then user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "20B-0.txt" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser


  Scenario: User creates new file (presses ENTER after entering file name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User creates new file (clicks CREATE confirmation button after entering file name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that item named "file1" has appeared in file browser
    And user of browser sees that item named "file1" is file in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User fails to create new file because of existing file with given name
    # TODO rm after integrating with swagger (test setup: create dir1)
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

    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    Then user of browser sees that the modal has disappeared
    And user of browser sees an error notify with text matching to: .*failed.*

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User removes existing file
    # TODO rm after integrating with swagger (test setup: create dir1)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser sees item(s) named "file1" in file browser
    And user of browser sees that item named "file1" is file in file browser
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User renames file (presses ENTER after entering file name)
    # TODO rm after integrating with swagger (test setup: create dir1)
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
    And user of browser types "new_file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user of browser sees an info notify with text matching to: .*renamed.*
    And user of browser sees that item named "file1" has disappeared from files browser
    And user of browser sees that item named "new_file1" has appeared in file browser
    And user of browser sees that item named "new_file1" is file in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "new_file1" has disappeared from files browser


  Scenario: User renames file (clicks OK confirmation button after entering file name)
    # TODO rm after integrating with swagger (test setup: create dir1)
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
    And user of browser types "new_file1" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees an info notify with text matching to: .*renamed.*
    And user of browser sees that item named "file1" has disappeared from files browser
    And user of browser sees that item named "new_file1" has appeared in file browser
    And user of browser sees that item named "new_file1" is file in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "new_file1" has disappeared from files browser


  Scenario: User creates new directory (presses ENTER after entering dir name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User creates new directory (clicks CREATE confirmation button after entering dir name)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees that item named "dir1" has appeared in file browser
    And user of browser sees that item named "dir1" is directory in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User fails to create new directory because of existing directory with given name
    # TODO rm after integrating with swagger (test setup: create dir1)
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

    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    Then user of browser sees an error notify with text matching to: .*failed.*

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User removes existing directory
    # TODO rm after integrating with swagger (test setup: create dir1)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    And user of browser sees item(s) named "dir1" in file browser
    And user of browser sees that item named "dir1" is directory in file browser
    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User renames directory (presses ENTER after entering dir name)
    # TODO rm after integrating with swagger (test setup: create dir1)
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

    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "new_dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    Then user of browser sees an info notify with text matching to: .*renamed.*
    And user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "new_dir1" has appeared in file browser
    And user of browser sees that item named "new_dir1" is directory in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "new_dir1" has disappeared from files browser


  Scenario: User renames directory (clicks OK confirmation button after entering dir name)
    # TODO rm after integrating with swagger (test setup: create dir1)
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

    And user of browser clicks once on item named "dir1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Rename element"
    And user of browser sees that "Rename file or directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "new_dir1" on keyboard
    And user of browser clicks "OK" confirmation button in displayed modal
    Then user of browser sees that the modal has disappeared
    And user of browser sees an info notify with text matching to: .*renamed.*
    And user of browser sees that item named "dir1" has disappeared from files browser
    And user of browser sees that item named "new_dir1" has appeared in file browser
    And user of browser sees that item named "new_dir1" is directory in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "new_dir1" has disappeared from files browser


  # 'space1' supported by 'Example Provider' defined in env.json
  Scenario: User creates file and checks if provider name is displayed in the file distribution panel
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    Then user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    And user of browser clicks once on file named "20B-0.txt" of files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    Then user of browser sees that chunk bar for provider named "Example Provider" is entirely filled
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser


  Scenario: User selects a bunch of files using ctrl
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file3" has appeared in file browser

    Then user of browser sees item(s) named ["file3", "file2", "file1"] in file browser in given order
    And user of browser selects ["file3", "file1"] item(s) from file browser with pressed ctrl
    And user of browser sees that ["file3", "file1"] items are selected in file browser
    And user of browser sees that "file2" item is not selected in file browser

    # TODO rm after integrating with swagger
    And user of browser selects "file2" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["file3", "file2", "file1"] have disappeared from files browser


  Scenario: User selects bunch of files using shift
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file3" has appeared in file browser

    Then user of browser sees item(s) named ["file3", "file2", "file1"] in file browser in given order
    And user of browser selects ["file3", "file1"] item(s) from file browser with pressed shift
    And user of browser sees that ["file3", "file2", "file1"] items are selected in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["file3", "file2", "file1"] have disappeared from files browser


  Scenario: User selects bunch of files using ctrl and shift
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file3" has appeared in file browser

    # create file4
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file4" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file4" has appeared in file browser

    # create file5
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file5" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file5" has appeared in file browser

    # create file6
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file6" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file6" has appeared in file browser

    # create file7
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file7" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file7" has appeared in file browser

    Then user of browser sees item(s) named ["file7", "file6", "file5", "file4", "file3", "file2", "file1"] in file browser in given order
    And user of browser selects ["file3", "file1"] item(s) from file browser with pressed shift
    And user of browser selects "file5" item(s) from file browser with pressed ctrl
    And user of browser selects "file7" item(s) from file browser with pressed shift
    And user of browser sees that ["file7", "file6", "file5", "file3", "file2", "file1"] items are selected in file browser

    # TODO rm after integrating with swagger
    And user of browser selects "file4" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["file7", "file6", "file5", "file4", "file3", "file2", "file1"] have disappeared from files browser


  Scenario: User changes viewed space
    When user of browser sees that displayed directory tree in sidebar panel belongs to space named "space1"
    And user of browser uses spaces select to change data space to "A"
    Then user of browser sees that displayed directory tree in sidebar panel belongs to space named "A"


  Scenario: User changes directory using breadcrumbs
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

    And user of browser double clicks on item named "dir3" in file browser
    Then user of browser sees that current working directory displayed in breadcrumbs is space1/dir1/dir2/dir3

    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser sees directory named "dir1" on files list

    # TODO rm after integrating with swagger
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User changes directory using sidebar directory tree view (with unfolding of folded directories)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    # create dir2 in space1/dir1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that current working directory displayed in directory tree is /dir1/
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir2" has appeared in file browser

    # create dir3 in space1/dir1/dir2
    And user of browser double clicks on item named "dir2" in file browser
    And user of browser sees that current working directory displayed in directory tree is /dir1/dir2/
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir3" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir3" has appeared in file browser

    And user of browser double clicks on item named "dir3" in file browser
    Then user of browser sees that current working directory displayed in directory tree is /dir1/dir2/dir3/

    And user of browser changes current working directory to / using directory tree
    And user of browser sees that current working directory displayed in directory tree is /
    And user of browser sees directory named "dir1" on files list
    And user of browser refreshes site
    And user of browser does not see /dir1/dir2/dir3/ in directory tree
    And user of browser changes current working directory to /dir1/dir2/dir3/ using directory tree
    And user of browser sees that current working directory displayed in directory tree is /dir1/dir2/dir3/
    And user of browser sees file browser in data tab in Oneprovider page

    # TODO rm after integrating with swagger
    And user of browser changes current working directory to / using directory tree
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User sees that without any file selected only ["Create directory", "Create file", "Upload file"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that ["Create directory", "Create file", "Upload file"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Share element", "Edit metadata", "Rename element", "Change element permissions", "Copy element", "Cut element", "Remove element"] buttons are disabled in toolbar in data tab in Oneprovider gui


  Scenario: User sees that with only one file selected only ["Create directory", "Create file", "Edit metadata", "Upload file", "Rename element", "Change element permissions", "Remove element", "Show file distribution"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects "file1" item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Edit metadata", "Upload file", "Rename element", "Change element permissions", "Remove element", "Show file distribution"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Share element", "Copy element", "Cut element"] buttons are disabled in toolbar in data tab in Oneprovider gui

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User sees that with only one directory selected only ["Create directory", "Create file", "Share element", "Edit metadata", "Upload file", "Rename element", "Change element permissions", "Remove element"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Share element", "Edit metadata", "Upload file", "Rename element", "Change element permissions", "Remove element"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Copy element", "Cut element", "Show file distribution"] buttons are disabled in toolbar in data tab in Oneprovider gui

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User sees that with several files selected only ["Create directory", "Create file", "Upload file", "Change element permissions", "Remove element"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    And user of browser selects ["file1", "file2"] item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Upload file", "Change element permissions", "Remove element"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Edit metadata", "Share element", "Rename element", "Copy element", "Cut element", "Show file distribution"] buttons are disabled in toolbar in data tab in Oneprovider gui

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["file1", "file2"] have disappeared from files browser


  Scenario: User sees that with several directories selected only ["Create directory", "Create file", "Upload file", "Change element permissions", "Remove element"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    # create dir2
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir2" has appeared in file browser

    And user of browser selects ["dir1", "dir2"] item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Upload file", "Change element permissions", "Remove element"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Share element", "Edit metadata", "Rename element", "Copy element", "Cut element", "Show file distribution"] buttons are disabled in toolbar in data tab in Oneprovider gui

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["dir1", "dir2"] have disappeared from files browser


  Scenario: User sees that with directory and file selected only ["Create directory", "Create file", "Upload file", "Remove element"] buttons from toolbar are enabled
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has appeared in file browser

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    And user of browser selects ["dir1", "file1"] item(s) from file browser with pressed ctrl
    Then user of browser sees that ["Create directory", "Create file", "Upload file", "Remove element"] buttons are enabled in toolbar in data tab in Oneprovider gui
    And user of browser sees that ["Share element", "Edit metadata", "Rename element", "Change element permissions", "Copy element", "Cut element", "Show file distribution"] buttons are disabled in toolbar in data tab in Oneprovider gui

    # TODO rm after integrating with swagger
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["dir1", "file1"] have disappeared from files browser


  Scenario: User removes several files
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file3" has appeared in file browser

    And user of browser selects ["file1", "file2", "file3"] item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["file1", "file2", "file3"] have disappeared from files browser


  Scenario: User removes directory containing several files
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

    # create file1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file3" has appeared in file browser

    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    Then user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User sees file size after upload and after site refresh
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    Then user of browser sees that item named "20B-0.txt" is of 20 B size in file browser
    And user of browser refreshes site
    And user of browser sees nonempty file browser in data tab in Oneprovider page
    And user of browser sees that item named "20B-0.txt" is of 20 B size in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "20B-0.txt" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser


  Scenario: User sees modification date after uploading file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser
    Then user of browser sees that modification date of item named "20B-0.txt" is not earlier than 120 seconds ago in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "20B-0.txt" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "20B-0.txt" has disappeared from files browser


  Scenario: User sees modification date after creating file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser
    Then user of browser sees that modification date of item named "file1" is not earlier than 120 seconds ago in file browser

    # TODO rm after integrating with swagger
    And user of browser clicks once on item named "file1" in file browser
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has disappeared from files browser


  Scenario: User creates files and sees that they are ordered on list by creation order
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file3" has appeared in file browser

    # create file4
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file4" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file4" has appeared in file browser

    # create file5
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file5" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file5" has appeared in file browser

    And user of browser sees item(s) named ["file5", "file4", "file3", "file2", "file1"] in file browser in given order
    And user of browser refreshes site
    And user of browser sees nonempty file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["file1", "file2", "file3", "file4", "file5"] in file browser in given order

    # TODO rm after integrating with swagger
    And user of browser selects ["file1", "file2", "file3", "file4", "file5"] item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["file5", "file4", "file3", "file2", "file1"] have disappeared from files browser


  Scenario: User upload files and sees they are ordered on list by upload order (uploads one file at time)
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file3" has appeared in file browser

    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser uses upload button in toolbar to upload file "20B-1.txt" to current dir

    And user of browser sees item(s) named ["20B-1.txt", "file3", "file2", "file1"] in file browser in given order
    And user of browser sees item(s) named ["20B-0.txt", "file3", "file2", "file1"] in file browser in given order
    And user of browser refreshes site
    And user of browser sees nonempty file browser in data tab in Oneprovider page
    Then user of browser sees item(s) named ["20B-0.txt", "20B-1.txt", "file1", "file2", "file3"] in file browser in given order

    # TODO rm after integrating with swagger
    And user of browser selects ["20B-0.txt", "20B-1.txt", "file1", "file2", "file3"] item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["20B-0.txt", "20B-1.txt", "file1", "file2", "file3"] have disappeared from files browser


  Scenario: User enters directory and views files in it
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

    # create file1
    And user of browser double clicks on item named "dir1" in file browser
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file1" has appeared in file browser

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file2" has appeared in file browser

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "file3" has appeared in file browser

    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser refreshes site
    And user of browser sees nonempty file browser in data tab in Oneprovider page
    And user of browser double clicks on item named "dir1" in file browser
    Then user of browser sees item(s) named ["file1", "file2", "file3"] in file browser in given order

    # TODO rm after integrating with swagger
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User opens nested directory via url and sees expanded directory tree in sidebar
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

    And user of browser double clicks on item named "dir3" in file browser
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir1/dir2/dir3
    And user of browser copies url from browser's location bar
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser refreshes site
    And user of browser sees directory named "dir1" on files list
    And user of browser does not see /dir1/dir2/dir3/ in directory tree
    And user of browser opens copied URL in browser's location bar
    And user of browser sees file browser in data tab in Oneprovider page
    Then user of browser sees that current working directory displayed in directory tree is /dir1/dir2/dir3/

    # TODO rm after integrating with swagger
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser selects "dir1" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "dir1" has disappeared from files browser


  Scenario: User sees that after uploading file with name of already existing file, the uploaded file appeared with suffix
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that item named "20B-0.txt" has appeared in file browser

    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    Then user of browser sees that item named "20B-0(1).txt" has appeared in file browser

    # TODO rm after integrating with swagger
    And user of browser selects ["20B-0.txt", "20B-0(1).txt"] item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that items named ["20B-0.txt", "20B-0(1).txt"] have disappeared from files browser


  Scenario: User sees that text area for directory name gets bigger while resizing directory tree sidebar
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees file browser in data tab in Oneprovider page

    # create dir1 in space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab" on keyboard
    And user of browser clicks "Create" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab" has appeared in file browser

    And user of browser records displayed name length for /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab/ in directory tree sidebar
    And user of browser expands data tab sidebar to the right of approximately 200px
    Then user of browser sees that displayed name length for /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab/ in directory tree sidebar is larger than before

    # TODO rm after integrating with swagger
    And user of browser selects "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab" item(s) from file browser with pressed ctrl
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that item named "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab" has disappeared from files browser


  Scenario: User sees empty directory message in directory without items
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees empty file browser in data tab in Oneprovider page
    Then user of browser sees empty directory message in file browser
