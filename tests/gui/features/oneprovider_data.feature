Feature: Oneprovider Data view
  Various operations on Data view

  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open a Onezone URL in [Firefox, Chrome]
    Given user opened browser window
    And user of browser opened Onezone URL
    # not used in non-homepage tests
    # And user clicks on the "login" link in Homepage main menu
    And user of browser clicked on the "plgrid" login button
    And user of browser clicked on the "user1" link
    And user of browser expanded the "go to your files" Onezone sidebar panel
    And user of browser clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicked on the "Go to your files" button in provider popup
    And user of browser seen that Oneprovider session has started


#  Scenario: User downloads file and checks it's content
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser uses upload button in toolbar to upload file "20B-1.txt" to current dir
#    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
#    And user of browser sees that file named "20B-1.txt" has appeared on files list
#    And user of browser double clicks on file named "20B-1.txt" of files list
#    Then user of browser sees that content of downloaded file "20B-1.txt" is equal to: "11111111111111111111"
#
#
#  Scenario: User uploads a small file to space that accepts large files
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
#    Then user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
#    And user of browser sees that file named "20B-0.txt" has appeared on files list
#
#    # TODO rm after integrating with swagger
#    And user of browser clicks once on file named "20B-0.txt" of files list
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that file named "20B-0.txt" has disappeared from files list









#  Scenario: User creates new file (presses ENTER after entering file name)
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees empty file browser in data tab in Oneprovider page
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser clicks the button from top menu bar with tooltip "Create file"
#    And user of browser sees that "New file" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "file1" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    Then user of browser sees that item(s) named "file1" has(have) appeared in file browser
#    And user of browser sees that item named "file1" is file in file browser
#
#    # TODO rm after integrating with swagger
#    And user of browser clicks once on item named "file1" in file browser
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that item(s) named "file1" has(have) disappeared from files browser
#
#
#  Scenario: User creates new file (clicks CREATE confirmation button after entering file name)
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees empty file browser in data tab in Oneprovider page
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser clicks the button from top menu bar with tooltip "Create file"
#    And user of browser sees that "New file" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "file1" on keyboard
#    And user of browser clicks "Create" confirmation button in displayed modal
#    And user of browser sees that the modal has disappeared
#    Then user of browser sees that item(s) named "file1" has(have) appeared in file browser
#    And user of browser sees that item named "file1" is file in file browser
#
#    # TODO rm after integrating with swagger
#    And user of browser clicks once on item named "file1" in file browser
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that item(s) named "file1" has(have) disappeared from files browser
#
#
#  Scenario: User creates new directory (presses ENTER after entering dir name)
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees empty file browser in data tab in Oneprovider page
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir1" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    Then user of browser sees that item(s) named "dir1" has(have) appeared in file browser
#    And user of browser sees that item named "dir1" is directory in file browser
#
#    # TODO rm after integrating with swagger
#    And user of browser clicks once on item named "dir1" in file browser
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that item(s) named "dir1" has(have) disappeared from files browser
#
#
#  Scenario: User creates new directory (clicks CREATE confirmation button after entering dir name)
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees empty file browser in data tab in Oneprovider page
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir1" on keyboard
#    And user of browser clicks "Create" confirmation button in displayed modal
#    And user of browser sees that the modal has disappeared
#    Then user of browser sees that item(s) named "dir1" has(have) appeared in file browser
#    And user of browser sees that item named "dir1" is directory in file browser
#
#    # TODO rm after integrating with swagger
#    And user of browser clicks once on item named "dir1" in file browser
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that item(s) named "dir1" has(have) disappeared from files browser
#
#
#  Scenario: User fails to create new directory because of existing directory with given name
#    # TODO rm after integrating with swagger (test setup: create dir1)
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees empty file browser in data tab in Oneprovider page
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir1" on keyboard
#    And user of browser clicks "Create" confirmation button in displayed modal
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that item(s) named "dir1" has(have) appeared in file browser
#    And user of browser sees that item named "dir1" is directory in file browser
#
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir1" on keyboard
#    And user of browser clicks "Create" confirmation button in displayed modal
#    And user of browser sees that the modal has disappeared
#    And user of browser sees an error notify with text matching to: .*failed.*
#
#    # TODO rm after integrating with swagger
#    And user of browser clicks once on item named "dir1" in file browser
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that item(s) named "dir1" has(have) disappeared from files browser
#
#
#  Scenario: User removes existing directory
#    # TODO rm after integrating with swagger (test setup: create dir1)
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees empty file browser in data tab in Oneprovider page
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir1" on keyboard
#    And user of browser clicks "Create" confirmation button in displayed modal
#    And user of browser sees that the modal has disappeared
#
#    And user of browser sees item(s) named "dir1" in file browser
#    And user of browser sees that item named "dir1" is directory in file browser
#    And user of browser clicks once on item named "dir1" in file browser
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    Then user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that item(s) named "dir1" has(have) disappeared from files browser






#  Scenario: User creates file and then removes it
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser clicks the button from top menu bar with tooltip "Create file"
#    And user of browser sees that "New file" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "file2" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that file named "file2" has appeared on files list
#
#    # TODO rm after integrating with swagger
#    And user of browser clicks once on file named "file2" of files list
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that file named "file2" has disappeared from files list
#
#
#  # 'space1' supported by 'p1' defined in env.json
#  Scenario: User creates file and checks if provider name is displayed in the file distribution panel
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser clicks the button from top menu bar with tooltip "Create file"
#    And user of browser sees that "New file" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "file3" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that file named "file3" has appeared on files list
#    And user of browser clicks once on file named "file3" of files list
#    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
#    And user of browser sees that "File distribution" modal has appeared
#    Then user of browser sees modal with name of provider supporting space in providers column
#    And user of browser clicks "Close" confirmation button in displayed modal
#    And user of browser sees that the modal has disappeared
#
#
#  Scenario: User creates 3 new files, selects and removes them
#    When user of browser uses spaces select to change data space to "space1"
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#
#    # create file1
#    And user of browser clicks the button from top menu bar with tooltip "Create file"
#    And user of browser sees that "New file" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "file1" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that file named "file1" has appeared on files list
#
#    # create file2
#    And user of browser clicks the button from top menu bar with tooltip "Create file"
#    And user of browser sees that "New file" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "file2" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that file named "file2" has appeared on files list
#
#    # create file3
#    And user of browser clicks the button from top menu bar with tooltip "Create file"
#    And user of browser sees that "New file" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "file3" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that file named "file3" has appeared on files list
#
#    And user of browser selects ["file1", "file2", "file3"] from files list
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser sees that message displayed in modal matches: .*3.*
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*3.*removed.*
#    And user of browser sees that the modal has disappeared
#
#    Then user of browser sees that files named ["file1", "file2", "file3"] have disappeared from files list





#  Scenario: User sees that after going to Oneprovider, without having any home space, the first one alphabetically is loaded into view
#    When user of browser sees that displayed directory tree in sidebar panel belongs to space named "space1"
#    And user of browser uses spaces select to change data space to "A"
#    Then user of browser sees that displayed directory tree in sidebar panel belongs to space named "A"
#
#
#  Scenario: User changes directory using breadcrumbs
#    When user of browser uses spaces select to change data space to "space1"
#
#    # create dir1 in space1
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir1" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that directory named "dir1" has appeared on files list
#
#    # create dir2 in space1/dir1
#    And user of browser double clicks on directory named "dir1" of files list
#    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir1
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir2" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that directory named "dir2" has appeared on files list
#
#    # create dir3 in space1/dir1/dir2
#    And user of browser double clicks on directory named "dir2" of files list
#    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir1/dir2
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir3" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that directory named "dir3" has appeared on files list
#
#    And user of browser double clicks on directory named "dir3" of files list
#    Then user of browser sees that current working directory displayed in breadcrumbs is space1/dir1/dir2/dir3
#
#    And user of browser changes current working directory to space1 using breadcrumbs
#    And user of browser sees that current working directory displayed in breadcrumbs is space1
#    And user of browser sees directory named "dir1" on files list
#
#    # TODO rm after integrating with swagger
#    And user of browser selects "dir1" from files list
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser does not see any shared directory named "dir1" on files list
#
#
#  Scenario: User changes directory (forward and backward ) using directory tree
#    When user of browser uses spaces select to change data space to "space1"
#
#    # create dir1 in space1
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir1" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that directory named "dir1" has appeared on files list
#
#    # create dir2 in space1/dir1
#    And user of browser double clicks on directory named "dir1" of files list
#    And user of browser sees that current working directory displayed in directory tree is /dir1/
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir2" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that directory named "dir2" has appeared on files list
#
#    # create dir3 in space1/dir1/dir2
#    And user of browser double clicks on directory named "dir2" of files list
#    And user of browser sees that current working directory displayed in directory tree is /dir1/dir2/
#    And user of browser clicks the button from top menu bar with tooltip "Create directory"
#    And user of browser sees that "New directory" modal has appeared
#    And user of browser clicks on input box in active modal
#    And user of browser types "dir3" on keyboard
#    And user of browser presses enter on keyboard
#    And user of browser sees that the modal has disappeared
#    And user of browser sees that directory named "dir3" has appeared on files list
#
#    And user of browser double clicks on directory named "dir3" of files list
#    Then user of browser sees that current working directory displayed in directory tree is /dir1/dir2/dir3/
#
#    And user of browser changes current working directory to / using directory tree
#    And user of browser sees that current working directory displayed in directory tree is /
#    And user of browser sees directory named "dir1" on files list
#    And user of browser refreshes site
#    And user of browser does not see /dir1/dir2/dir3/ in directory tree
#    And user of browser changes current working directory to /dir1/dir2/dir3/ using directory tree
#    And user of browser sees that current working directory displayed in directory tree is /dir1/dir2/dir3/
#    And user of browser sees empty file browser in data tab in Oneprovider page
#
#    # TODO rm after integrating with swagger
#    And user of browser changes current working directory to / using directory tree
#    And user of browser selects "dir1" from files list
#    And user of browser clicks the button from top menu bar with tooltip "Remove element"
#    And user of browser sees that "Remove files" modal has appeared
#    And user of browser clicks "Yes" confirmation button in displayed modal
#    And user of browser sees an info notify with text matching to: .*removed.*
#    And user of browser sees that the modal has disappeared
#    And user of browser does not see any directory named "dir1" on files list
