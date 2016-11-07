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


  Scenario: User downloads file and checks it's content
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-1.txt" to current dir
    And user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that file named "20B-1.txt" has appeared on files list
    And user of browser double clicks on file named "20B-1.txt" from files list
    Then user of browser sees that content of downloaded file "20B-1.txt" is equal to: "11111111111111111111"


  Scenario: User uploads a small file to space that accepts large files
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    Then user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*1.*
    And user of browser sees that file named "20B-0.txt" has appeared on files list

    # TODO rm after integrating with swagger
    And user of browser clicks on file named "20B-0.txt" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "20B-0.txt" has disappeared from files list


  Scenario: User creates new file and then removes it
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    # TODO rm after integrating with swagger
    And user of browser clicks on file named "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has disappeared from files list


  Scenario: User creates new directory and then removes it
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "directory1" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees that the modal has disappeared
    And user of browser sees that directory named "directory1" has appeared on files list

    # TODO rm after integrating with swagger
    And user of browser selects "directory1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "directory1" has disappeared from files list


  Scenario: User creates file and then removes it
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file2" has appeared on files list

    # TODO rm after integrating with swagger
    And user of browser clicks on file named "file2" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file2" has disappeared from files list


  # 'space1' supported by 'p1' defined in env.json
  Scenario: User creates file and checks if provider name is displayed in the file distribution panel
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file3" has appeared on files list
    And user of browser clicks on file named "file3" from files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    And user of browser sees that "File distribution" modal has appeared
    Then user of browser sees modal with name of provider supporting space in providers column
    And user of browser clicks "Close" confirmation button in displayed modal
    And user of browser sees that the modal has disappeared


  # 'space1' supported by 'p1' defined in env.json
  Scenario: User uploads 5 files at once
    Given user of browser has 5 files in directory named "my_files"
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir10" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir10" has appeared on files list
    And user of browser double clicks on directory named "dir10" of files list
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir10
    And user of browser uses upload button in toolbar to upload files from local directory "my_files" to remote current dir
    Then user of browser sees an info notify with text matching to: .*[Cc]ompleted upload.*5.*
    And user of browser sees that file browser contains 5 file(s)

    # TODO rm after integrating with swagger
    # in order to change cwd to root dir change space to other than change back
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser selects "dir10" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser does not see any directory named "dir10" on files list


  # 'space1' supported by 'p1' defined in env.json
  Scenario: User uploads more than 50 files and uses files list lazy loading
    Given user of browser has 70 files in directory named "my_files"
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir100" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir100" has appeared on files list
    And user of browser double clicks on directory named "dir100" of files list
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir100
    And user of browser uses upload button in toolbar to upload files from local directory "my_files" to remote current dir
    And user of browser is idle for 5 seconds
    And user of browser sees that file browser contains 70 file(s)
    And user of browser refreshes site
    And user of browser sees that content of current directory has been loaded
    And user of browser sees that file browser contains 50 file(s)
    And user of browser scrolls to the bottom of file list in file browser
    And user of browser is idle for 10 seconds
    Then user of browser sees that file browser contains 70 file(s)

    # TODO rm after integrating with swagger
    # in order to change cwd to root dir change space to other than change back
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser selects "dir100" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser does not see any directory named "dir100" on files list


  Scenario: User creates 3 new files, selects and removes them
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create file1
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file1" has appeared on files list

    # create file2
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file2" has appeared on files list

    # create file3
    And user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that "New file" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "file3" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that file named "file3" has appeared on files list

    And user of browser selects ["file1", "file2", "file3"] from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser sees that message displayed in modal matches: .*3.*
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*3.*removed.*
    And user of browser sees that the modal has disappeared

    Then user of browser sees that files named ["file1", "file2", "file3"] have disappeared from files list


  Scenario: User changes directory while uploading bunch of files
    Given user of browser has 70 files in directory named "my_files"
    When user of browser uses spaces select to change data space to "space1"
    And user of browser sees that current working directory displayed in breadcrumbs is space1

    # create dir20
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir20" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir20" has appeared on files list
    And user of browser double clicks on directory named "dir20" of files list
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir20

    # create dir 10 in dir20
    And user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that "New directory" modal has appeared
    And user of browser clicks on input box in active modal
    And user of browser types "dir10" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that the modal has disappeared
    And user of browser sees that directory named "dir10" has appeared on files list
    And user of browser double clicks on directory named "dir10" of files list
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir20/dir10

    # start uploading files in dir10 and go back to dir 20
    And user of browser uses upload button in toolbar to upload files from local directory "my_files" to remote current dir
    And user of browser is idle for 0.02 seconds
    And user of browser changes current working directory to space1/dir20 using breadcrumbs
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir20
    And user of browser is idle for 5 seconds
    And user of browser sees that file browser contains 1 file(s)

    # go to dir 10 and see if every file has benn uploaded
    And user of browser double clicks on directory named "dir10" of files list
    And user of browser sees that current working directory displayed in breadcrumbs is space1/dir20/dir10
    Then user of browser sees that file browser contains 70 file(s)

    # TODO rm after integrating with swagger
    # in order to change cwd to root dir change space to other than change back
    And user of browser changes current working directory to space1 using breadcrumbs
    And user of browser sees that current working directory displayed in breadcrumbs is space1
    And user of browser selects "dir20" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser sees that "Remove files" modal has appeared
    And user of browser clicks "Yes" confirmation button in displayed modal
    And user of browser sees an info notify with text matching to: .*removed.*
    And user of browser sees that the modal has disappeared
    And user of browser does not see any directory named "dir20" on files list
