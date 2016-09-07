Feature: Oneprovider Data view
  Various operations on Data view

  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open a Onezone URL in [Firefox, Chrome]
    Given user of browser opens a Onezone URL
    # not used in non-homepage tests
    # And user clicks on the "login" link in Homepage main menu
    And user of browser clicks on the "plgrid" login button
    And user of browser clicks on the "user1" link
    And user of browser expands the "go to your files" Onezone sidebar panel
    And user of browser clicks on the "p1" provider in Onezone providers sidebar panel
    And user of browser clicks on the "Go to your files" button in provider popup
    And user of browser sees that main content has ended loading
    # data is default, so it is unnecessary
    # And user clicks on the "data" menu item in Oneprovider main menu

  Scenario: User downloads file
    When user of browser uses spaces select to change data space to "space1"
    And user of browser uses upload button in toolbar to upload file "20B-1.txt" to current dir
    And user of browser double clicks on file "20B-1.txt" from files list
    Then user of browser sees that downloaded file "20B-1.txt" contains "11111111111111111111"


  Scenario: Uploading a small file to space that accepts large files should succeed
    When user of browser uses spaces select to change data space to "space1"
    And user of browser uses upload button in toolbar to upload file "20B-0.txt" to current dir
    Then user of browser sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*


  Scenario: Create new file and then remove it
    Given that in browser there is no file named "file1" in files list
    When user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file1" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees that "New file" modal has disappeared
    And user of browser sees new file named "file1" in files list
    And user of browser selects "file1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser should not see file named "file1" in files list


  Scenario: Create new directory and then remove it
    Given that in browser there is no directory named "directory1" in files list
    When user of browser clicks the button from top menu bar with tooltip "Create directory"
    And user of browser sees that input box in "New directory" modal is active
    And user of browser types "directory1" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser sees that "New directory" modal has disappeared
    And user of browser sees new directory named "directory1" in files list
    And user of browser selects "directory1" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    And user of browser sees an success notify with text matching to: .*removed.*
    And user of browser should not see file named "directory1" in files list


  Scenario: Create file and then remove it
    When user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file2" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees new file named "file2" in files list
    And user of browser selects "file2" from files list
    And user of browser clicks the button from top menu bar with tooltip "Remove element"
    And user of browser clicks "OK" confirmation button in displayed modal
    Then user of browser sees an success notify with text matching to: .*removed.*
    And user of browser should not see file named "file2" in files list


  # 'space1' supported by 'p1' defined in env.json
  Scenario: Create file and check if provider name is displayed in the file distribution panel
    When user of browser clicks the button from top menu bar with tooltip "Create file"
    And user of browser sees that input box in "New file" modal is active
    And user of browser types "file3" on keyboard
    And user of browser presses enter on keyboard
    And user of browser sees that "New file" modal has disappeared
    And user of browser sees new file named "file3" in files list
    And user of browser selects "file3" from files list
    And user of browser clicks the button from top menu bar with tooltip "Show file distribution"
    Then user of browser sees modal with name of provider supporting space in providers column
