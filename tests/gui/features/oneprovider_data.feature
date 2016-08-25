Feature: Oneprovider Data view
  Various operations on Data view

  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open a Onezone URL in [Firefox, Chrome]
    Given user opens a Onezone URL in a web browser
    # not used in non-homepage tests
    # And user clicks on the "login" link in Homepage main menu
    And user clicks on the "plgrid" login button
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup
    And user sees that main content has ended loading
    # data is default, so it is unnecessary
    # And user clicks on the "data" menu item in Oneprovider main menu


  Scenario: Uploading a small file to space that accepts large files should succeed
    When user uses spaces select to change data space to "space1"
    And user uses upload button in toolbar to upload file "20B-0.txt" to current dir
    Then user sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*


  Scenario: Create new file and then remove it
    Given there is no file named "file1" in files list
    When user clicks the button from top menu bar with tooltip "Create file"
    And user sees that input box in "New file" modal is active
    And user types "file1" on keyboard
    And user presses enter on keyboard
    Then user sees that "New file" modal has vanished
    And user sees new file named "file1" in files list
    And user selects "file1" from files list
    And user clicks the button from top menu bar with tooltip "Remove element"
    And user clicks "OK" confirmation button in displayed modal
    And user sees an success notify with text matching to: .*removed.*
    And user should not see file named "file1" in files list


  Scenario: Create new directory and then remove it
    Given there is no directory named "directory1" in files list
    When user clicks the button from top menu bar with tooltip "Create directory"
    And user sees that input box in "New directory" modal is active
    And user types "directory1" on keyboard
    And user presses enter on keyboard
    Then user sees that "New directory" modal has vanished
    And user sees new directory named "directory1" in files list
    And user selects "directory1" from files list
    And user clicks the button from top menu bar with tooltip "Remove element"
    And user clicks "OK" confirmation button in displayed modal
    And user sees an success notify with text matching to: .*removed.*
    And user should not see file named "directory1" in files list


  Scenario: Create file and then remove it
    When user clicks the button from top menu bar with tooltip "Create file"
    And user sees that input box in "New file" modal is active
    And user types "file2" on keyboard
    And user presses enter on keyboard
    And user sees that "New file" modal has vanished
    And user sees new file named "file2" in files list
    And user selects "file2" from files list
    And user clicks the button from top menu bar with tooltip "Remove element"
    And user clicks "OK" confirmation button in displayed modal
    Then user sees an success notify with text matching to: .*removed.*
    And user should not see file named "file2" in files list


  # 'space1' supported by 'p1' defined in env.json
  Scenario: Create file and check if provider name is displayed in the file distribution panel
    When user clicks the button from top menu bar with tooltip "Create file"
    And user sees that input box in "New file" modal is active
    And user types "file3" on keyboard
    And user presses enter on keyboard
    And user sees that "New file" modal has vanished
    And user sees new file named "file3" in files list
    And user selects "file3" from files list
    And user clicks the button from top menu bar with tooltip "Show file distribution"
    Then user sees modal with name of provider supporting space in providers column


