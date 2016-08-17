Feature: Oneprovider Data view
  Various operations on Data view

  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open a Onezone URL in [Firefox, Chrome]
    Given user opens a Onezone URL in a web browser
    # not used in non-homepage tests
    # And user clicks on the "login" link in Homepage main menu
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup
    # data is default, so it is unnecessary
    # And user clicks on the "data" menu item in Oneprovider main menu


  Scenario: Uploading a small file to space that accepts large files should succeed
    When user uses spaces select to change data space to "space1"
    And user uses upload button in toolbar to upload file "20B-0.txt" to current dir
    Then user sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*


  # assuming there is not file1
  Scenario: Create new file
    When user clicks the button from top menu bar with tooltip "Create file"
    And user should see that "New file" input box is active
    And user types "file1" on keyboard
    And user presses enter on keyboard
    Then user should not see modal with title "New file"
    And user should see new file named "file1" in files list


  # assuming there is not directory1
  Scenario: Create new directory
    When user clicks the button from top menu bar with tooltip "Create directory"
    And user should see that "New directory" input box is active
    And user types "directory1" on keyboard
    And user presses enter on keyboard
    Then user should not see modal with title "New directory"
    And user should see new directory named "directory1" in files list


  # assuming there is file2
  Scenario: Remove existing file and then create file with the same name
    Given there is a "file2" file on the files list
    When user selects "file2" from files list
    And user clicks the button from top menu bar with tooltip "Remove element"
    And user clicks "OK" confirmation button in displayed modal
    Then user sees an success notify with text matching to: .*removed.*
    And user should not see file named "file2" in files list
    And user clicks the button from top menu bar with tooltip "Create file"
    And user should see that "New file" input box is active
    And user types "file2" on keyboard
    And user presses enter on keyboard
    And user should not see modal with title "New file"
    And user should see new file named "file2" in files list


  # assuming there is file1 in space1 supported by p1
  Scenario: Check if provider name is displayed in the file distribution panel
    Given there is provider "p1" supporting space named "space1"
    And there is a "file1" file on the files list
    When user selects "file1" from files list
    And user clicks the button from top menu bar with tooltip "Show file distribution"
    Then user should see modal with provider's name "p1" in providers column


