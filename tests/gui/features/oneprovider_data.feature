Feature: Oneprovider Data view
  Various operations on Data view

  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open a Onezone URL in [Firefox, Chrome]
    Given user opens a Onezone URL in a web browser
    #not used in non-homepage tests
    #And user clicks on the "login" link in Homepage main menu
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup
    #data is default, so it is unnecessary
    #And user clicks on the "data" menu item in Oneprovider main menu


  Scenario: Uploading a small file to space that accepts large files should succeed
    When user uses spaces select to change data space to "space1"
    And user uses upload button in toolbar to upload file "20B-0.txt" to current dir
    Then user sees an info notify with text matching to: .*20B-0\.txt.*uploaded successfully.*


  Scenario: Create new file
    When user clicks "Create file" button
    And user should see, that input box for file name is active
    And user types "file1" on keyboard
    And user presses enter on keyboard
    Then user should see new file named "file1"


  Scenario: Create new directory
    When user clicks "Create directory" button
    And user should see, that input box for directory name is active
    And user types "directory1" on keyboard
    And user presses enter on keyboard
    Then user should see new directory named "directory1"


   #In this test i assumed that file with name "file" already exists
  Scenario: Remove existing file
    When user selects "file"
    And user clicks "Remove element" button
    And user clicks "OK" button
    Then user sees an success notify with text matching to: .*removed.*
    And user should not see "file"


