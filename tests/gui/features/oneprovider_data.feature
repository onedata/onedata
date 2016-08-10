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


  Scenario: Create new space with specified name
    When user clicks "Spaces" button
    And user clicks "Create" button
    And user should see, that name input box is active
    And user types "newSpace1" on keyboard
    And user presses enter on keyboard
    Then user should see new space "newSpace1"


  Scenario: Rename existing space and then rename it back
    Given existing space name
    When user clicks "Spaces" button
    And user moves cursor on the "space1"
    And user clicks "Settings" icon
    #And user sees settings dropdown panel
    And user clicks "RENAME" option
    And user should see, that rename input box is active
    #And user presses backspace on keyboard
    And user types "NewNameSpace" on keyboard
    And user presses enter on keyboard
    And user should see, that name was changed successfully
    Then user should see new space "NewNameSpace"
    And user cannot see rename input box
    And user moves cursor on the "NewNameSpace"
    And user clicks "Settings" icon
    #And user sees settings dropdown panel
    And user clicks "RENAME" option
    And user should see, that rename input box is active
    #And user presses backspace on keyboard
    And user types "space1" on keyboard
    And user presses enter on keyboard
    And user should see, that name was changed successfully


  Scenario: Fetch invite user token in spaces
    Given existing space name
    When user clicks "Spaces" button
    And user moves cursor on the "space1"
    And user clicks "Settings" icon
    And user clicks "INVITE USER" option
    And user should see, that invite user token box is active
    Then user should see invite user token
    And user can click "Copy" button


  Scenario: Fetch invite group token in spaces
    Given existing space name
    When user clicks "Spaces" button
    And user moves cursor on the "space1"
    And user clicks "Settings" icon
    And user clicks "INVITE GROUP" option
    And user should see, that invite group token box is active
    Then user should see invite group token
    And user can click "Copy" button


  Scenario: Fetch space creation token in spaces
    Given existing space name
    When user clicks "Spaces" button
    And user moves cursor on the "space1"
    And user clicks "Settings" icon
    And user clicks "GET SUPPORT" option
    And user should see, that space creation token box is active
    Then user should see space creation token
    And user can click "Copy" button

  Scenario: Trying join to space with invalid token
    Given existing space name
    When user clicks "Spaces" button
    And user clicks "Join" button
    And user should see, that token input box is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user should see error message


#  Scenario: Switching between spaces
#    When user clicks "Spaces" button
    #And user sees current url
#    And user moves cursor on the "space2"
#    And user clicks "space2" button
    #Then user should see new url
#    Then user should see space menu for "space2"

  Scenario: Create new file
    When user clicks "Create file" button
    And user should see, that new file name input box is active
    And user types "file1" on keyboard
    And user presses enter on keyboard
    Then user should see "file1" file

