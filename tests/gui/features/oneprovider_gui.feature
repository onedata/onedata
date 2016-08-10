  Feature: Oneprovider GUI elements
    A user interface for managing account

  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup


  Scenario: Create new space with specified name
    When user clicks "Spaces" button
    And user clicks "Create" button
    And user should see, that name input box is active
    And user types "newSpace1" on keyboard
    And user presses enter on keyboard
    Then user should see new space "newSpace1"


  Scenario: Rename existing space and then rename it back
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button
    And user moves cursor on the "space1"
    And user clicks "Settings" icon
    And user clicks "RENAME" option
    And user should see, that rename input box is active
    And user types "NewNameSpace" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    Then user should see new space "NewNameSpace"
    And user cannot see rename input box
    And user moves cursor on the "NewNameSpace"
    And user clicks "Settings" icon
    And user clicks "RENAME" option
    And user should see, that rename input box is active
    And user types "space1" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*


  Scenario: Fetch invite user token in spaces
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1
    When user clicks "Spaces" button
    And user moves cursor on the "space1"
    And user clicks "Settings" icon
    And user clicks "INVITE USER" option
    And user should see, that invite user token box is active
    Then user should see invite user token
    And user can click "Copy" button


  Scenario: Fetch invite group token in spaces
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button
    And user moves cursor on the "space1"
    And user clicks "Settings" icon
    And user clicks "INVITE GROUP" option
    And user should see, that invite group token box is active
    Then user should see invite group token
    And user can click "Copy" button


  Scenario: Fetch space creation token in spaces
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1
    When user clicks "Spaces" button
    And user moves cursor on the "space1"
    And user clicks "Settings" icon
    And user clicks "GET SUPPORT" option
    And user should see, that space creation token box is active
    Then user should see space creation token
    And user can click "Copy" button

  Scenario: Trying join to space with invalid token
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button
    And user clicks "Join" button
    And user should see, that token input box is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*Invalid.*token.*
