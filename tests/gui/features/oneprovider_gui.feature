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
    And user should see, that input box for space name is active
    And user types "newSpace1" on keyboard
    And user presses enter on keyboard
    Then user should see new space named "newSpace1"


  Scenario: Rename existing space and then rename it back
    #I assumed here that we already have existing space with name "space1"
    Given existing "space1"
    When user clicks "Spaces" button
    And click "Settings" icon in "space1"
    And user clicks "RENAME" option
    And user should see, that input box for new name is active
    And user types "NewNameSpace" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    Then user should see space named "NewNameSpace"
    And user should not see input box for new name
    And click "Settings" icon in "NewNameSpace"
    And user clicks "RENAME" option
    And user should see, that input box for new name is active
    And user types "space1" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*


  Scenario: Fetch invite user token in spaces
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1
    When user clicks "Spaces" button
    And click "Settings" icon in "space1"
    And user clicks "INVITE USER" option
    And user should see, that invite user token box is active
    Then user should see invite user token
    #And user can click "Copy" button


  Scenario: Fetch invite group token in spaces
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button
    And click "Settings" icon in "space1"
    And user clicks "INVITE GROUP" option
    And user should see, that invite group token box is active
    Then user should see invite group token
    #And user can click "Copy" button


  Scenario: Fetch get support token in spaces
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button
    And click "Settings" icon in "space1"
    And user clicks "GET SUPPORT" option
    And user should see, that get support token box is active
    Then user should see get support token
    #And user can click "Copy" button


  Scenario: Trying join to space with invalid token
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button
    And user clicks "Join" button
    And user should see, that token input box is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*Invalid.*token.*


    #I assumed here that we already have existing space with name "space2"
    #and that starting url is not adress to "space2"
  Scenario: Switching between spaces
    When user clicks "Spaces" button
    And click "Settings" icon in "space1"
    And user sees current url
    And user clicks "space2"
    Then user should see space menu for "space2"
    And user should see new url


  Scenario: Set space as home
    #I assumed here that we already have existing space named "space1"
    Given existing "space1"
    When user clicks "Spaces" button
    And click "Settings" icon in "space1"
    And user clicks "SET AS HOME" option
    Then user sees an info notify with text matching to: .*space1.*home.*
    And user should see home space icon next to "space1"


  Scenario: Leave existing space
    #I assumed here that we already have existing space named "space1"
    Given existing "space1"
    When user clicks "Spaces" button
    And click "Settings" icon in "space4"
    And user clicks "LEAVE SPACE" option
    And user clicks "YES" button
    Then user sees an info notify with text matching to: .*space4.*left
    And user refreshes site
    And user should not see space named "space4"
