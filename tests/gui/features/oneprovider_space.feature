Feature: Oneprovider space functionality
  Various operations on spaces

  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup
    And user clicks on the "spaces" tab in main menu sidebar
    And user sees that main content reloaded


    Scenario: Create new space with specified name
    Given valid name string
    When user clicks on the "Create" button in spaces sidebar
    And user sees that input box in "Create a new space" modal is active
    And user types given name on keyboard
    And user presses enter on keyboard
    Then user sees that the new item has appeared on the spaces list


  # assuming there is space1
  Scenario: Rename existing space and then rename it back
    Given there is a "space1" item on the spaces list
    When user clicks a settings icon displayed for "space1" item on the spaces list
    And user sees a settings dropdown menu for "space1" item on the spaces list
    And user clicks on the "RENAME" item in current settings dropdown
    And user sees that input box in "Rename a space" modal is active
    And user types "NewNameSpace" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    Then user sees that the "NewNameSpace" has appeared on the spaces list
    And user should not see modal with title "Rename a space"
    And user clicks a settings icon displayed for "NewNameSpace" item on the spaces list
    And user sees a settings dropdown menu for "NewNameSpace" item on the spaces list
    And user clicks on the "RENAME" item in current settings dropdown
    And user sees that input box in "Rename a space" modal is active
    And user types "space1" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*


  # assuming there is space1
  Scenario: Check if "invite user" token box is not empty
    Given there is a "space1" item on the spaces list
    When user clicks a settings icon displayed for "space1" item on the spaces list
    And user sees a settings dropdown menu for "space1" item on the spaces list
    And user clicks on the "INVITE USER" item in current settings dropdown
    And user sees that token box in "Invite user to the space" modal is active
    Then user sees non-empty token in active modal


  # assuming there is space1
  Scenario: Check if "invite group" token box is not empty
    Given there is a "space1" item on the spaces list
    When user clicks a settings icon displayed for "space1" item on the spaces list
    And user sees a settings dropdown menu for "space1" item on the spaces list
    And user clicks on the "INVITE GROUP" item in current settings dropdown
    And user sees that token box in "Invite group to the space" modal is active
    Then user sees non-empty token in active modal


  # assuming there is space1
  Scenario: Check if "get support" token box is not empty
    Given there is a "space1" item on the spaces list
    When user clicks a settings icon displayed for "space1" item on the spaces list
    And user sees a settings dropdown menu for "space1" item on the spaces list
    And user clicks on the "GET SUPPORT" item in current settings dropdown
    And user sees that token box in "Get support for the space" modal is active
    Then user sees non-empty token in active modal


  # assuming there is space1
  Scenario: Try join to space with invalid token
    Given there is a "space1" item on the spaces list
    When user clicks on the "Join" button in spaces sidebar
    And user sees that input box in "Join a space" modal is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*invalid.*token.*


  # assuming there is space2 and starting url is not address to space2
  Scenario: Switching between spaces
    When user can see current url
    And user clicks space named "space2" from spaces list
    Then user sees submenu for space named "space2"
    And user sees that url has changed


  # assuming there is space1 and space2
  Scenario: Set given space as home and than set previous space as home
    Given there is a "space2" item on the spaces list
    When user clicks a settings icon displayed for "space2" item on the spaces list
    And user sees a settings dropdown menu for "space2" item on the spaces list
    And user clicks on the "SET AS HOME" item in current settings dropdown
    Then user sees an info notify with text matching to: .*space2.*home.*
    And user sees home space icon next to displayed name of space "space2" in spaces list
    And user clicks a settings icon displayed for "space1" item on the spaces list
    And user sees a settings dropdown menu for "space1" item on the spaces list
    And user clicks on the "SET AS HOME" item in current settings dropdown
    And user sees an info notify with text matching to: .*space1.*home.*
    And user sees home space icon next to displayed name of space "space1" in spaces list


  # assuming there is space2
  Scenario: Leave existing space and then create space with the same name
    Given there is a "space2" item on the spaces list
    When user clicks a settings icon displayed for "space2" item on the spaces list
    And user sees a settings dropdown menu for "space2" item on the spaces list
    And user clicks on the "LEAVE SPACE" item in current settings dropdown
    And user clicks "YES" confirmation button in displayed modal
    Then user sees an info notify with text matching to: .*space2.*left
    And user should not see modal with title "Leave a space"
    And user refreshes site
    And user clicks on the "Create" button in spaces sidebar
    And user sees that input box in "Create a new space" modal is active
    And user types "space2" on keyboard
    And user presses enter on keyboard
    And user sees that the "space2" has appeared on the spaces list
