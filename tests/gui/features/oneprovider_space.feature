Feature: Oneprovider space functionality
  Various operations on spaces

  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup
    And user clicks on the "spaces" tab in main menu
    #And user should see that main content has been reloaded


    Scenario: Create new space with specified name
    Given valid name string
    When user clicks on the "Create" button in current sidebar
    And user should see that "Create a new space" input box on Oneprovider page is active
    And user types the space name on keyboard
    And user presses enter on keyboard
    Then user should see that the new space appears on the list


  # I assumed here that we already have existing space with name "space1"
  Scenario: Rename existing space and then rename it back
    Given existing "space1"
    When user clicks settings icon displayed on "space1" in current sidebar
    And user should see settings drop down menu for spaces
    And user clicks on the "RENAME" button in current settings dropdown
    And user should see that "Rename a space" input box on Oneprovider page is active
    And user types "NewNameSpace" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    Then user should see that the "NewNameSpace" appears on the list
    And user should not see modal with title "Rename a space"
    And user clicks settings icon displayed on "NewNameSpace" in current sidebar
    And user should see settings drop down menu for spaces
    And user clicks on the "RENAME" button in current settings dropdown
    And user should see that "Rename a space" input box on Oneprovider page is active
    And user types "space1" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*


  # I assumed here that we already have existing space with name 'space1'
  Scenario: Check if "invite user" token box is not empty
    Given existing "space1
    When user clicks settings icon displayed on "space1" in current sidebar
    And user should see settings drop down menu for spaces
    And user clicks on the "INVITE USER" button in current settings dropdown
    And user should see that "Invite user to the space" token box on Oneprovider page is active
    Then user should see non-empty token in active window on Oneprovider page


  # I assumed here that we already have existing space with name 'space1'
  Scenario: Check if "invite group" token box is not empty
    Given existing "space1"
    When user clicks settings icon displayed on "space1" in current sidebar
    And user should see settings drop down menu for spaces
    And user clicks on the "INVITE GROUP" button in current settings dropdown
    And user should see that "Invite group to the space" token box on Oneprovider page is active
    Then user should see non-empty token in active window on Oneprovider page


  # I assumed here that we already have existing space with name 'space1'
  Scenario: Check if "get support" token box is not empty
    Given existing "space1"
    When user clicks settings icon displayed on "space1" in current sidebar
    And user should see settings drop down menu for spaces
    And user clicks on the "GET SUPPORT" button in current settings dropdown
    And user should see that "Get support for the space" token box on Oneprovider page is active
    Then user should see non-empty token in active window on Oneprovider page


  # I assumed here that we already have existing space with name 'space1'
  Scenario: Try join to space with invalid token
    Given existing "space1"
    When user clicks on the "Join" button in current sidebar
    And user should see that "Join a space" token box on Oneprovider page is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*Invalid.*token.*


  # I assumed here that we already have existing space with name "space2"
  # and that starting url is not adress to "space2"
  Scenario: Switching between spaces
    When user can see current url
    And user clicks space named "space2" from spaces list
    Then user should see submenu for space named "space2"
    And user should see that url has changed


  # I assumed here that we already have existing space named "space1" and
  # space named "space2"
  Scenario: Set given space as home and than set previous space as home
    Given existing "space1"
    When user clicks settings icon displayed on "space2" in current sidebar
    And user should see settings drop down menu for spaces
    And user clicks on the "SET AS HOME" button in current settings dropdown
    Then user sees an info notify with text matching to: .*space2.*home.*
    And user should see home space icon next to "space2"
    And user clicks settings icon displayed on "space1" in current sidebar
    And user should see settings drop down menu for spaces
    And user clicks on the "SET AS HOME" button in current settings dropdown
    And user sees an info notify with text matching to: .*space1.*home.*
    And user should see home space icon next to "space1"


  # I assumed here that we already have existing space named "space1"
  Scenario: Leave existing space and then create space with the same name
    Given existing "space1"
    When user clicks settings icon displayed on "space2" in current sidebar
    And user should see settings drop down menu for spaces
    And user clicks on the "LEAVE SPACE" button in current settings dropdown
    And user clicks "YES" confirmation button in displayed modal
    Then user sees an info notify with text matching to: .*space2.*left
    And user should not see modal with title "Leave a space"
    And user clicks on the "spaces" tab in main menu
    And user clicks on the "Create" button in current sidebar
    And user should see that "Create a new space" input box on Oneprovider page is active
    And user types "space2" on keyboard
    And user presses enter on keyboard
    And user should see that the "space2" appears on the list
