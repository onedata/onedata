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
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user clicks "Create" button from spaces menu bar
    And user should see, that input box for space name is active
    And user types "spaceNew1" on keyboard
    And user presses enter on keyboard
    Then user should see new space named "spaceNew1" in spaces list


  Scenario: Rename existing space and then rename it back
    #I assumed here that we already have existing space with name "space1"
    Given existing "space1"
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user clicks "Settings" icon displayed on space named "space1"
    And user should see settings drop down menu for spaces
    And user clicks "RENAME" option from drop down menu for spaces
    And user should see, that input box for new name is active
    And user types "NewNameSpace" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*space1.*renamed.*NewNameSpace.*
    Then user should see space named "NewNameSpace" in spaces list
    And user should not see input box for new name
    And user clicks "Settings" icon displayed on space named "NewNameSpace"
    And user should see settings drop down menu for spaces
    And user clicks "RENAME" option from drop down menu for spaces
    And user should see, that input box for new name is active
    And user types "space1" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*NewNameSpace.*renamed.*space1.*


  Scenario: Check if "invite user" token box is not empty
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user clicks "Settings" icon displayed on space named "space1"
    And user should see settings drop down menu for spaces
    And user clicks "INVITE USER" option from drop down menu for spaces
    And user should see, that invite user token box is active
    Then user should see, that "invite user" token box is not empty


  Scenario: Check if "invite group" token box is not empty
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user clicks "Settings" icon displayed on space named "space1"
    And user should see settings drop down menu for spaces
    And user clicks "INVITE GROUP" option from drop down menu for spaces
    And user should see, that invite group token box is active
    Then user should see, that "invite group" token box is not empty


  Scenario: Check if "get support" token box is not empty
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user clicks "Settings" icon displayed on space named "space1"
    And user should see settings drop down menu for spaces
    And user clicks "GET SUPPORT" option from drop down menu for spaces
    And user should see, that get support token box is active
    Then user should see, that "get support" token box is not empty


  Scenario: Trying join to space with invalid token
    #I assumed here that we already have existing space with name 'space1'
    Given existing "space1"
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user clicks "Join" button from spaces menu
    And user should see, that token input box is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*Invalid.*token.*


    #I assumed here that we already have existing space with name "space2"
    #and that starting url is not adress to "space2"
  Scenario: Switching between spaces
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user can see current url
    And user clicks space named "space2" from spaces list
    Then user should see submenu for space named "space2"
    And user should see that url has changed


  Scenario: Set given space as home and than set previous space as home
    #I assumed here that we already have existing space named "space1" and
    # space named "space2"
    Given existing "space1"
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user clicks "Settings" icon displayed on space named "space2"
    And user should see settings drop down menu for spaces
    And user clicks "SET AS HOME" option from drop down menu for spaces
    Then user sees an info notify with text matching to: .*space2.*home.*
    And user should see home space icon next to "space2"
    And user clicks "Settings" icon displayed on space named "space1"
    And user should see settings drop down menu for spaces
    And user clicks "SET AS HOME" option from drop down menu for spaces
    And user sees an info notify with text matching to: .*space1.*home.*
    And user should see home space icon next to "space1"


  Scenario: Leave existing space and then create space with the same name
    #I assumed here that we already have existing space named "space1"
    Given existing "space1"
    When user clicks "Spaces" button from sidebar panel
    And user should see, that main content has been reloaded
    And user clicks "Settings" icon displayed on space named "space2"
    And user should see settings drop down menu for spaces
    And user clicks "LEAVE SPACE" option from drop down menu for spaces
    And user clicks "YES" button in popup window asking if he is sure
    Then user sees an info notify with text matching to: .*space2.*left
    And user should not see popup window
    And user clicks "Create" button from spaces menu bar
    And user should see, that input box for space name is active
    And user types "space2" on keyboard
    And user presses enter on keyboard
    And user should see new space named "space2" in spaces list
   # And user refreshes site
   # And user should not see space named "space2" in spaces list



