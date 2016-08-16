Feature: Oneprovider Group functionality
  Various operations on groups


  # assuming there is user1
  # assuming there is p1
  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    # assuming there is user1
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    # assuming there is p1
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup
    And user clicks on the "groups" tab in main menu

  Scenario: User can add new group
    Given valid name string
    When user clicks on the "Create" button in current sidebar
    And user should see that "Create a new group" input box is active
    And user types given name on keyboard
    And user presses enter on keyboard
    Then user should see that the new group appeared on the list

  # assuming there is group1
  Scenario: User can invite other user
    Given there is "group1" on list in current sidebar
    When user clicks settings icon displayed for given element
    And user should see settings drop down menu for given element
    And user clicks on the "Invite user" item in current settings dropdown
    And user should see that "Invite user to the group" token box is active
    Then user should see non-empty token in active modal on Oneprovider page

  # assuming there is group1
  Scenario: User can invite group
    Given there is "group1" on list in current sidebar
    When user clicks settings icon displayed for given element
    And user should see settings drop down menu for given element
    And user clicks on the "Invite group" item in current settings dropdown
    And user should see that "Invite group to the group" token box is active
    Then user should see non-empty token in active modal on Oneprovider page

  # assuming there is group1
  Scenario: User can request space creation
    Given there is "group1" on list in current sidebar
    When user clicks settings icon displayed for given element
    And user should see settings drop down menu for given element
    And user clicks on the "Request space creation" item in current settings dropdown
    And user should see that "Request space creation for the group" token box is active
    Then user should see non-empty token in active modal on Oneprovider page

  # assuming there is group1
  Scenario: User can try to join space with incorrect token
    Given there is "group1" on list in current sidebar
    When user clicks settings icon displayed for given element
    And user should see settings drop down menu for given element
    And user clicks on the "Join space" item in current settings dropdown
    And user should see that "Join a space" input box is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*join.*group1.*space.*

  # assuming there is group1
  Scenario: User can try to join as subgroup with incorrect token
    Given there is "group1" on list in current sidebar
    When user clicks settings icon displayed for given element
    And user should see settings drop down menu for given element
    And user clicks on the "Join as subgroup" item in current settings dropdown
    And user should see that "Join a group to group" input box is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*join.*group1.*subgroup.*

  Scenario: Try join to group with incorrect token
    When user clicks on the "Join" button in current sidebar
    And user should see that "Join a group" input box is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*Failed.*join.*group.*

  # assuming there is group1
  Scenario: User can rename existing group and then rename it back
    Given there is "group1" on list in current sidebar
    When user clicks a settings icon displayed for "group1" list item
    And user should see a settings dropdown menu for "group1" list item
    And user clicks on the "rename" item in current settings dropdown
    And user should see that "Rename a group" input box is active
    And user types "NewNameGroup" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*group1.*renamed.*NewNameGroup.*
    Then user should see that the group "NewNameGroup" appears on the list
    And user should not see modal with title "Rename a group"
    And user clicks a settings icon displayed for "NewNameGroup" list item
    And user should see a settings dropdown menu for "NewNameGroup" list item
    And user clicks on the "rename" item in current settings dropdown
    And user should see that "Rename a group" input box is active
    And user types "group1" on keyboard
    And user presses enter on keyboard
    And user sees an info notify with text matching to: .*NewNameGroup.*renamed.*group1.*

  # assuming there is group1
  Scenario: User can leave existing group and then create group with the same name
    Given there is "group1" on list in current sidebar
    When user clicks a settings icon displayed for "group1" list item
    And user should see a settings dropdown menu for "group1" list item
    And user clicks on the "leave this group" item in current settings dropdown
    And user clicks "YES" confirmation button in displayed modal
    Then user sees an info notify with text matching to: .*group1.*left
    And user should not see modal with title "Leave the group"
    And user clicks on the "hroups" tab in main menu
    And user clicks on the "Create" button in sidebar panel
    And user should see that "Create a new group" input box is active
    And user types "group1" on keyboard
    And user presses enter on keyboard
    And user should see that the group "group1" appears on the list
