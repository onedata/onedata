Feature: Oneprovider Group functionality
  Various operations on groups

  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    # assuming there is user1
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    # assuming there is p1
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup
    And user clicks on the "groups" Oneprovider's sidebar panel

  Scenario: Add new group
    Given user has name for new group
    When user clicks on the "Create" button in current sidebar
    And user should see that name input box is active
    And user types group name on keyboard
    And user presses enter on keyboard
    Then user should see, that the new group appear on the list

  Scenario: Invite user
    # assuming there is group1
    When user clicks on the settings button for "group1" in current sidebar
    And user clicks on the "INVITE USER" button in current settings dropdown
    And user should see that "Invite user to the group" token box on Oneprovider page is active
    Then user should see non-empty token in active window on Oneprovider page

  Scenario: Invite group
    # assuming there is group1
    When user clicks on the settings button for "group1" in current sidebar
    And user clicks on the "INVITE GROUP" button in current settings dropdown
    And user should see that "Invite group to the group" token box on Oneprovider page is active
    Then user should see non-empty token in active window on Oneprovider page

  Scenario: Request space creation
    # assuming there is group1
    When user clicks on the settings button for "group1" in current sidebar
    And user clicks on the "REQUEST SPACE CREATION" button in current settings dropdown
    And user should see that "Request space creation for the group" token box on Oneprovider page is active
    Then user should see non-empty token in active window on Oneprovider page

  Scenario: Try to join space with incorrect token
    # assuming there is group1
    When user clicks on the settings button for "group1" in current sidebar
    And user clicks on the "JOIN SPACE" button in current settings dropdown
    And user should see that "Join a space" input box on Oneprovider page is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*join.*group1.*space.*

  Scenario: Try to join as subgroup with incorrect token
    # assuming there is group1
    When user clicks on the settings button for "group1" in current sidebar
    And user clicks on the "JOIN AS SUBGROUP" button in current settings dropdown
    And user should see that "Join a group to group" input box on Oneprovider page is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*join.*group1.*subgroup.*

  Scenario: Try join to group with incorrect token
    When user clicks on the "Join" button in current sidebar
    And user should see that "Join a group" input box on Oneprovider page is active
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user sees an error notify with text matching to: .*Failed.*join.*group.*


#  Scenario: Rename group
#    Given user has new name for group
#    When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#    # group1 is defined in json
#    And user clicks on the settings button for "group1"
#    And user clicks on the "RENAME" in current settings dropdown
#    And user should see that rename input box is active
#    And user types new group name on keyboard
#    And user presses enter on keyboard
#    Then user should see popup with information about name change
#    And user should see, that the new name replaced old one on the list
