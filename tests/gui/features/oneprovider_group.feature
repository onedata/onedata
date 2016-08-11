Feature: Oneprovider Group functionality
  Various operations on groups

  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup
    And user clicks on the "groups" Oneprovider's sidebar panel

  Scenario: Add new group
    Given user has name for new group
    When user clicks on the "Create" button
    And user should see that name input box is active
    And user types group name on keyboard
    And user presses enter on keyboard
    Then user should see, that the new group appear on the list


#  Scenario: Rename group
#    Given user has new name for group
#    When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#    # group1 is defined in json
#    And user clicks on the settings button for "group1"
#    And user clicks on the "RENAME" element
#    And user should see that rename input box is active
#    And user types new group name on keyboard
#    And user presses enter on keyboard
#    Then user should see popup with information about name change
#    And user should see, that the new name replaced old one on the list
#
#    Scenario: Invite user
#      When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#      And user clicks on the settings button for "group1"
#      And user clicks on the "INVITE USER" element
#      And user should see that invite input box is active
#      Then user can copy visible invite-user token
#
#    Scenario: Invite group
#      When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#      And user clicks on the settings button for "group1"
#      And user clicks on the "INVITE GROUP" element
#      And user should see that invite input box is active
#      Then user can copy visible invite-group token
#
#    Scenario: Request space creation
#      When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#      And user clicks on the settings button for "group1"
#      And user clicks on the "REQUEST SPACE CREATION" element
#      And user should see that request input box is active
#      Then user can copy visible request token
#
#    Scenario: Try to join space with incorrect token
#      When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#      And user clicks on the settings button for "group1"
#      And user clicks on the "JOIN SPACE" element
#      And user should see that join space input box is active
#      And user types "helloworld" on keyboard
#      And user presses enter on keyboard
#      Then user sees an error notify with text matching to: .*join.*group1.*space.*
#
#    Scenario: Try to join group with incorrect token
#      When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#      And user clicks on the settings button for "group1"
#      And user clicks on the "JOIN AS SUBGROUP" element
#      And user should see that join as subgroup input box is active
#      And user types "helloworld" on keyboard
#      And user presses enter on keyboard
#      Then user sees an error notify with text matching to: .*join.*group1.*subgroup.*
#
#    Scenario: Try join to group with incorrect token
#      When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#      And user clicks on the "Join" button
#      And user should see that join group input box is active
#      And user types "helloworld" on keyboard
#      And user presses enter on keyboard
#      Then user sees an error notify with text matching to: .*Failed.*join.*group.*
