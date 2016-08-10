Feature: Oneprovider Group functionality
  Various operations on groups

  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link
    And user expands the "go to your files" Onezone sidebar panel
    And user clicks on the "p1" provider in Onezone providers sidebar panel
    And user clicks on the "Go to your files" button in provider popup


#  Scenario: Add new group
#    Given user has name for new group
#    When user clicks on the "groups" provider in Oneprovider providers sidebar panel
#    And user clicks on the "Create" button
#    And user should see that name input box is active
#    And user types group name on keyboard
#    And user presses enter on keyboard
#    Then user should see, that the new group appear on the list


  Scenario: Rename group
    Given user has new name for group
    When user clicks on the "groups" provider in Oneprovider providers sidebar panel
    And user clicks on the settings button for "group1"
    And user clicks on the "RENAME" element
    And user should see that rename input box is active
    And user types new group name on keyboard
    And user presses enter on keyboard
    # Then user should see popup with information about name change
    Then user should see, that the new name replaced old one on the list
