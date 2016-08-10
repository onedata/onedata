Feature: Onezone GUI elements
  A user interface for managing Onezone account

  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link


  Scenario: User can add new space
    Given user has name for new space
    When user expands the "data space management" Onezone sidebar panel
    And user clicks on the "Create new space"
    And user types space name on keyboard
    And user presses enter on keyboard
    Then user should see, that the new space appear on the list
