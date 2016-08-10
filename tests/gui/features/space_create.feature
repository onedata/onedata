Feature: Create space

  Background:
    Given user opens a Onezone URL in a web browser
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link

  Scenario: create space with specified name
    When user expands the "data space management" Onezone sidebar panel
    And user clicks on the Create new space button
    And user types "cba" on keyboard
    And user presses enter on keyboard
    Then user should see new space with "cba" name