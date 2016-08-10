Feature: Onezone GUI elements
  A user interface for managing Onezone account

  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open [http://a.com, http://b.com] in [Firefox, Chrome]
    Given user opens a Onezone URL in a web browser
    # not used in non-homepage tests
#    And user clicks on the "login" link in Homepage main menu
    And user clicks on the "indigo" login button
    And user clicks on the "user1" link


  Scenario: User can change his alias using valid alias string
    When user expands the "user alias" Onezone sidebar panel
    And user clicks on the user alias
    And user types "helloworld" on keyboard
    And user presses enter on keyboard
    Then user should see, that the alias changed to "helloworld"
