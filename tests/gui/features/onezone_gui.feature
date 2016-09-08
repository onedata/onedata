Feature: Onezone GUI elements
  A user interface for managing Onezone account


  Background:
    # in future: Given [u1, u2] open a Onezone URL in their web browsers
    # in future: Given [u1, u2] open [http://a.com, http://b.com] in [Firefox, Chrome]
    Given user opened browser window for browser
    And user of browser opens a Onezone URL
    # not used in non-homepage tests
    And user of browser clicks on the "plgrid" login button
    And user of browser clicks on the "user1" link


  Scenario: User can change his alias using valid alias string
    When user of browser expands the "user alias" Onezone sidebar panel
    And user of browser clicks on the user alias
    And user of browser types "helloworld" on keyboard
    And user of browser presses enter on keyboard
    Then user of browser should see that the alias changed to "helloworld"

  Scenario: User can create space using valid name string
    Given user of browser generates valid name string
    When user of browser expands the "data space management" Onezone sidebar panel
    And user of browser clicks on the "Create new space" in "Data space management" sidebar panel
    And user of browser clicks on new space name input box
    And user of browser types given name on keyboard
    And user of browser presses enter on keyboard
    Then user of browser should see that the new space has appeared on the spaces list in "Data space management" sidebar panel
