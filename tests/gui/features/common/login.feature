Feature: Space utilities using onepanel

  Background:
    Given user opened browser window
    And user of browser opened z1 zone panel page


  Scenario: User successfully login to Zone panel
    Given user of browser entered credentials for admin in login form
    And users of browser pressed Sign in button
    Then user of browser sees that he successfully logged in zone panel


  Scenario: User fails to login because of invalid credentials
    When user of browser types "admin" to Username input in login form
    And user of browser types "as" to Password input in login form
    And user of browser presses Sign in button
    Then user of browser sees error message about invalid credentials


  Scenario: User successfully logout
    Given user of browser entered credentials for admin in login form
    And users of browser pressed Sign in button
    When user of browser clicks on user account button in main menu
    And user of browser clicks on Logout button in user account popover
    Then user of browser sees that he was logged out
